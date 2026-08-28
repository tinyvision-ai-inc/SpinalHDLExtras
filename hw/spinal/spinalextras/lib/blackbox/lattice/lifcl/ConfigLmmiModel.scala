package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._
import spinalextras.lib.bus.LMMI

import scala.language.postfixOps

/**
 * Datasheet behavioral CONFIG_LMMI (TN-02099). Not the encrypted CONFIG_LMMIA.
 * LMMI host on LMMICLK; ADDR_CFG=0x01 command stream while REQUEST is held.
 * Elaborate under the LMMICLK ClockDomain (pins are for the datasheet map).
 */
class ConfigLmmiModel(stallCycles: Int = 3) extends Component {
  require(stallCycles >= 1, "stallCycles >= 1 so READY backpressure is visible")

  val io = new Bundle {
    val LMMICLK = in Bool()
    val LMMIREQUEST = in Bool()
    val LMMIWRRD_N = in Bool()
    val LMMIOFFSET = in UInt (8 bits)
    val LMMIWDATA = in Bits (8 bits)
    val LMMIRDATA = out Bits (8 bits)
    val LMMIREADY = out Bool()
    val LMMIRDATAVALID = out Bool()
    val LMMIRESETN = in Bool()
    val RSTSMCLK = in Bool() default False
    val SMCLK = in Bool() default False

    val capturedCount = out UInt (5 bits)
    val lastBytes = out Vec(Bits(8 bits), ConfigLmmiModel.capBytes)
    val protocolError = out Bool()
    val errorOffset = out Bool()
    val errorDrop = out Bool()
    val errorOpcode = out Bool()
    val sawBackpressure = out Bool()
    /** Writer/mailbox reset: abort in-flight command without errDrop. */
    val abort = in Bool() default False
  }

  noIoPrefix()

  def attachLmmi(lmmi: LMMI): Unit = {
    io.LMMIREQUEST := lmmi.cmd.valid
    io.LMMIWRRD_N := lmmi.cmd.write
    io.LMMIOFFSET := lmmi.cmd.offset
    io.LMMIWDATA := lmmi.cmd.data
    lmmi.cmd.ready := io.LMMIREADY
    lmmi.rsp.valid := io.LMMIRDATAVALID
    lmmi.rsp.payload := io.LMMIRDATA
  }

  val por = Reg(UInt(4 bits)) init 15
  val stall = Reg(UInt(4 bits)) init 0
  val readyComb = (por === 0) && (stall === 0)
  io.LMMIREADY := readyComb
  val accept = io.LMMIREQUEST && readyComb

  when(por =/= 0) {
    por := por - 1
  }
  when(accept) {
    stall := stallCycles
  } elsewhen (stall =/= 0) {
    stall := stall - 1
  }

  val sawBp = RegInit(False)
  when(io.LMMIREQUEST && !readyComb) {
    sawBp := True
  }
  io.sawBackpressure := sawBp

  val cap = Vec(Reg(Bits(8 bits)) init 0, ConfigLmmiModel.capBytes)
  val capIdx = Reg(UInt(5 bits)) init 0
  val cmdPos = Reg(UInt(2 bits)) init 0
  val cmdCount = Reg(UInt(2 bits)) init 0
  val errOffset = RegInit(False)
  val errDrop = RegInit(False)
  val errOpcode = False
  val idMode = RegInit(False)
  val idCmd = RegInit(False)
  val idIdx = Reg(UInt(2 bits)) init 0
  val rdataHold = Reg(Bits(8 bits)) init B(ConfigLmmiModel.readStatus, 8 bits)
  val idcode = Vec(ConfigLmmiModel.idcodeBytes.map(b => B(b, 8 bits)))

  val rdPend = RegInit(False)
  val reqDly = RegNext(io.LMMIREQUEST) init False
  when(io.abort) {
    cmdPos := 0
    cmdCount := 0
    idMode := False
    idCmd := False
    idIdx := 0
    rdPend := False
    capIdx := 0
    errDrop := False
    errOffset := False
  } elsewhen (reqDly && !io.LMMIREQUEST && cmdPos =/= 0) {
    errDrop := True
  }

  when(accept) {
    when(io.LMMIOFFSET =/= U(NexusSysConfig.addrCfg, 8 bits)) {
      errOffset := True
    }
    when(io.LMMIWRRD_N) {
      when(cmdPos === 0) {
        idCmd := io.LMMIWDATA === B(NexusSysConfig.readId, 8 bits)
        when(io.LMMIWDATA === B(NexusSysConfig.readId, 8 bits)) {
          idMode := True
          idIdx := 0
        }
      }
      for (i <- 0 until ConfigLmmiModel.capBytes) {
        when(capIdx === i) {
          cap(i) := io.LMMIWDATA
        }
      }
      when(capIdx < ConfigLmmiModel.capBytes) {
        capIdx := capIdx + 1
      }
      cmdPos := cmdPos + 1
      when(cmdPos === 3 && !idCmd) {
        cmdCount := cmdCount + 1
      }
    } otherwise {
      rdPend := True
      when(idMode) {
        rdataHold := idcode(idIdx)
        idIdx := idIdx + 1
      } otherwise {
        rdataHold := B(ConfigLmmiModel.readStatus, 8 bits)
      }
    }
  }

  val rvalid = RegInit(False)
  rvalid := False
  when(rdPend && stall === 1) {
    rvalid := True
    rdPend := False
  }
  io.LMMIRDATAVALID := rvalid

  val proto = errOffset || errDrop
  assert(!errOffset, "CONFIG_LMMI offset is not ADDR_CFG")
  assert(!errDrop, "LMMIREQUEST dropped mid-command")
  io.protocolError := proto
  io.errorOffset := errOffset
  io.errorDrop := errDrop
  io.errorOpcode := errOpcode
  /* Success status is non-zero; host timeout/stall sentinels only on error. */
  io.LMMIRDATA := Mux(proto, B(CONFIG_LMMI.lmmiTimeoutRdata, 8 bits), rdataHold)
  io.capturedCount := capIdx
  io.lastBytes := cap
}

object ConfigLmmiModel {
  val capBytes = 12
  val readStatus = 0x01
  /** Stand-in Nexus IDCODE for sim READ_ID (0xE0) data bytes. */
  val idcode = 0x010F1043L
  def idcodeBytes: Seq[Int] = NexusSysConfig.be32(idcode)
}
