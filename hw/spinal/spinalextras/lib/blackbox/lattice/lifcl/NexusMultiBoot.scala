package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinalextras.lib.bus.LMMI

import scala.language.postfixOps

object NexusSysConfig {
  val addrCfg = 1
  val iscEnableX = 0x74
  val iscDisable = 0x26
  val lscProgCntrl0 = 0x22
  val lscRefresh = 0x79
  val readId = 0xE0
  val cr0SpimBit = 19
  val cmdGapCycles = 4
  val lmmiFmax = 50 MHz
  val fwDepth = 16

  def be32(value: Long): Seq[Int] = {
    val v = value & 0xFFFFFFFFL
    Seq(((v >> 24) & 0xFF).toInt, ((v >> 16) & 0xFF).toInt, ((v >> 8) & 0xFF).toInt, (v & 0xFF).toInt)
  }

  def refreshCommands(cr0: Long = 1L << cr0SpimBit, cr0Mask: Long = 0xFFF7FFFFL): Seq[Seq[Int]] = Seq(
    Seq(iscEnableX, 0x00, 0x00, 0x00),
    Seq(lscProgCntrl0, 0x01, 0x00, 0x00) ++ be32(cr0) ++ be32(cr0Mask),
    Seq(iscDisable, 0x00, 0x00, 0x00),
    Seq(lscRefresh, 0x00, 0x00, 0x00)
  )

  /** ISC_ENABLE_X, ISC_DISABLE, LSC_REFRESH. No CR0/SPIM (boot from flash 0). */
  def refreshFromFlash0(): Seq[Seq[Int]] = Seq(
    Seq(iscEnableX, 0x00, 0x00, 0x00),
    Seq(iscDisable, 0x00, 0x00, 0x00),
    Seq(lscRefresh, 0x00, 0x00, 0x00)
  )
}

case class ConfigLmmiCmdBeat() extends Bundle {
  val data = Bits(8 bits)
  val lastInCmd = Bool()
}

/**
 * CONFIG_LMMI sequencer at ADDR_CFG=0x01. ROM playback ([[io.start]]) is the
 * constructor command list with REQUEST dropped between commands. Firmware
 * playback ([[io.startFw]]) is one Class A/B/C command: nWr writes then nRd
 * reads with REQUEST held (TN-02099 D.3).
 */
class ConfigLmmiCommandWriter(commands: Seq[Seq[Int]] = NexusSysConfig.refreshCommands(),
                              fwDepth: Int = NexusSysConfig.fwDepth) extends Component {
  val io = new Bundle {
    val lmmi = master(LMMI(8, 8))
    val start = in Bool()
    val startFw = in Bool()
    val fwNrd = in UInt (log2Up(fwDepth + 1) bits)
    val fwWr = slave Stream (Bits(8 bits))
    val rd = master Stream (Bits(8 bits))
    val busy = out Bool()
    val wrFill = out UInt (log2Up(fwDepth + 1) bits)
    val rdOcc = out UInt (log2Up(fwDepth + 1) bits)
    val reset = in Bool() default False
  }

  val beats = Vec(commands.flatMap { cmd =>
    cmd.zipWithIndex.map { case (b, i) =>
      val beat = ConfigLmmiCmdBeat()
      beat.data := B(b, 8 bits)
      beat.lastInCmd := Bool(i == cmd.length - 1)
      beat
    }
  })
  val idx = Reg(UInt(log2Up(beats.length) bits)) init 0
  val gap = Reg(UInt(log2Up(NexusSysConfig.cmdGapCycles + 1) bits)) init 0
  val beat = beats(idx)

  val cntW = log2Up(fwDepth + 1)
  val wrMem = Mem(Bits(8 bits), fwDepth)
  val wrFill = Reg(UInt(cntW bits)) init 0
  val nWr = Reg(UInt(cntW bits)) init 0
  val nRd = Reg(UInt(cntW bits)) init 0
  val wrIdx = Reg(UInt(cntW bits)) init 0
  val rdIdx = Reg(UInt(cntW bits)) init 0
  val rdFifo = StreamFifo(Bits(8 bits), fwDepth)
  rdFifo.io.flush := io.reset
  io.rd << rdFifo.io.pop
  io.wrFill := wrFill
  io.rdOcc := rdFifo.io.occupancy.resized

  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val send = new State
    val waitGap = new State
    val sendFw = new State
    io.busy := !isActive(idle) && !io.reset
  }

  io.fwWr.ready := fsm.isActive(fsm.idle) && wrFill =/= U(fwDepth) && !io.reset

  fsm.always {
    when(io.reset) {
      fsm.forceGoto(fsm.idle)
      wrFill := 0
      nWr := 0
      nRd := 0
      wrIdx := 0
      rdIdx := 0
      idx := 0
      gap := 0
    }
  }

  when(io.fwWr.fire) {
    wrMem.write(wrFill.resized, io.fwWr.payload)
    wrFill := wrFill + 1
  }

  rdFifo.io.push.valid := False
  rdFifo.io.push.payload := io.lmmi.rsp.payload

  val cmdValid = Bool()
  val cmdWrite = Bool()
  val cmdData = Bits(8 bits)
  cmdValid := False
  cmdWrite := True
  cmdData := beat.data

  val wrData = wrMem.readAsync(wrIdx.resized)

  fsm.idle.whenIsActive {
    idx := 0
    wrIdx := 0
    rdIdx := 0
    when(io.start) {
      fsm.goto(fsm.send)
    } elsewhen (io.startFw && wrFill =/= 0) {
      val launched = wrFill
      nWr := launched
      /* GO nRead; if the CDC path still shows 0, Class A nRead=nWr. */
      nRd := Mux(io.fwNrd === 0, launched, io.fwNrd)
      wrFill := 0
      fsm.goto(fsm.sendFw)
    }
  }

  fsm.send.whenIsActive {
    cmdValid := True
    cmdWrite := True
    cmdData := beat.data
    when(io.lmmi.cmd.fire) {
      when(beat.lastInCmd) {
        gap := NexusSysConfig.cmdGapCycles - 1
        fsm.goto(fsm.waitGap)
      } otherwise {
        idx := idx + 1
      }
    }
  }

  fsm.waitGap.whenIsActive {
    when(gap === 0) {
      when(idx === beats.length - 1) {
        fsm.goto(fsm.idle)
      } otherwise {
        idx := idx + 1
        fsm.goto(fsm.send)
      }
    } otherwise {
      gap := gap - 1
    }
  }

  fsm.sendFw.whenIsActive {
    val writing = wrIdx < nWr
    val reading = !writing && (rdIdx < nRd)
    cmdValid := writing || reading
    cmdWrite := writing
    cmdData := wrData
    when(writing && io.lmmi.cmd.fire) {
      wrIdx := wrIdx + 1
      when(wrIdx === nWr - 1 && nRd === 0) {
        fsm.goto(fsm.idle)
      }
    }
    when(reading && io.lmmi.rsp.valid && rdFifo.io.push.ready) {
      rdFifo.io.push.valid := True
      rdIdx := rdIdx + 1
      when(rdIdx === nRd - 1) {
        fsm.goto(fsm.idle)
      }
    }
  }

  io.lmmi.cmd.valid := cmdValid && !io.reset
  io.lmmi.cmd.write := cmdWrite
  io.lmmi.cmd.offset := NexusSysConfig.addrCfg
  io.lmmi.cmd.data := cmdData
}

object NexusMultiBoot {
  def sysFrequency(cd: ClockDomain): Option[HertzNumber] = {
    try {
      Some(cd.frequency.getValue)
    } catch {
      case _: Throwable => None
    }
  }

  def findOscClk(): Bool = {
    def search(x: Component): Option[OSCD] = {
      x.children.collectFirst { case o: OSCD => o }.orElse {
        x.children.toStream.map(search).collectFirst { case Some(o) => o }
      }
    }
    var c = Component.current
    while (c != null) {
      search(c).foreach(o => return o.io.HFCLKCFG)
      c = c.parent
    }
    SpinalError("NexusMultiBoot: connect oscClk from OSCD.io.HFCLKCFG")
  }
}

/**
 * MULTIBOOT + CONFIG_LMMI + CONFIG_CLKRST_CORE. Fabric LMMI host clock is
 * sys/2 when needed; LMMI_CLK_O is only wired to the primitive (map DRC).
 */
class NexusMultiBoot() extends Component {
  val io = new Bundle {
    val oscClk = in Bool()
    val bootAddr = in UInt (32 bits)
    val start = in Bool()
    val busy = out Bool()
  }

  val sysCd = ClockDomain.current
  val sysFreq = NexusMultiBoot.sysFrequency(sysCd)
  val divided = sysFreq.exists(_ > NexusSysConfig.lmmiFmax)
  val lmmiFreq = sysFreq.map(f => if (divided) f / 2 else f)

  val lmmiClkIn = if (divided) {
    val tog = Reg(Bool()) init False
    tog := !tog
    KeepAttribute(tog)
    tog
  } else {
    sysCd.readClockWire
  }

  val clkrst = CONFIG_CLKRST_CORE(io.oscClk, lmmiClkIn, !sysCd.isResetActive)
  val lmmiCd = ClockDomain(
    clock = lmmiClkIn,
    reset = sysCd.isResetActive,
    config = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
    frequency = lmmiFreq.map(FixedFrequency(_)).getOrElse(sysCd.frequency)
  )

  if (divided) {
    spinalextras.lib.Constraints.create_clock(lmmiClkIn, lmmiFreq.get)
  }

  val startLmmi = if (divided) PulseCCByToggle(io.start, clockIn = sysCd, clockOut = lmmiCd) else io.start

  val lmmiArea = new ClockingArea(lmmiCd) {
    val bootAddr = if (divided) BufferCC(io.bootAddr) else CombInit(io.bootAddr)
    MULTIBOOT(bootAddr)

    val writer = new ConfigLmmiCommandWriter()
    writer.io.start := startLmmi
    writer.io.startFw := False
    writer.io.fwNrd := 0
    writer.io.fwWr.valid := False
    writer.io.fwWr.payload := 0
    writer.io.rd.ready := True

    CONFIG_LMMI(
      clkrst.io.LMMI_CLK_O,
      clkrst.io.LMMI_RST,
      clkrst.io.CFG_CLK,
      clkrst.io.SMCLK_RST,
      writer.io.lmmi
    )
  }

  io.busy := (if (divided) BufferCC(lmmiArea.writer.io.busy) else CombInit(lmmiArea.writer.io.busy))
}
