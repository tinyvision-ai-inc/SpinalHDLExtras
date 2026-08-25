package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._
import spinal.lib.fsm.{EntryPoint, State, StateMachine}
import spinalextras.lib.Constraints
import spinalextras.lib.bus.LMMI

import scala.language.postfixOps

object NexusSysConfig {
  val addrCfg = 1
  val iscEnableX = 0x74
  val iscDisable = 0x26
  val lscProgCntrl0 = 0x22
  val lscRefresh = 0x79
  val cr0SpimBit = 19
  val cmdGapCycles = 4
  val lmmiFmax = 50 MHz

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
}

case class ConfigLmmiCmdBeat() extends Bundle {
  val data = Bits(8 bits)
  val lastInCmd = Bool()
}

/**
 * LMMI host that writes CONFIG_LMMI slave commands (offset ADDR_CFG=0x01).
 * Holds request for the whole command, then waits [[NexusSysConfig.cmdGapCycles]]
 * clocks after ready before the next command.
 */
class ConfigLmmiCommandWriter(commands: Seq[Seq[Int]] = NexusSysConfig.refreshCommands()) extends Component {
  val io = new Bundle {
    val lmmi = master(LMMI(8, 8))
    val start = in Bool()
    val busy = out Bool()
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

  io.lmmi.cmd.valid := False
  io.lmmi.cmd.write := True
  io.lmmi.cmd.offset := NexusSysConfig.addrCfg
  io.lmmi.cmd.data := beat.data

  val fsm = new StateMachine {
    val idle = new State with EntryPoint
    val send = new State
    val waitGap = new State

    io.busy := !isActive(idle)

    idle.whenIsActive {
      idx := 0
      when(io.start) {
        goto(send)
      }
    }

    send.whenIsActive {
      io.lmmi.cmd.valid := True
      when(io.lmmi.cmd.fire) {
        when(beat.lastInCmd) {
          gap := NexusSysConfig.cmdGapCycles - 1
          goto(waitGap)
        } otherwise {
          idx := idx + 1
        }
      }
    }

    waitGap.whenIsActive {
      when(gap === 0) {
        when(idx === beats.length - 1) {
          goto(idle)
        } otherwise {
          idx := idx + 1
          goto(send)
        }
      } otherwise {
        gap := gap - 1
      }
    }
  }
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
 * MULTIBOOT + CONFIG_LMMI + CONFIG_CLKRST_CORE. Drive [[io.bootAddr]] then pulse
 * [[io.start]] to set CR0.SPIM and issue LSC_REFRESH.
 *
 * [[io.oscClk]] is OSCD.HFCLKCFG (CONFIG_CLKRST_CORE.OSCCLK).
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
    tog
  } else {
    sysCd.readClockWire
  }

  val clkrst = CONFIG_CLKRST_CORE(io.oscClk, lmmiClkIn, !sysCd.isResetActive)
  val lmmiCd = sysCd.copy(
    clock = clkrst.io.LMMI_CLK_O,
    frequency = lmmiFreq.map(FixedFrequency(_)).getOrElse(sysCd.frequency)
  )

  addPrePopTask(() => {
    lmmiFreq.foreach(f => Constraints.create_clock(clkrst.io.LMMI_CLK_O, f))
  })

  val startLmmi = if (divided) PulseCCByToggle(io.start, clockIn = sysCd, clockOut = lmmiCd) else io.start

  val lmmiArea = new ClockingArea(lmmiCd) {
    val bootAddr = if (divided) BufferCC(io.bootAddr) else CombInit(io.bootAddr)
    MULTIBOOT(bootAddr)

    val writer = new ConfigLmmiCommandWriter()
    writer.io.start := startLmmi

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
