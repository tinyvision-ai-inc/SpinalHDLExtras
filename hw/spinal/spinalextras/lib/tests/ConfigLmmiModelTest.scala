package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone.Wishbone
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.{
  CONFIG_LMMI,
  ConfigClkrstCoreModel,
  ConfigLmmiModel,
  NexusSysConfig
}
import spinalextras.lib.soc.Rcc

import scala.language.postfixOps

/**
 * RCC mailbox + [[ConfigLmmiFabricHost]] + [[ConfigClkrstCoreModel]] +
 * [[ConfigLmmiModel]]. Writer is the only CONFIG LMMI master.
 */
class ConfigLmmiHostDut(withCdc: Boolean, stallCycles: Int) extends Component {
  val io = new Bundle {
    val start = in Bool()
    val lmmiClk = in Bool()
    val lmmiReset = in Bool()
    val bus = slave(Wishbone(Rcc.wishboneConfig))
    val writerBusy = out Bool()
    val capturedCount = out UInt (5 bits)
    val lastBytes = out Vec(Bits(8 bits), ConfigLmmiModel.capBytes)
    val protocolError = out Bool()
    val errorOffset = out Bool()
    val errorDrop = out Bool()
    val errorOpcode = out Bool()
    val sawBackpressure = out Bool()
    val LMMIREQUEST = out Bool()
    val LMMIREADY = out Bool()
    val LMMI_CLK_O = out Bool()
    val LMMI_RST = out Bool()
  }

  val cpuCd = ClockDomain.current
  val lmmiCd = if (withCdc) {
    ClockDomain(
      clock = io.lmmiClk,
      reset = io.lmmiReset,
      config = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH),
      frequency = CONFIG_LMMI.lmmiClkSpec.toClockFrequency()
    )
  } else {
    cpuCd
  }

  val rcc = new Rcc()
  rcc.io.bus <> io.bus
  rcc.io.pllLock := True

  val host = CONFIG_LMMI.driveFromRcc(rcc, io.start, cpuCd, lmmiCd)
  val fabricClk = if (withCdc) io.lmmiClk else cpuCd.readClockWire
  val fabricRstn = if (withCdc) !io.lmmiReset else !cpuCd.isResetActive
  val clkrst = lmmiCd on new ConfigClkrstCoreModel
  clkrst.io.LMMI_CLK := fabricClk
  clkrst.io.LMMI_LRST_N := fabricRstn
  clkrst.io.OSCCLK := fabricClk
  clkrst.io.JTAG_LRST_N := True
  clkrst.io.MBISTCLK := False
  clkrst.io.SEDC_CLK := False
  clkrst.io.SEDC_LRST_N := True
  clkrst.io.WDT_LRST_N := True
  val model = lmmiCd on new ConfigLmmiModel(stallCycles)
  model.io.LMMICLK := clkrst.io.LMMI_CLK_O
  model.io.LMMIRESETN := clkrst.io.LMMI_RST
  model.io.SMCLK := clkrst.io.CFG_CLK
  model.io.RSTSMCLK := clkrst.io.SMCLK_RST
  model.attachLmmi(host.io.lmmi)
  if (withCdc) {
    model.io.abort := lmmiCd on { BufferCC(host.io.mailboxReset, init = False) }
  } else {
    model.io.abort := host.io.mailboxReset
  }

  io.writerBusy := (if (withCdc) BufferCC(host.io.busy) else CombInit(host.io.busy))
  io.capturedCount := model.io.capturedCount
  io.lastBytes := model.io.lastBytes
  io.protocolError := model.io.protocolError
  io.errorOffset := model.io.errorOffset
  io.errorDrop := model.io.errorDrop
  io.errorOpcode := model.io.errorOpcode
  io.sawBackpressure := model.io.sawBackpressure
  io.LMMIREQUEST := model.io.LMMIREQUEST
  io.LMMIREADY := model.io.LMMIREADY
  io.LMMI_CLK_O := clkrst.io.LMMI_CLK_O
  io.LMMI_RST := clkrst.io.LMMI_RST
  if (withCdc) {
    List(
      io.capturedCount, io.protocolError, io.errorOffset, io.errorDrop, io.errorOpcode,
      io.sawBackpressure, io.LMMIREQUEST, io.LMMIREADY, io.LMMI_CLK_O, io.LMMI_RST
    ).foreach(_.addTag(crossClockDomain))
    io.lastBytes.foreach(_.addTag(crossClockDomain))
  }
}

class ConfigLmmiModelTest extends AnyFunSuite {
  val expected = NexusSysConfig.refreshFromFlash0().flatten.map(_ & 0xFF)

  def addr(off: Int): Long = (Rcc.mapping.base + off).toLong

  def wbIdle(bus: Wishbone): Unit = {
    bus.CYC #= false
    bus.STB #= false
    bus.WE #= false
    bus.ADR #= 0
    bus.DAT_MOSI #= 0
    bus.SEL #= 0
  }

  def wbWrite(bus: Wishbone, a: Long, data: Long, cd: ClockDomain): Unit = {
    bus.CYC #= true
    bus.STB #= true
    bus.WE #= true
    bus.ADR #= a
    bus.DAT_MOSI #= data
    bus.SEL #= 0xF
    cd.waitSamplingWhere(bus.ACK.toBoolean)
    bus.STB #= false
    bus.CYC #= false
    cd.waitSampling()
  }

  def wbRead(bus: Wishbone, a: Long, cd: ClockDomain): Long = {
    bus.CYC #= true
    bus.STB #= true
    bus.WE #= false
    bus.ADR #= a
    bus.SEL #= 0xF
    cd.waitSamplingWhere(bus.ACK.toBoolean)
    val v = bus.DAT_MISO.toLong
    bus.STB #= false
    bus.CYC #= false
    cd.waitSampling()
    v
  }

  def captured(dut: ConfigLmmiHostDut): Seq[Int] = {
    val n = dut.io.capturedCount.toInt
    (0 until n).map(i => dut.io.lastBytes(i).toInt & 0xFF)
  }

  def checkSeq(dut: ConfigLmmiHostDut, tag: String): Unit = {
    val got = captured(dut)
    val msg =
      s"$tag offset=${dut.io.errorOffset.toBoolean} drop=${dut.io.errorDrop.toBoolean} " +
        s"opcode=${dut.io.errorOpcode.toBoolean} got=${got.map(b => f"$b%02x")}"
    assert(!dut.io.protocolError.toBoolean, s"writer sequence $msg")
    assert(got == expected, s"$tag $msg expected ${expected.map(b => f"$b%02x")}")
    assert(!dut.io.writerBusy.toBoolean)
  }

  def cpuScratch(dut: ConfigLmmiHostDut, cd: ClockDomain): Unit = {
    val por = wbRead(dut.io.bus, addr(Rcc.OffConfigScratch), cd) & 0xFF
    assert(por == Rcc.configScratchReset, f"scratch POR want 0x${Rcc.configScratchReset}%02x got 0x$por%02x")
    val probe = 0x3C
    wbWrite(dut.io.bus, addr(Rcc.OffConfigScratch), probe, cd)
    val got = wbRead(dut.io.bus, addr(Rcc.OffConfigScratch), cd) & 0xFF
    assert(got == probe, f"scratch want 0x$probe%02x got 0x$got%02x")
  }

  test("writer + model same clock delivers refreshFromFlash0") {
    val name = "ConfigLmmiHostDut_same_s3"
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName(name).doSim(
      new ConfigLmmiHostDut(withCdc = false, stallCycles = 3).setDefinitionName(name)
    ) { dut =>
      SimTimeout(200 us)
      dut.clockDomain.forkStimulus(75 MHz)
      dut.io.lmmiClk #= false
      dut.io.lmmiReset #= false
      dut.io.start #= false
      wbIdle(dut.io.bus)
      dut.clockDomain.waitSampling(32)
      dut.io.start #= true
      dut.clockDomain.waitSampling()
      dut.io.start #= false

      var idle = 0
      var sawStall = false
      while ((dut.io.writerBusy.toBoolean || dut.io.capturedCount.toInt < expected.length) && idle < 8000) {
        if (dut.io.LMMIREQUEST.toBoolean && !dut.io.LMMIREADY.toBoolean) {
          sawStall = true
        }
        dut.clockDomain.waitSampling()
        idle += 1
      }
      dut.clockDomain.waitSampling(8)
      assert(sawStall || dut.io.sawBackpressure.toBoolean, "READY backpressure never seen")
      checkSeq(dut, "same-clock")
    }
  }

  test("attach_bus CDC (CPU vs LMMI_CLK_O) delivers refreshFromFlash0") {
    val name = "ConfigLmmiHostDut_cdc_s3"
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName(name).doSim(
      new ConfigLmmiHostDut(withCdc = true, stallCycles = 3).setDefinitionName(name)
    ) { dut =>
      SimTimeout(200 us)
      dut.clockDomain.forkStimulus(75 MHz)
      val lmmiCd = ClockDomain(
        clock = dut.io.lmmiClk,
        reset = dut.io.lmmiReset,
        config = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
      )
      lmmiCd.forkStimulus(45 MHz)
      dut.io.start #= false
      wbIdle(dut.io.bus)
      dut.clockDomain.waitSampling(32)
      lmmiCd.waitSampling(16)
      assert(dut.io.LMMI_RST.toBoolean == !dut.io.lmmiReset.toBoolean, "CLKRST LMMI_RST is LMMI_LRST_N")

      dut.io.start #= true
      dut.clockDomain.waitSampling()
      dut.io.start #= false

      var idle = 0
      var sawStall = false
      while ((dut.io.writerBusy.toBoolean || dut.io.capturedCount.toInt < expected.length) && idle < 16000) {
        if (dut.io.LMMIREQUEST.toBoolean && !dut.io.LMMIREADY.toBoolean) {
          sawStall = true
        }
        dut.clockDomain.waitSampling()
        idle += 1
      }
      lmmiCd.waitSampling(16)
      assert(sawStall || dut.io.sawBackpressure.toBoolean, "READY backpressure never seen")
      checkSeq(dut, "cdc")
    }
  }

  test("RCC scratch POR 0xC3 then write/read 0x3C (CDC)") {
    val name = "ConfigLmmiHostDut_scratch_cdc"
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName(name).doSim(
      new ConfigLmmiHostDut(withCdc = true, stallCycles = 3).setDefinitionName(name)
    ) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(75 MHz)
      val lmmiCd = ClockDomain(
        clock = dut.io.lmmiClk,
        reset = dut.io.lmmiReset,
        config = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
      )
      lmmiCd.forkStimulus(45 MHz)
      dut.io.start #= false
      wbIdle(dut.io.bus)
      dut.clockDomain.waitSampling(32)
      lmmiCd.waitSampling(16)
      cpuScratch(dut, dut.clockDomain)
    }
  }

  test("RCC push/GO/pop READ_ID returns model IDCODE") {
    val name = "ConfigLmmiHostDut_fw_read_id"
    val wr = Seq(NexusSysConfig.readId, 0x00, 0x00, 0x00)
    val id = ConfigLmmiModel.idcodeBytes
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName(name).doSim(
      new ConfigLmmiHostDut(withCdc = false, stallCycles = 3).setDefinitionName(name)
    ) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(75 MHz)
      dut.io.lmmiClk #= false
      dut.io.lmmiReset #= false
      dut.io.start #= false
      wbIdle(dut.io.bus)
      dut.clockDomain.waitSampling(32)

      wr.foreach { b =>
        wbWrite(dut.io.bus, addr(Rcc.OffConfigPush), b, dut.clockDomain)
      }
      wbWrite(dut.io.bus, addr(Rcc.OffConfigGo), id.length, dut.clockDomain)

      var n = 0
      var st = 1L
      while ((st & 1) != 0 && n < 8000) {
        st = wbRead(dut.io.bus, addr(Rcc.OffConfigStat), dut.clockDomain)
        n += 1
      }
      assert((st & 1) == 0, "FW sequencer still busy")
      assert(!dut.io.writerBusy.toBoolean)
      assert(!dut.io.protocolError.toBoolean, "READ_ID treated as protocol error")
      assert(captured(dut) == wr, s"captured ${captured(dut).map(b => f"$b%02x")}")

      val got = id.indices.map(_ =>
        (wbRead(dut.io.bus, addr(Rcc.OffConfigPop), dut.clockDomain) & 0xFF).toInt
      )
      assert(got == id, s"IDCODE got ${got.map(b => f"$b%02x")} want ${id.map(b => f"$b%02x")}")
      val empty = wbRead(dut.io.bus, addr(Rcc.OffConfigPop), dut.clockDomain) & 0xFF
      assert(empty == Rcc.configPopEmpty, f"empty pop want 0x${Rcc.configPopEmpty}%02x got 0x$empty%02x")
    }
  }

  test("RCC push/GO/pop READ_ID over CPU/LMMI CDC") {
    val name = "ConfigLmmiHostDut_fw_read_id_cdc"
    val wr = Seq(NexusSysConfig.readId, 0x00, 0x00, 0x00)
    val id = ConfigLmmiModel.idcodeBytes
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName(name).doSim(
      new ConfigLmmiHostDut(withCdc = true, stallCycles = 3).setDefinitionName(name)
    ) { dut =>
      SimTimeout(500 us)
      dut.clockDomain.forkStimulus(75 MHz)
      val lmmiCd = ClockDomain(
        clock = dut.io.lmmiClk,
        reset = dut.io.lmmiReset,
        config = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
      )
      lmmiCd.forkStimulus(45 MHz)
      dut.io.start #= false
      wbIdle(dut.io.bus)
      dut.clockDomain.waitSampling(32)
      lmmiCd.waitSampling(16)

      wr.foreach { b =>
        wbWrite(dut.io.bus, addr(Rcc.OffConfigPush), b, dut.clockDomain)
      }
      var n = 0
      var st = 0L
      var fill = 0L
      var occ = 0L
      do {
        st = wbRead(dut.io.bus, addr(Rcc.OffConfigStat), dut.clockDomain)
        fill = (st >> 1) & 7
        n += 1
      } while (fill < wr.length && n < 200)
      assert(fill >= wr.length, f"push CDC wrFill $fill want ${wr.length} st=0x$st%02x")

      wbWrite(dut.io.bus, addr(Rcc.OffConfigGo), id.length, dut.clockDomain)
      dut.clockDomain.waitSampling(2000)
      lmmiCd.waitSampling(200)
      val stAfter = wbRead(dut.io.bus, addr(Rcc.OffConfigStat), dut.clockDomain)
      val cap = captured(dut)
      assert(dut.io.writerBusy.toBoolean || cap.nonEmpty || (stAfter & 1) != 0 || ((stAfter >> 4) & 0xF) != 0,
        f"GO did nothing st=0x$stAfter%02x cap=${cap.map(b => f"$b%02x")} busy=${dut.io.writerBusy.toBoolean}")
      assert(cap == wr, s"captured ${cap.map(b => f"$b%02x")}")

      n = 0
      occ = (stAfter >> 4) & 0xF
      st = stAfter
      do {
        st = wbRead(dut.io.bus, addr(Rcc.OffConfigStat), dut.clockDomain)
        occ = (st >> 4) & 0xF
        n += 1
      } while (occ < id.length && n < 400)
      assert(occ >= id.length, f"CPU pop occ $occ st=0x$st%02x")
      assert(!dut.io.writerBusy.toBoolean)
      assert(!dut.io.protocolError.toBoolean)
      val got = id.indices.map(_ =>
        (wbRead(dut.io.bus, addr(Rcc.OffConfigPop), dut.clockDomain) & 0xFF).toInt
      )
      assert(got == id, s"IDCODE CDC got ${got.map(b => f"$b%02x")} want ${id.map(b => f"$b%02x")} st=0x$stAfter%02x")
      assert(got == id, s"IDCODE CDC got ${got.map(b => f"$b%02x")} want ${id.map(b => f"$b%02x")}")
    }
  }

  test("RCC scratch ACKs while ROM writer is busy and LMMI READY is low") {
    val name = "ConfigLmmiHostDut_scratch_during_writer"
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName(name).doSim(
      new ConfigLmmiHostDut(withCdc = true, stallCycles = 15).setDefinitionName(name)
    ) { dut =>
      SimTimeout(80 us)
      dut.clockDomain.forkStimulus(75 MHz)
      val lmmiCd = ClockDomain(
        clock = dut.io.lmmiClk,
        reset = dut.io.lmmiReset,
        config = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = HIGH)
      )
      lmmiCd.forkStimulus(45 MHz)
      dut.io.start #= false
      wbIdle(dut.io.bus)
      dut.clockDomain.waitSampling(32)
      lmmiCd.waitSampling(16)

      dut.io.start #= true
      dut.clockDomain.waitSampling()
      dut.io.start #= false

      var n = 0
      while (!dut.io.writerBusy.toBoolean && n < 256) {
        dut.clockDomain.waitSampling()
        n += 1
      }
      assert(dut.io.writerBusy.toBoolean, "ROM writer never went busy")

      n = 0
      var sawStall = false
      while (!sawStall && n < 512) {
        if (dut.io.LMMIREQUEST.toBoolean && !dut.io.LMMIREADY.toBoolean) {
          sawStall = true
        }
        dut.clockDomain.waitSampling()
        n += 1
      }
      assert(sawStall || dut.io.sawBackpressure.toBoolean, "READY low never seen while writer busy")
      assert(dut.io.writerBusy.toBoolean, "writer finished before scratch probe")

      cpuScratch(dut, dut.clockDomain)
      assert(dut.io.writerBusy.toBoolean, "writer must still be busy after scratch")
      val st = wbRead(dut.io.bus, addr(Rcc.OffConfigStat), dut.clockDomain)
      assert((st & 1) == 1, f"status bit0 busy want 1 got 0x$st%02x")
    }
  }

  test("CONFIG_RESET clears mailbox wrFill") {
    val name = "ConfigLmmiHostDut_config_reset"
    val wr = Seq(NexusSysConfig.readId, 0x00, 0x00, 0x00)
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName(name).doSim(
      new ConfigLmmiHostDut(withCdc = false, stallCycles = 3).setDefinitionName(name)
    ) { dut =>
      SimTimeout(80 us)
      dut.clockDomain.forkStimulus(75 MHz)
      dut.io.lmmiClk #= false
      dut.io.lmmiReset #= false
      dut.io.start #= false
      wbIdle(dut.io.bus)
      dut.clockDomain.waitSampling(32)

      wr.foreach { b =>
        wbWrite(dut.io.bus, addr(Rcc.OffConfigPush), b, dut.clockDomain)
      }
      var st = wbRead(dut.io.bus, addr(Rcc.OffConfigStat), dut.clockDomain)
      assert(((st >> 1) & 7) == wr.length, f"wrFill before reset st=0x$st%02x")

      wbWrite(dut.io.bus, addr(Rcc.OffConfigReset), 1, dut.clockDomain)
      dut.clockDomain.waitSampling(Rcc.cpuResetCycles + 4)
      st = wbRead(dut.io.bus, addr(Rcc.OffConfigStat), dut.clockDomain)
      assert((st & 1) == 0, f"busy after reset st=0x$st%02x")
      assert(((st >> 1) & 7) == 0, f"wrFill after reset st=0x$st%02x")
      assert(!dut.io.writerBusy.toBoolean)
    }
  }
}
