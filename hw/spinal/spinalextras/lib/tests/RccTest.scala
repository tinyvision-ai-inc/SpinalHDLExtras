package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.wishbone.Wishbone
import spinalextras.lib.Config
import spinalextras.lib.soc.Rcc

import scala.language.postfixOps

class RccTest extends AnyFunSuite {
  def addr(off: Int): Long = (Rcc.mapping.base + off).toLong

  def wbWrite(bus: Wishbone, addr: Long, data: Long, cd: ClockDomain): Unit = {
    bus.CYC #= true
    bus.STB #= true
    bus.WE #= true
    bus.ADR #= addr
    bus.DAT_MOSI #= data
    bus.SEL #= 0xF
    cd.waitSamplingWhere(bus.ACK.toBoolean)
    bus.STB #= false
    bus.CYC #= false
    cd.waitSampling()
  }

  def wbRead(bus: Wishbone, addr: Long, cd: ClockDomain): Long = {
    bus.CYC #= true
    bus.STB #= true
    bus.WE #= false
    bus.ADR #= addr
    bus.SEL #= 0xF
    cd.waitSamplingWhere(bus.ACK.toBoolean)
    val v = bus.DAT_MISO.toLong
    bus.STB #= false
    bus.CYC #= false
    cd.waitSampling()
    v
  }

  test("cpu reset pulse and fabric alive") {
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName("Rcc").doSim(new Rcc()) { dut =>
      SimTimeout(100 us)
      dut.clockDomain.forkStimulus(75 MHz)
      dut.io.pllLock #= true
      dut.io.configPush.ready #= true
      dut.io.configPop.valid #= false
      dut.io.configPop.payload #= 0
      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.clockDomain.waitSampling(4)

      assert((wbRead(dut.io.bus, addr(Rcc.OffPllStatus), dut.clockDomain) & 1) == 1)
      assert((wbRead(dut.io.bus, addr(Rcc.OffConfigScratch), dut.clockDomain) & 0xFF) == Rcc.configScratchReset)
      wbWrite(dut.io.bus, addr(Rcc.OffConfigScratch), 0x3C, dut.clockDomain)
      assert((wbRead(dut.io.bus, addr(Rcc.OffConfigScratch), dut.clockDomain) & 0xFF) == 0x3C)
      assert((wbRead(dut.io.bus, addr(Rcc.OffConfigPop), dut.clockDomain) & 0xFF) == Rcc.configPopEmpty)
      assert((wbRead(dut.io.bus, addr(Rcc.OffFabricAlive), dut.clockDomain) & 1) == 0)

      wbWrite(dut.io.bus, addr(Rcc.OffFabricAlive), 1, dut.clockDomain)
      assert((wbRead(dut.io.bus, addr(Rcc.OffFabricAlive), dut.clockDomain) & 1) == 1)
      wbWrite(dut.io.bus, addr(Rcc.OffFabricAlive), 0, dut.clockDomain)
      assert((wbRead(dut.io.bus, addr(Rcc.OffFabricAlive), dut.clockDomain) & 1) == 1)

      wbWrite(dut.io.bus, addr(Rcc.OffCpuReset), 1, dut.clockDomain)
      assert(dut.io.cpuResetPulse.toBoolean)
      dut.clockDomain.waitSampling(Rcc.cpuResetCycles + 4)
      assert(!dut.io.cpuResetPulse.toBoolean)
      assert((wbRead(dut.io.bus, addr(Rcc.OffFabricAlive), dut.clockDomain) & 1) == 1)
    }
  }

  test("reset cause POR, software latch, W1C clear") {
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName("RccResetCause").doSim(new Rcc()) { dut =>
      SimTimeout(100 us)
      dut.clockDomain.forkStimulus(75 MHz)
      dut.io.pllLock #= true
      dut.io.configPush.ready #= true
      dut.io.configPop.valid #= false
      dut.io.configPop.payload #= 0
      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.clockDomain.waitSampling(4)

      val porMask = Rcc.resetCausePorMask.toLong
      assert((wbRead(dut.io.bus, addr(Rcc.OffResetCause), dut.clockDomain) & porMask) == porMask)

      wbWrite(dut.io.bus, addr(Rcc.OffFabricAlive), 1, dut.clockDomain)
      wbWrite(dut.io.bus, addr(Rcc.OffCpuReset), 1, dut.clockDomain)
      dut.clockDomain.waitSampling(Rcc.cpuResetCycles + 2)
      val swMask = 1L << Rcc.resetCauseSoftware
      assert((wbRead(dut.io.bus, addr(Rcc.OffResetCause), dut.clockDomain) & swMask) == swMask)

      wbWrite(dut.io.bus, addr(Rcc.OffResetCause), porMask | swMask, dut.clockDomain)
      assert((wbRead(dut.io.bus, addr(Rcc.OffResetCause), dut.clockDomain) & (porMask | swMask)) == 0)
    }
  }

  test("config reset pulse blocks push/go") {
    SimConfig.withConfig(Config.spinal).withVerilator.workspaceName("RccConfigReset").doSim(new Rcc()) { dut =>
      SimTimeout(100 us)
      dut.clockDomain.forkStimulus(75 MHz)
      dut.io.pllLock #= true
      dut.io.configPush.ready #= true
      dut.io.configPop.valid #= false
      dut.io.configPop.payload #= 0
      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.clockDomain.waitSampling(4)

      wbWrite(dut.io.bus, addr(Rcc.OffConfigReset), 1, dut.clockDomain)
      assert(dut.io.configResetPulse.toBoolean)

      dut.io.bus.CYC #= true
      dut.io.bus.STB #= true
      dut.io.bus.WE #= true
      dut.io.bus.ADR #= addr(Rcc.OffConfigPush)
      dut.io.bus.DAT_MOSI #= 0xE0
      dut.io.bus.SEL #= 0xF
      var pushSeen = false
      dut.clockDomain.waitSamplingWhere {
        if (dut.io.bus.ACK.toBoolean) {
          pushSeen = dut.io.configPush.valid.toBoolean
        }
        dut.io.bus.ACK.toBoolean
      }
      dut.io.bus.STB #= false
      dut.io.bus.CYC #= false
      dut.clockDomain.waitSampling()
      assert(!pushSeen)

      wbWrite(dut.io.bus, addr(Rcc.OffConfigGo), 4, dut.clockDomain)
      dut.clockDomain.waitSampling(2)
      assert(!dut.io.configGo.toBoolean)

      dut.clockDomain.waitSampling(Rcc.cpuResetCycles + 4)
      assert(!dut.io.configResetPulse.toBoolean)
      wbWrite(dut.io.bus, addr(Rcc.OffConfigGo), 4, dut.clockDomain)
      assert(dut.io.configGo.toBoolean)
    }
  }
}
