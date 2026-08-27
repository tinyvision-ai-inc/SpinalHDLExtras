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
    Config.sim.doSim(new Rcc()) { dut =>
      SimTimeout(100 us)
      dut.clockDomain.forkStimulus(75 MHz)
      dut.io.pllLock #= true
      dut.io.bus.CYC #= false
      dut.io.bus.STB #= false
      dut.clockDomain.waitSampling(4)

      assert((wbRead(dut.io.bus, addr(Rcc.OffPllStatus), dut.clockDomain) & 1) == 1)
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
}
