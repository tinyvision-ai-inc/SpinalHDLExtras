package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.misc.SizeMapping
import spinalextras.lib.Config
import spinalextras.lib.soc.spinex.plugins.BusErrorTestPlugin
import spinalextras.lib.soc.spinex.{Spinex, SpinexConfig}

import java.io.File
import scala.language.postfixOps

/**
 * RISC-V sim of [[SpinexConfig.minimal]] / BusErrorTestPlugin windows.
 * Firmware: `sw/samples/bus_error/bus_error.bin` writes tohost @ 0x41000000:
 *   1 = all of UNIMPL-load, TIMEOUT, SLVERR, UNIMPL-store trapped with expected mcause.
 */
class SpinexBusErrorCpuDut(romBin: String) extends Component {
  val busErrTest = new BusErrorTestPlugin(hangTimeout = 1 us)
  val cfg = SpinexConfig.default(
    withJtag = false,
    xipConfig = None,
    withI2C = false,
    withUart = false,
    ram_mapping = SizeMapping(0x40000000L, 0x0004000 Bytes),
    rom_mapping = SizeMapping(0x20200000L, 0x00010000)
  ).appendPlugins(busErrTest)

  val som = Spinex(cfg)
  som.init_rom(romBin)
  busErrTest.host.simPublic()
  busErrTest.tohostWrCount.simPublic()
  busErrTest.unimplFireCount.simPublic()
  som.system.cpu.lastStagePc.simPublic()
  som.resetCtrl.systemReset.simPublic()
}

class SpinexBusErrorCpuTest extends AnyFunSuite {
  private val romBin = {
    val here = new File("sw/samples/bus_error/bus_error.bin")
    val alt = new File("SpinalHDLExtras/sw/samples/bus_error/bus_error.bin")
    if (here.exists()) here.getAbsolutePath
    else if (alt.exists()) alt.getAbsolutePath
    else here.getAbsolutePath
  }

  test("UNIMPL / TIMEOUT / SLVERR loads and UNIMPL store trap to mcause 5/7") {
    assert(new File(romBin).exists(), s"missing firmware $romBin — build sw/samples/bus_error")
    Config.sim.doSim(new SpinexBusErrorCpuDut(romBin).setDefinitionName("SpinexBusErrorCpu")) { dut =>
      SimTimeout(500 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.clockDomain.waitSampling(1200)

      var n = 0
      while (dut.busErrTest.host.toLong == 0 && n < 40000) {
        dut.clockDomain.waitSampling()
        n += 1
        if (n % 5000 == 0) {
          val pc = dut.som.system.cpu.lastStagePc.toLong & 0xffffffffL
          println(f"cyc=$n%6d pc=0x$pc%08x tohost=0x${dut.busErrTest.host.toLong}%x wrc=${dut.busErrTest.tohostWrCount.toLong} fire=${dut.busErrTest.unimplFireCount.toLong}")
        }
      }
      val host = dut.busErrTest.host.toLong & 0xffffffffL
      val wrc = dut.busErrTest.tohostWrCount.toLong
      val fire = dut.busErrTest.unimplFireCount.toLong
      val pc = dut.som.system.cpu.lastStagePc.toLong & 0xffffffffL
      println(f"done cyc=$n pc=0x$pc%08x tohost=0x$host%x wrc=$wrc fire=$fire")
      assert(host == 1L, f"expected tohost=1 (PASS), got 0x$host%08x (pc=0x$pc%08x wrc=$wrc fire=$fire)")
    }
  }
}
