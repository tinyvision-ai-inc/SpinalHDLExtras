package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.bus.simple.PipelinedMemoryBusConfig
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.{master, slave}
import spinalextras.lib.Config
import spinalextras.lib.bus.{PipelinedMemoryBusToWishbone, WishboneStage}

import scala.language.postfixOps

class WishboneStageErrDut(m2s: Boolean, s2m: Boolean) extends Component {
  val cfg = WishboneConfig(addressWidth = 32, dataWidth = 32, selWidth = 4,
    addressGranularity = AddressGranularity.BYTE, useERR = true)
  val io = new Bundle {
    val host = slave(Wishbone(cfg))
    val core = master(Wishbone(cfg))
  }
  io.core << WishboneStage(io.host, m2s_stage = m2s, s2m_stage = s2m)
}

class PmbToWbErrDut extends Component {
  val wbCfg = WishboneConfig(addressWidth = 32, dataWidth = 32, selWidth = 4,
    addressGranularity = AddressGranularity.BYTE, useERR = true)
  val io = new Bundle {
    val pmb = slave(spinal.lib.bus.simple.PipelinedMemoryBus(PipelinedMemoryBusConfig(32, 32)))
    val wb = master(Wishbone(wbCfg))
  }
  val adapter = new PipelinedMemoryBusToWishbone(wbCfg, PipelinedMemoryBusConfig(32, 32), rspQueue = 1)
  adapter.io.pmb <> io.pmb
  adapter.io.wb <> io.wb
}

class WishboneErrForwardTest extends AnyFunSuite {
  private def wbInit(b: Wishbone): Unit = {
    b.CYC #= false
    b.STB #= false
    b.WE #= false
    b.ADR #= 0
    b.DAT_MOSI #= 0
    if (b.SEL != null) b.SEL #= 0xf
  }

  private def slaveIdle(b: Wishbone): Unit = {
    b.ACK #= false
    b.ERR #= false
    b.DAT_MISO #= 0
  }

  test("WishboneStage m2s+s2m forwards ACK+ERR on read and write") {
    Config.sim.doSim(new WishboneStageErrDut(m2s = true, s2m = true).setDefinitionName("WbStageErr")) { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(5000)
      wbInit(dut.io.host)
      slaveIdle(dut.io.core)
      dut.clockDomain.waitSampling(2)

      def cycle(write: Boolean, ack: Boolean, err: Boolean, data: Long): Unit = {
        dut.io.host.CYC #= true
        dut.io.host.STB #= true
        dut.io.host.WE #= write
        dut.io.host.ADR #= 0x40
        dut.io.host.DAT_MOSI #= 0x11
        var n = 0
        while (!(dut.io.core.CYC.toBoolean && dut.io.core.STB.toBoolean) && n < 40) {
          dut.clockDomain.waitSampling()
          n += 1
        }
        assert(n < 40, "staged STB never reached core")
        dut.io.core.DAT_MISO #= data
        dut.io.core.ACK #= ack
        dut.io.core.ERR #= err
        n = 0
        while (!dut.io.host.ACK.toBoolean && !dut.io.host.ERR.toBoolean && n < 40) {
          dut.clockDomain.waitSampling()
          n += 1
        }
        assert(n < 40, "host cycle never terminated")
        assert(dut.io.host.ERR.toBoolean == err, s"write=$write ERR dropped")
        if (!write) {
          assert((dut.io.host.DAT_MISO.toLong & 0xffffffffL) == (data & 0xffffffffL))
        }
        dut.io.core.ACK #= false
        dut.io.core.ERR #= false
        dut.io.host.CYC #= false
        dut.io.host.STB #= false
        dut.clockDomain.waitSampling(4)
      }

      cycle(write = false, ack = true, err = true, data = 0xdeadbeefL)
      cycle(write = true, ack = true, err = true, data = 0)
      cycle(write = false, ack = false, err = true, data = 0xdeadbeefL)
      cycle(write = false, ack = true, err = false, data = 0xcafecafeL)
    }
  }

  private def pmbToWbErrCycle(dut: PmbToWbErrDut, write: Boolean, data: Long): Unit = {
    dut.io.pmb.cmd.valid #= true
    dut.io.pmb.cmd.write #= write
    dut.io.pmb.cmd.address #= 0x80
    dut.io.pmb.cmd.data #= (if (write) 0x11 else 0)
    dut.io.pmb.cmd.mask #= 0xf
    var n = 0
    while (!(dut.io.wb.CYC.toBoolean && dut.io.wb.STB.toBoolean) && n < 20) {
      dut.clockDomain.waitSampling()
      n += 1
    }
    assert(n < 20, s"WB request never issued write=$write")
    dut.io.wb.DAT_MISO #= data
    dut.io.wb.ACK #= true
    dut.io.wb.ERR #= true
    dut.clockDomain.waitSampling()
    dut.io.pmb.cmd.valid #= false
    dut.io.wb.ACK #= false
    dut.io.wb.ERR #= false
    n = 0
    while (!dut.io.pmb.rsp.valid.toBoolean && n < 20) {
      dut.clockDomain.waitSampling()
      n += 1
    }
    assert(n < 20, s"PMB rsp never valid write=$write")
    assert(dut.io.pmb.rsp.error.toBoolean, s"WB ERR must set rsp.error write=$write")
    if (!write) {
      assert((dut.io.pmb.rsp.data.toLong & 0xffffffffL) == (data & 0xffffffffL))
    }
    dut.clockDomain.waitSampling(2)
  }

  test("PipelinedMemoryBusToWishbone sets rsp.error from WB ERR") {
    Config.sim.doSim(new PmbToWbErrDut().setDefinitionName("PmbWbErr")) { dut =>
      dut.clockDomain.forkStimulus(10)
      SimTimeout(4000)
      dut.io.pmb.cmd.valid #= false
      slaveIdle(dut.io.wb)
      dut.clockDomain.waitSampling(2)

      pmbToWbErrCycle(dut, write = false, data = 0xdeadbeefL)
      pmbToWbErrCycle(dut, write = true, data = 0)
    }
  }
}
