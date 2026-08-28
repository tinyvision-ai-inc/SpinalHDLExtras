package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinalextras.lib.Config
import spinalextras.lib.blackbox.lattice.lifcl.{ConfigLmmiCommandWriter, NexusMultiBoot, NexusSysConfig}

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

class NexusMultiBootTest extends AnyFunSuite {
  test("refreshCommands matches FPGA-TN-02145 / CR0.SPIM sequence") {
    val cmds = NexusSysConfig.refreshCommands()
    assert(cmds.head == Seq(0x74, 0x00, 0x00, 0x00))
    assert(cmds(1).take(4) == Seq(0x22, 0x01, 0x00, 0x00))
    assert(cmds(1).drop(4) == Seq(0x00, 0x08, 0x00, 0x00, 0xFF, 0xF7, 0xFF, 0xFF))
    assert(cmds(2) == Seq(0x26, 0x00, 0x00, 0x00))
    assert(cmds(3) == Seq(0x79, 0x00, 0x00, 0x00))
  }

  test("ConfigLmmiCommandWriter emits refresh command bytes") {
    val expected = NexusSysConfig.refreshCommands().flatten.map(_ & 0xFF)
    SimConfig.withConfig(Config.spinal).withVerilator.doSim(new ConfigLmmiCommandWriter()) { dut =>
      dut.clockDomain.forkStimulus(25 MHz)
      dut.io.lmmi.cmd.ready #= true
      dut.io.lmmi.rsp.valid #= false
      dut.io.lmmi.rsp.payload #= 0
      dut.io.start #= false
      dut.io.startFw #= false
      dut.io.fwNrd #= 0
      dut.io.fwWr.valid #= false
      dut.io.fwWr.payload #= 0
      dut.io.rd.ready #= true
      dut.clockDomain.waitSampling(4)

      dut.io.start #= true
      dut.clockDomain.waitSampling()
      dut.io.start #= false

      val got = ArrayBuffer[Int]()
      var idle = 0
      while (got.length < expected.length && idle < 200) {
        dut.clockDomain.waitSampling()
        if (dut.io.lmmi.cmd.valid.toBoolean && dut.io.lmmi.cmd.ready.toBoolean) {
          assert(dut.io.lmmi.cmd.write.toBoolean)
          assert(dut.io.lmmi.cmd.offset.toInt == NexusSysConfig.addrCfg)
          got += dut.io.lmmi.cmd.data.toInt & 0xFF
          idle = 0
        } else {
          idle += 1
        }
      }

      assert(got == expected, s"got ${got.map(b => f"$b%02x")} expected ${expected.map(b => f"$b%02x")}")
      dut.clockDomain.waitSampling(8)
      assert(!dut.io.busy.toBoolean)
    }
  }

  test("NexusMultiBoot elaborates") {
    Config.spinal.generateVerilog(new NexusMultiBoot())
  }
}
