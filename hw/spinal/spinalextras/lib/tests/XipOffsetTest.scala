package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.com.spi.ddr.Apb3SpiXdrMasterCtrl
import spinalextras.lib.Config
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.peripherals.{XipFlashPlugin, XipOffset, XipOffsetApb, XipOffsetRemapDut}

import scala.language.postfixOps

class XipOffsetApbDut extends Component {
  val io = new Bundle {
    val apb = slave(Apb3(Apb3SpiXdrMasterCtrl.getApb3Config))
    val idle = in Bool()
    val offset = out UInt (32 bits)
    val downSel = out Bool()
  }
  val ctrl = new XipOffsetApb()
  ctrl.io.up <> io.apb
  ctrl.io.idle := io.idle
  io.offset := ctrl.io.offset
  ctrl.io.down.PREADY := True
  ctrl.io.down.PRDATA := B(0xA5A5A5A5L, 32 bits)
  io.downSel := ctrl.io.down.PSEL.orR
}

class XipOffsetTest extends AnyFunSuite {
  def sim = SimConfig.withConfig(Config.spinal).withVerilator.workspacePath("simulations/")
  def apbIdle(dut: XipOffsetApbDut): Unit = {
    dut.io.apb.PSEL #= 0
    dut.io.apb.PENABLE #= false
    dut.io.apb.PWRITE #= false
    dut.io.apb.PADDR #= 0
    dut.io.apb.PWDATA #= 0
  }

  def apbAccess(dut: XipOffsetApbDut, addr: Int, write: Boolean, data: BigInt = 0): BigInt = {
    dut.io.apb.PSEL #= 1
    dut.io.apb.PENABLE #= false
    dut.io.apb.PWRITE #= write
    dut.io.apb.PADDR #= addr
    dut.io.apb.PWDATA #= data
    dut.clockDomain.waitSampling()
    dut.io.apb.PENABLE #= true
    dut.clockDomain.waitSampling()
    while (!dut.io.apb.PREADY.toBoolean) {
      dut.clockDomain.waitSampling()
    }
    val prdata = dut.io.apb.PRDATA.toBigInt
    val downSel = dut.io.downSel.toBoolean
    if (addr == XipOffset.csrAddr) {
      assert(!downSel, "offset CSR must not be forwarded to the SPI controller")
    } else {
      assert(downSel, "non-offset APB accesses must reach the SPI controller")
    }
    dut.clockDomain.waitSampling()
    apbIdle(dut)
    dut.clockDomain.waitSampling()
    prdata
  }

  test("remap reset offset is identity with 0x20200000 link") {
    sim.doSim(new XipOffsetRemapDut().setDefinitionName("XipOffsetRemapIdentity")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.offset #= XipOffset.resetOffset
      dut.io.byteLast #= 3

      dut.io.cpuAddr #= 0x20200000L
      dut.clockDomain.waitSampling()
      assert(dut.io.nor.toLong == 0x200000)
      assert(!dut.io.overflow.toBoolean)

      dut.io.cpuAddr #= 0x20000000L
      dut.clockDomain.waitSampling()
      assert(dut.io.nor.toLong == 0)
      assert(!dut.io.overflow.toBoolean)

      dut.io.cpuAddr #= 0x20200000L
      dut.io.offset #= 0x300000
      dut.clockDomain.waitSampling()
      assert(dut.io.nor.toLong == 0x300000)
      assert(!dut.io.overflow.toBoolean)
    }
  }

  test("remap overflow at 16MB and underflow below link base") {
    sim.doSim(new XipOffsetRemapDut().setDefinitionName("XipOffsetRemapOverflow")) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)

      // last byte of window, 4-byte beat — still inside
      dut.io.offset #= XipOffset.resetOffset
      dut.io.cpuAddr #= 0x20FFFFFCL
      dut.io.byteLast #= 3
      dut.clockDomain.waitSampling()
      assert(dut.io.nor.toLong == 0xFFFFFC)
      assert(!dut.io.overflow.toBoolean)

      // 8-byte beat crosses 16 MiB
      dut.io.byteLast #= 7
      dut.clockDomain.waitSampling()
      assert(dut.io.overflow.toBoolean)

      // NOR base at 16 MiB
      dut.io.cpuAddr #= 0x20200000L
      dut.io.offset #= 0x1000000
      dut.io.byteLast #= 0
      dut.clockDomain.waitSampling()
      assert(dut.io.overflow.toBoolean)

      // cpu below link base with offset 0
      dut.io.offset #= 0
      dut.io.cpuAddr #= 0x20000000L
      dut.io.byteLast #= 3
      dut.clockDomain.waitSampling()
      assert(dut.io.overflow.toBoolean)
    }
  }

  test("APB 0x4C offset CSR: reset, align, idle gate, pass-through") {
    sim.doSim(new XipOffsetApbDut().setDefinitionName("XipOffsetApbCsr")) { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.idle #= true
      apbIdle(dut)
      dut.clockDomain.waitRisingEdge(40)

      assert(dut.io.offset.toLong == XipOffset.resetOffset)
      val resetRead = apbAccess(dut, XipOffset.csrAddr, write = false)
      assert(resetRead == XipOffset.resetOffset)

      apbAccess(dut, XipOffset.csrAddr, write = true, 0x300000)
      assert(dut.io.offset.toLong == 0x300000)
      assert(apbAccess(dut, XipOffset.csrAddr, write = false) == 0x300000)

      // [15:0] tied off
      apbAccess(dut, XipOffset.csrAddr, write = true, 0x300001)
      assert(dut.io.offset.toLong == 0x300000)

      dut.io.idle #= false
      apbAccess(dut, XipOffset.csrAddr, write = true, 0x400000)
      assert(dut.io.offset.toLong == 0x300000)

      dut.io.idle #= true
      // other APB addresses are forwarded (dummy down selected)
      apbAccess(dut, 0x40, write = true, 1)
    }
  }
}

class XipFlashDtsTest extends AnyFunSuite {
  test("flash DT matches Zephyr SoC node shape") {
    val plugin = XipFlashPlugin(clockDomain = null)
    assert(plugin.entryName == "flashctrl: flash-controller@e0001000")
    assert(plugin.regs.head == ("base", spinal.lib.bus.misc.SizeMapping(0, XipFlashPlugin.csrWindowSize)))

    val dt = new DeviceTree()
    plugin.appendDeviceTree(dt)
    val text = dt.root.str()
    assert(text.contains("flashctrl: flash-controller@e0001000"))
    assert(text.contains("0xe0001000 0x5c"))
    assert(text.contains("\"base\""))
    assert(text.contains("flash0: flash@20000000"))
    assert(text.contains("write-block-size = <32>;"))
    assert(text.contains("erase-block-size = <0x1000>;"))
    assert(!text.contains("tinyvision,xip-"))
  }
}
