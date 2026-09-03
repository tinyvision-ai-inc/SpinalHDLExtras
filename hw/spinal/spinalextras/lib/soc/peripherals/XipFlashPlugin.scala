package spinalextras.lib.soc.peripherals

import spinal.core._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3CC}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.com.spi.ddr.{Apb3SpiXdrMasterCtrl, SpiXdrMasterCtrl, SpiXdrParameter}
import spinal.lib.com.spi.ddr.SpiXdrMasterCtrl.{MemoryMappingParameters, XipBus}
import spinalextras.lib.Constraints
import spinalextras.lib.io.TristateBuffer
import spinalextras.lib.misc.GlobalSignals
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFilePlugin}

import scala.language.postfixOps

object XipFlashPlugin {
  /** APB CSRs (byte offsets from flashctrl base). Last used address is 0x58. */
  val csrWindowSize = 0x5c
  /** NOR program page / sector; consumed by Zephyr `soc-nv-flash`. */
  val writeBlockSize = 32
  val eraseBlockSize = 0x1000
  val registers: Seq[(String, Int)] = Seq(
    ("data", 0x00),
    ("buffer", 0x04),
    ("config", 0x08),
    ("interrupt", 0x0C),
    ("clk_divider", 0x20),
    ("ss_setup", 0x24),
    ("ss_hold", 0x28),
    ("ss_disable", 0x2C),
    ("ss_active", 0x30),
    ("xip_enable", 0x40),
    ("xip_instr", 0x44),
    ("xip_mod", 0x48),
    ("xip_offset", XipOffset.csrAddr),
    ("data32", 0x50),
    ("data32_rw", 0x54),
    ("data32_rsp", 0x58)
  )

  val defaultConfig = SpiXdrMasterCtrl.MemoryMappingParameters(
    SpiXdrMasterCtrl.Parameters(8, 12, SpiXdrParameter(
        dataWidth = 4,
        ioRate = 1,
        ssWidth = 1))
      .addFullDuplex(id = 0, rate = 1, ddr = false)
      .addHalfDuplex(id = 1, rate = 1, ddr = false, spiWidth = 4, lateSampling = false),
    cmdFifoDepth = 32,
    rspFifoDepth = 32,
    xipEnableInit = true,

    modInit = 0,
    xipInstructionModInit = 0,
    xipAddressModInit  = 1,
    xipDummyModInit  = 1,
    xipPayloadModInit  = 1,
    //xipInstructionDataInit = 0x6B,
    xipInstructionDataInit = 0xeb,

    xipDummyDataInit = 0xFF,
    xipDummyCountInit = 2,
    //xipConfigWritable = false,

    xip = SpiXdrMasterCtrl.XipBusParameters(addressWidth = 24, lengthWidth = 5)
  )
}
case class XipFlashPlugin(config: MemoryMappingParameters = XipFlashPlugin.defaultConfig,
                          memoryMapping : SizeMapping = SizeMapping(0x20000000L, 0x01000000),
                          registerMapping : SizeMapping = SizeMapping(0x01000, 1 KiB),
                          var clockDomain : ClockDomain = ClockDomain.current,
                          name : String = "spiflash",
                          xipLinkBase : BigInt = XipOffset.linkBase,
                          xipResetOffset : BigInt = XipOffset.resetOffset,
                          xipFlashSize : BigInt = XipOffset.flashSize)
  extends SpinexRegisterFilePlugin("/soc/flashctrl",
    SizeMapping(0xe0000000L + registerMapping.base, XipFlashPlugin.csrWindowSize)) {

  override val compatible: Seq[String] = Seq("tinyvision,flash")

  /** Zephyr SoC node: label `flashctrl`, unit name `flash-controller@…`. */
  override def entryName: String =
    s"flashctrl: flash-controller@${regBase.toString(16)}"

  override def regs: Seq[(String, SizeMapping)] =
    ("base" -> SizeMapping(0, XipFlashPlugin.csrWindowSize)) +:
      XipFlashPlugin.registers.map { case (n, off) => n -> SizeMapping(off, 4) }

  override def appendDeviceTree(dt: DeviceTree): Unit = {
    super.appendDeviceTree(dt)

    val xipPath = baseEntryPath :+ s"flash0: flash@${memoryMapping.base.toString(16)}"
    dt.addEntry("""compatible = "soc-nv-flash";""", xipPath: _*)
    dt.addEntry("#address-cells = <1>;", xipPath: _*)
    dt.addEntry("#size-cells = <0>;", xipPath: _*)
    dt.addEntry(s"reg = <0x${memoryMapping.base.toString(16)} 0x${memoryMapping.size.toString(16)}>;", xipPath: _*)
    dt.addEntry(s"write-block-size = <${XipFlashPlugin.writeBlockSize}>;", xipPath: _*)
    dt.addEntry(s"erase-block-size = <0x${XipFlashPlugin.eraseBlockSize.toHexString}>;", xipPath: _*)
  }

  lazy val spiflash_clk = out(Bool())
  lazy val spiflash_cs_n = out(Bool())
  lazy val spiflash_dq = inout(Analog(Bits(config.ctrl.spi.dataWidth bits)))

  def export_signals() = {
    spiflash_cs_n <> out(Bool()).setName("spiflash_cs_n")
    spiflash_clk <> out(Bool()).setName("spiflash_clk")
    spiflash_dq <> inout(Analog(Bits(4 bits)).setName("spiflash_dq"))
  }

  override def apply(som: Spinex): Unit = {
    val systemClockDomain = ClockDomain.current

    som.io.valCallbackRec(spiflash_clk, s"${name}_clk")
    som.io.valCallbackRec(spiflash_cs_n, s"${name}_cs_n")
    som.io.valCallbackRec(spiflash_dq, s"${name}_dq")
    if (clockDomain == null) {
      clockDomain = ClockDomain.current
    }

    val clockArea = new ClockingArea(if (clockDomain == null) ClockDomain.current else clockDomain) {
      val ctrl = Apb3SpiXdrMasterCtrl(config)

      val buffers = ctrl.io.spi.data.map(_ => TristateBuffer())

      for (i <- spiflash_dq.bitsRange) {
        val (phy, tristate, xdr) = (spiflash_dq(i), buffers(i), ctrl.io.spi.data(i))
        tristate.io.output_enable := xdr.writeEnable
        tristate.io.input := xdr.write(0)
        xdr.read(0) := RegNext(RegNext(tristate.io.output))
        tristate.io.phy <> phy
      }

      spiflash_clk := ctrl.io.spi.sclk.write(0)
      spiflash_cs_n := ctrl.io.spi.ss(0)

    }
    val xip = if (systemClockDomain == clockArea.clockDomain) clockArea.ctrl.io.xip else {
      val flashClockDomain = clockArea.clockDomain
      val ref_xip = clockArea.ctrl.io.xip
      val cc_xip = new XipBus(ref_xip.p)
      cc_xip.cmd.queue(4, systemClockDomain, flashClockDomain) >> ref_xip.cmd
      ref_xip.rsp.queue(4, flashClockDomain, systemClockDomain) >> cc_xip.rsp
      cc_xip
    }

    val pending = Reg(UInt(5 bits)) init 0
    val pendingNext = pending +^ xip.cmd.fire.asUInt -^ xip.rsp.lastFire.asUInt
    pending := pendingNext.resized
    val xipIdle = pending === 0 && !xip.cmd.valid

    val cpuApb = Apb3(Apb3SpiXdrMasterCtrl.getApb3Config)
    val offsetApb = new XipOffsetApb(xipResetOffset)
    offsetApb.io.up <> cpuApb
    offsetApb.io.idle := xipIdle
    if (systemClockDomain == clockArea.clockDomain) {
      offsetApb.io.down <> clockArea.ctrl.io.apb
    } else {
      val cc = Apb3CC(cpuApb.config, systemClockDomain, clockArea.clockDomain)
      cc.io.input <> offsetApb.io.down
      cc.io.output <> clockArea.ctrl.io.apb
    }
    som.add_peripheral(cpuApb, registerMapping)
    XipOffsetContext.bind(offsetApb.io.offset, xipLinkBase, xipFlashSize)

    som.add_slave(xip, "xip", memoryMapping, "iBus", "dBus")

    // sclkToggleInit=0: SCK toggles every controller cycle, so the pin is sysclk/2.
    Constraints.create_generated_clock(
      spiflash_clk,
      clockDomain.readClockWire,
      clockDomain.frequency.getValue / 2,
      clockDomain.frequency.getValue
    )
    Constraints.set_max_skew(1 ns, spiflash_clk, spiflash_cs_n, spiflash_dq)
  }
}
