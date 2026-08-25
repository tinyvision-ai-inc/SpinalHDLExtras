package spinalextras.lib.soc.spinex.plugins

import spinal.core._
import spinal.lib.bus.misc.SizeMapping
import spinalextras.lib.blackbox.lattice.lifcl.{NexusMultiBoot, NexusSysConfig}
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFileApbPlugin}

import scala.language.postfixOps

/**
 * APB window to select the next Nexus FPGA boot image and trigger LSC_REFRESH.
 *
 * 0x00 bootAddr  RW  SPI flash byte address (MULTIBOOT.MSPIMADDR)
 * 0x04 status    RW  [0] busy (RO), write any value to start
 */
case class NexusMultiBootPlugin(mapping: SizeMapping = SizeMapping(0x2C00, 16 Bytes),
                                oscClk: Bool = null)
  extends SpinexRegisterFileApbPlugin("sysconfig", mapping) {

  override val compatible: Seq[String] = Seq("spinex,sysconfig", "lattice,nexus-multiboot")

  override def apply(som: Spinex): Unit = {
    val boot = new NexusMultiBoot()
    boot.io.oscClk := Option(oscClk).getOrElse(NexusMultiBoot.findOscClk())

    val bootAddr = busCtrl.createReadAndWrite(UInt(32 bits), 0x00, 0, documentation = "bootAddr") init 0
    boot.io.bootAddr := bootAddr

    val start = False
    busCtrl.onWrite(0x04, documentation = "start") {
      start := True
    }
    boot.io.start := start
    busCtrl.read(boot.io.busy, 0x04, bitOffset = 0, documentation = "busy")

    super.apply(som)
  }

  override def appendDeviceTree(dt: DeviceTree): Unit = {
    super.appendDeviceTree(dt)
    dt.addEntry(s"lmmi-max-frequency = <${NexusSysConfig.lmmiFmax.toInt}>;", baseEntryPath: _*)
    dt.addEntry("lmmi-offset = <0x01>;", baseEntryPath: _*)
  }
}
