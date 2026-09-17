package spinalextras.lib.soc.spinex.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.{DefaultMapping, SizeMapping}
import spinalextras.lib.bus.{BusError, BusErrorEvent, BusErrorLogger, BusErrorMaster, PmbMissCompleter}
import spinalextras.lib.logging.FlowLogger
import spinalextras.lib.soc.DeviceTree
import spinalextras.lib.soc.spinex.{Spinex, SpinexRegisterFilePlugin}

import scala.language.postfixOps

case class BusErrorPlugin(address: BigInt = BusError.Address,
                          depth: Int = 32,
                          localDepth: Int = 4,
                          irqId: Int = 5) extends SpinexRegisterFilePlugin("bus_error", SizeMapping(address, BusError.Window)) {
  var interruptIdx = irqId
  override def interrupts: Seq[(Int, Int)] = if (depth > 0) Seq((interruptIdx, 0)) else Seq()

  override def compatible: Seq[String] = Seq("spinex,bus-error")

  override def regs: Seq[(String, SizeMapping)] = FlowLogger.csrDtRegs(BusError.Window.toInt)

  override def appendDeviceTree(dt: DeviceTree): Unit = {
    if (depth <= 0) {
      return
    }
    super.appendDeviceTree(dt)
    val names = BusErrorLogger.get().signals.flatMap { s =>
      Option(s._2.getName()).filter(_.nonEmpty)
    }
    if (names.nonEmpty) {
      dt.addEntry(
        "channel-names = " + names.map(n => "\"" + n + "\"").mkString(",\n                     ") + ";",
        baseEntryPath: _*)
    }
  }

  override def apply(som: Spinex): Unit = {
    if (depth > 0) {
      BusError.enableLogging()
    } else {
      BusError.disableLogging()
    }

    val pmbCfg = som.system.pipelinedMemoryBusConfig
    val missD = new PmbMissCompleter(pmbCfg, BusErrorMaster.DBus, eventName = "pmb_decode_miss_d")
    som.add_slave(missD.io.bus, "pmb_decode_miss", DefaultMapping, "dBus")
    val missI = new PmbMissCompleter(pmbCfg, BusErrorMaster.IBus, eventName = "pmb_decode_miss_i")
    som.add_slave(missI.io.bus, "pmb_decode_miss_i", DefaultMapping, true, "iBus")

    if (depth > 0) {
      def ingest(name: String): Flow[BusErrorEvent] = {
        val p = slave(Flow(BusErrorEvent())).setName(name)
        BusError.reportFlow(p)
        p
      }
      if (som.parent != null) {
        som.axiUsbBusErrorIn = ingest("axi_usb_bus_error")
        som.axiMmiBusErrorIn = ingest("axi_mmi_bus_error")
        som.wbBusErrorIn = ingest("wb_decode_miss")
      }

      val irq = False
      irq.setName("bus_error_irq")
      irq.allowOverride()
      BusErrorLogger.create_logger_port(som.interconnect, address, depth, localDepth = localDepth, irq = irq)
      interruptIdx = som.system.addNamedInterrupt("bus_error", irq, irqId)
    }
  }
}
