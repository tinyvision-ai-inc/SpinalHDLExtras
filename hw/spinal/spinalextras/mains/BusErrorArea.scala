package spinalextras.mains

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinalextras.lib.Config
import spinalextras.lib.bus.{BusError, BusErrorEvent, BusErrorLogger, BusErrorMaster, PmbMissCompleter}
import spinalextras.lib.misc.PipelinedMemoryBusTimeout
import spinalextras.lib.soc.spinex.plugins.PmbHangSlave

import scala.language.postfixOps

/** Infra-only logger: miss d/i, timeout, two dBus holes merged to dbus_unimpl. */
class BusErrorAreaDut(depth: Int = 32) extends Component {
  BusError.enableLogging()
  val cfg = PipelinedMemoryBusConfig(32, 32)
  val io = new Bundle {
    val dBus = slave(PipelinedMemoryBus(cfg))
    val iBus = slave(PipelinedMemoryBus(cfg))
    val tmo = slave(PipelinedMemoryBus(cfg))
    val holeA = slave(Flow(BusErrorEvent()))
    val holeB = slave(Flow(BusErrorEvent()))
    val log = master(Stream(Bits(95 bits)))
    val irq = out Bool()
  }
  io.holeA.setName("hole_a")
  io.holeB.setName("hole_b")
  BusError.unimplTap(io.holeA, BusErrorMaster.DBus)
  BusError.unimplTap(io.holeB, BusErrorMaster.DBus)
  val missD = new PmbMissCompleter(cfg, BusErrorMaster.DBus, eventName = "pmb_decode_miss_d")
  missD.io.bus <> io.dBus
  val missI = new PmbMissCompleter(cfg, BusErrorMaster.IBus, eventName = "pmb_decode_miss_i")
  missI.io.bus <> io.iBus
  val hang = new PmbHangSlave(cfg)
  val tmo = new PipelinedMemoryBusTimeout(cfg, 100 us)
  tmo.io.pmb_m <> hang.io.bus
  tmo.io.pmb_s <> io.tmo
  val irq = False
  irq.setName("bus_error_irq")
  irq.allowOverride()
  val inFlow = Flow(Bits(32 bits))
  inFlow.setIdle()
  BusErrorLogger.build(null, 0, depth, "BusErrorLogger", Some((io.log, inFlow)), localDepth = 2, irq = irq)
  io.irq := irq
}

class BusErrorAreaOffDut extends Component {
  BusError.disableLogging()
  val cfg = PipelinedMemoryBusConfig(32, 32)
  val io = new Bundle {
    val dBus = slave(PipelinedMemoryBus(cfg))
    val iBus = slave(PipelinedMemoryBus(cfg))
    val tmo = slave(PipelinedMemoryBus(cfg))
  }
  val missD = new PmbMissCompleter(cfg, BusErrorMaster.DBus, eventName = "pmb_decode_miss_d")
  missD.io.bus <> io.dBus
  val missI = new PmbMissCompleter(cfg, BusErrorMaster.IBus, eventName = "pmb_decode_miss_i")
  missI.io.bus <> io.iBus
  val hang = new PmbHangSlave(cfg)
  val tmo = new PipelinedMemoryBusTimeout(cfg, 100 us)
  tmo.io.pmb_m <> hang.io.bus
  tmo.io.pmb_s <> io.tmo
}

object BusErrorArea {
  def main(args: Array[String]): Unit = {
    val depth = args.find(_ != "off").map(_.toInt).getOrElse(32)
    val cfg = Config.spinal.copy(targetDirectory = "hw/gen/BusErrorArea")
    if (args.contains("off")) {
      cfg.generateVerilog(new BusErrorAreaOffDut().setDefinitionName("BusErrorAreaOffDut"))
    } else {
      cfg.generateVerilog(new BusErrorAreaDut(depth = depth).setDefinitionName("BusErrorAreaDut"))
      cfg.generateVerilog(new BusErrorAreaOffDut().setDefinitionName("BusErrorAreaOffDut"))
    }
  }
}
