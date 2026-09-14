package spinalextras.lib.soc.spinex.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.AccessType.RW
import spinal.lib.bus.regif.ClassName
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinalextras.lib.bus._
import spinalextras.lib.bus.simple.PipelinedMemoryBusInterface
import spinalextras.lib.misc.PipelinedMemoryBusTimeout
import spinalextras.lib.soc.spinex.{Spinex, SpinexPlugin}

import scala.language.postfixOps

/** Stimulus windows for the RISC-V bus-error sample. Not production. */
class BusErrorTestPlugin(hangTimeout: TimeNumber = 1 us,
                          tohost: UInt = null) extends SpinexPlugin {
  override def apply(som: Spinex): Unit = {
    val cfg = som.system.pipelinedMemoryBusConfig

    val host = Reg(UInt(32 bits)) init 0
    if (tohost != null) {
      tohost := host
    }
    val hostFact = som.interconnect.add_slave_factory("buserr_tohost", SizeMapping(0xe0008900L, 16), false, false, "dBus")
    hostFact.write(host, 0xe0008900L)

    val unimplBus = PipelinedMemoryBus(cfg).setName("buserr_unimpl")
    som.add_slave(unimplBus, "buserr_unimpl", SizeMapping(0xe0008500L, 0x100), "dBus")
    implicit val moduleName: ClassName = ClassName("buserr_unimpl")
    val bif = PipelinedMemoryBusInterface(unimplBus, SizeMapping(0xe0008500L, 0x100))
    val keep = bif.newRegAt(0xe0008500L, "keep")
    keep.field(UInt(32 bits), RW)

    val slverr = new PmbErrorSlave(cfg, 2, 0x5A5A5A5AL, "pmb_slverr")
    som.add_slave(slverr.io.bus, "buserr_slverr", SizeMapping(0xe0008700L, 0x100), "dBus")

    val proto = new PmbErrorSlave(cfg, 5, 0xDEADBEEFL, "pmb_proto")
    som.add_slave(proto.io.bus, "buserr_proto", SizeMapping(0xe0008800L, 0x100), "dBus")

    val hang = new PmbHangSlave(cfg)
    val tmo = new PipelinedMemoryBusTimeout(cfg, hangTimeout)
    tmo.io.pmb_m <> hang.io.bus
    som.add_slave(tmo.io.pmb_s, "buserr_timeout", SizeMapping(0xe0008600L, 0x100), "dBus")
  }
}

class PmbHangSlave(config: PipelinedMemoryBusConfig) extends Component {
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(config))
  }
  io.bus.cmd.ready := False
  io.bus.rsp.valid := False
  io.bus.rsp.data.assignDontCare()
  io.bus.rsp.error := False
}

class PmbErrorSlave(config: PipelinedMemoryBusConfig, cause: Int, sentinel: BigInt, eventName: String) extends Component {
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(config))
  }
  val waitRsp = RegInit(False)
  val wrErr = RegNext(io.bus.cmd.fire && io.bus.cmd.write) init False
  val dat = B(sentinel, 32 bits)
  io.bus.cmd.ready := !waitRsp || io.bus.cmd.write
  when(io.bus.cmd.fire && !io.bus.cmd.write) {
    waitRsp := True
  } elsewhen (waitRsp) {
    waitRsp := False
  }
  io.bus.rsp.valid := waitRsp || wrErr
  io.bus.rsp.data := dat
  io.bus.rsp.error := waitRsp || wrErr

  val ev = Flow(BusErrorEvent())
  ev.setName(eventName)
  val e = BusErrorEvent()
  e.assign(io.bus.cmd.address, io.bus.cmd.write, BusErrorMaster.DBus, U(cause, BusErrorCause.width bits), dat)
  ev.valid := io.bus.cmd.fire
  ev.payload := e
  BusError.reportFlow(ev)
}
