package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}

class PmbMissCompleter(config: PipelinedMemoryBusConfig,
                       masterId: Int,
                       pendingMax: Int = 4,
                       eventName: String = "pmb_decode_miss") extends ComponentWithFormalProperties {
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(config))
    val event = master(Flow(BusErrorEvent()))
  }

  val waitRsp = RegInit(False)
  val wrErr = RegNext(io.bus.cmd.fire && io.bus.cmd.write) init False
  io.bus.cmd.ready := !waitRsp || io.bus.cmd.write
  when(io.bus.cmd.fire && !io.bus.cmd.write) {
    waitRsp := True
  } elsewhen (waitRsp) {
    waitRsp := False
  }

  io.bus.rsp.valid := waitRsp || wrErr
  io.bus.rsp.data := BusErrorSentinel.DECERR
  io.bus.rsp.error := waitRsp || wrErr

  val ev = BusErrorEvent()
  ev.assign(io.bus.cmd.address, io.bus.cmd.write, masterId, BusErrorCause.DECERR, BusErrorSentinel.DECERR)
  io.event.valid := io.bus.cmd.fire
  io.event.payload := ev
  io.event.setName(eventName)
  BusError.reportFlow(io.event)

  override protected def formalProperties() = new FormalProperties(this) {
    addFormalProperty(!(waitRsp && io.bus.cmd.fire && !io.bus.cmd.write),
      "miss completer holds one outstanding read")
  }
}

object PmbMissCompleter {
  def apply(bus: PipelinedMemoryBus, masterId: Int, eventName: String = "pmb_decode_miss"): PmbMissCompleter = {
    val c = new PmbMissCompleter(bus.config, masterId, eventName = eventName)
    c.io.bus <> bus
    c
  }
}
