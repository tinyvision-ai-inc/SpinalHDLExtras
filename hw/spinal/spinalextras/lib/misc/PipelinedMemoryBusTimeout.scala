package spinalextras.lib.misc

import spinal.core._
import spinal.lib.bus.simple._
import spinal.lib._
import spinalextras.lib.bus.{BusError, BusErrorCause, BusErrorEvent, BusErrorMaster, BusErrorSentinel}

case class PipelinedMemoryBusTimeout(config : PipelinedMemoryBusConfig, timeout : TimeNumber = 100 us) extends Component {
  val io = new Bundle {
    val pmb_m = master(PipelinedMemoryBus(config))
    val pmb_s = slave(PipelinedMemoryBus(config))
  }

  val timeout_counter = Timeout(timeout)
  val needsRdyCond = io.pmb_s.cmd.valid
  val needsRdy = RegNextWhen(True, needsRdyCond) clearWhen(io.pmb_s.cmd.fire) init(False)
  val needsRespCond = io.pmb_s.cmd.fire && !io.pmb_s.cmd.write
  val needsResp = RegNextWhen(True, needsRespCond) clearWhen(io.pmb_s.rsp.valid) init(False)
  when(needsRespCond || needsRdyCond.rise(False)) {
    timeout_counter.clear()
  }

  val timedOutRsp = timeout_counter && needsResp
  val forceCmd = RegInit(False)
  when(timeout_counter && io.pmb_s.cmd.valid && !io.pmb_m.cmd.ready) {
    forceCmd := True
  }
  when(io.pmb_s.cmd.fire) {
    forceCmd := False
  }

  io.pmb_m.cmd.valid := io.pmb_s.cmd.valid && !forceCmd
  io.pmb_m.cmd.payload := io.pmb_s.cmd.payload
  io.pmb_s.cmd.ready := forceCmd || io.pmb_m.cmd.ready

  val lastAddr = RegNextWhen(io.pmb_s.cmd.address, io.pmb_s.cmd.fire)
  val lastWrite = RegNextWhen(io.pmb_s.cmd.write, io.pmb_s.cmd.fire)
  val rspHold = RegInit(False)
  val rspHoldData = Reg(Bits(config.dataWidth bits)) init 0
  val rspHoldIsCmd = RegInit(False)
  when(io.pmb_s.cmd.fire && forceCmd && !io.pmb_s.cmd.write) {
    rspHold := True
    rspHoldData := BusErrorSentinel.TIMEOUT_CMD.resized
    rspHoldIsCmd := True
  } elsewhen (timedOutRsp && !rspHold) {
    rspHold := True
    rspHoldData := BusErrorSentinel.TIMEOUT_RSP.resized
    rspHoldIsCmd := False
  } elsewhen (rspHold) {
    rspHold := False
  }

  io.pmb_s.rsp.valid := rspHold || io.pmb_m.rsp.valid
  io.pmb_s.rsp.data := Mux(rspHold, rspHoldData, io.pmb_m.rsp.data)

  val timeoutEvent = Flow(BusErrorEvent())
  timeoutEvent.setName("dbus_timeout")
  timeoutEvent.valid := rspHold
  timeoutEvent.payload.assignDontCare()
  when(rspHold) {
    when(rspHoldIsCmd) {
      timeoutEvent.payload.assign(lastAddr, lastWrite, BusErrorMaster.DBus, BusErrorCause.TIMEOUT, BusErrorSentinel.TIMEOUT_CMD)
    } otherwise {
      timeoutEvent.payload.assign(lastAddr, False, BusErrorMaster.DBus, BusErrorCause.TIMEOUT, BusErrorSentinel.TIMEOUT_RSP)
    }
  }
  BusError.reportFlow(timeoutEvent)
}
object PipelinedMemoryBusTimeout {
  def apply(bus : PipelinedMemoryBus) : PipelinedMemoryBus = {
    val dut = PipelinedMemoryBusTimeout(bus.config)
    dut.io.pmb_m <> bus
    dut.io.pmb_s
  }
}

case class PipelinedMemoryBusBuffered(cfg : PipelinedMemoryBusConfig, inFlightMax : Int) extends Component {
  val io = new Bundle {
    val config = cfg
    val cmd = slave(Stream(PipelinedMemoryBusCmd(config)))
    val rsp = master(Stream(PipelinedMemoryBusRsp(config)))

    val bus = master(PipelinedMemoryBus(config))

  }
//  val fifo = StreamFifo(io.rsp.payload, inFlightMax)
//  fifo.io.push.payload := io.rsp.payload
//  fifo.io.push.valid := io.rsp.valid
//  assert(~fifo.io.push.isStall, "Bus Buffered Stalled")
//  fifo.io.pop <> io.rsp

  val overflow = Bool()
  io.rsp <> io.bus.rsp.toStream(overflow)
  assert(~overflow, "Bus Buffered Stalled")

  val inFlight = new CounterUpDown(inFlightMax, handleOverflow = false)
  when(io.bus.cmd.fire) { inFlight.increment() }
  when(io.bus.rsp.fire) { inFlight.decrement() }

  io.cmd.takeWhen(io.rsp.ready && ~inFlight.willOverflowIfInc) >> io.bus.cmd
}

object PipelinedMemoryBusBuffered {
  def apply(bus: PipelinedMemoryBus, rspQueue : Int) = {
    val dut = new PipelinedMemoryBusBuffered(bus.config, rspQueue)
    dut.io.bus <> bus
    dut.io
  }

}