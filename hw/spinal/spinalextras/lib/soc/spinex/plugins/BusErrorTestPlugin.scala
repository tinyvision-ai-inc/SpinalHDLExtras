package spinalextras.lib.soc.spinex.plugins

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinalextras.lib.bus._
import spinalextras.lib.misc.PipelinedMemoryBusTimeout
import spinalextras.lib.soc.spinex.{Spinex, SpinexPlugin}

import scala.language.postfixOps

/** Stimulus windows for the RISC-V bus-error sample. Not production. */
class BusErrorTestPlugin(hangTimeout: TimeNumber = 1 us) extends SpinexPlugin {
  /** PASS/FAIL @ 0x41000000 on direct dBus (peripheral tohost was a decode miss). */
  var host: UInt = null
  var tohostWrCount: UInt = null
  var unimplFireCount: UInt = null

  override def apply(som: Spinex): Unit = {
    val cfg = som.system.pipelinedMemoryBusConfig

    val tohostBus = PipelinedMemoryBus(cfg).setName("buserr_tohost")
    som.add_slave(tohostBus, "buserr_tohost", SizeMapping(0x41000000L, 0x10), true, "dBus")
    host = Reg(UInt(32 bits)) init 0
    host.setName("buserr_tohost")
    tohostWrCount = Reg(UInt(8 bits)) init 0
    tohostWrCount.setName("buserr_tohost_wrcnt")
    tohostBus.cmd.ready := True
    when(tohostBus.cmd.fire && tohostBus.cmd.write) {
      host := tohostBus.cmd.data.asUInt
      tohostWrCount := tohostWrCount + 1
    }
    val tohostRd = RegNext(tohostBus.cmd.fire && !tohostBus.cmd.write) init False
    tohostBus.rsp.valid := tohostRd
    tohostBus.rsp.data := host.asBits
    tohostBus.rsp.error := False

    /* Hole window: every offset faults (UNIMPL load/store). */
    val unimplBus = PipelinedMemoryBus(cfg).setName("buserr_unimpl")
    som.add_slave(unimplBus, "buserr_unimpl", SizeMapping(0xe0008500L, 0x100), "dBus")
    unimplFireCount = Reg(UInt(16 bits)) init 0
    unimplFireCount.setName("buserr_unimpl_fire")
    unimplBus.cmd.ready := True
    when(unimplBus.cmd.fire) {
      unimplFireCount := unimplFireCount + 1
    }
    val rdFault = RegNext(unimplBus.cmd.fire && !unimplBus.cmd.write) init False
    val wrFault = RegNext(unimplBus.cmd.fire && unimplBus.cmd.write) init False
    unimplBus.rsp.valid := rdFault || wrFault
    unimplBus.rsp.data := B(0xA5A5A5A5L, 32 bits)
    unimplBus.rsp.error := True

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
