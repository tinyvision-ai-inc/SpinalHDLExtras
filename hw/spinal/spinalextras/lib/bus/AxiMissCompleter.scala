package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.misc.SizeMapping
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties}

/** Finish AXI bursts that hit no mapped slave. RDATA is tiled DECERR, never X / 0. */
class AxiMissCompleter(axiConfig: Axi4Config) extends ComponentWithFormalProperties {
  val io = new Bundle {
    val axi = slave(Axi4(axiConfig))
  }

  val consumeData = RegInit(False)
  val sendWriteRsp = RegInit(False)
  val sendReadRsp = RegInit(False)
  val id = if (axiConfig.useId) Reg(UInt(axiConfig.idWidth bits)) else null
  val remaining = Reg(UInt(8 bits))
  val remainingZero = remaining === 0

  io.axi.aw.ready := !(consumeData || sendWriteRsp || sendReadRsp)
  io.axi.ar.ready := !(consumeData || sendWriteRsp || sendReadRsp) && !io.axi.aw.valid

  when(io.axi.aw.fire) {
    consumeData := True
    if (id != null) id := io.axi.aw.id
  }
  when(io.axi.ar.fire) {
    sendReadRsp := True
    remaining := (if (axiConfig.useLen) io.axi.ar.len else U(0))
    if (id != null) id := io.axi.ar.id
  }

  io.axi.w.ready := consumeData
  when(io.axi.w.fire && io.axi.w.last) {
    consumeData := False
    sendWriteRsp := True
  }

  io.axi.b.valid := sendWriteRsp
  if (axiConfig.useResp) io.axi.b.setDECERR
  if (axiConfig.useId) io.axi.b.id := id
  when(io.axi.b.fire) {
    sendWriteRsp := False
  }

  io.axi.r.valid := sendReadRsp
  io.axi.r.data := BusErrorSentinel.tile(axiConfig.dataWidth, BusErrorSentinel.DECERR)
  if (axiConfig.useId) io.axi.r.id := id
  if (axiConfig.useResp) io.axi.r.setDECERR
  if (axiConfig.useLast) io.axi.r.last := remainingZero
  when(sendReadRsp) {
    when(io.axi.r.ready) {
      remaining := remaining - 1
      when(remainingZero) {
        sendReadRsp := False
      }
    }
  }

  override protected def formalProperties() = new FormalProperties(this) {
    addFormalProperty(!(sendReadRsp && sendWriteRsp), "AXI miss completer one response path")
  }
}

/** 1×2 decode after a 2×1 AXI arbiter: LRAM vs miss. Avoids a 2×2 64-bit xbar. */
class AxiRamOrMiss(axiConfig: Axi4Config, ramBytes: BigInt, totalBytes: BigInt) extends Component {
  val io = new Bundle {
    val bus = slave(Axi4(axiConfig))
    val ram = master(Axi4(axiConfig))
  }
  val miss = new AxiMissCompleter(axiConfig)
  val xbar = Axi4CrossbarFactory()
  xbar.addSlaves(
    io.ram -> SizeMapping(0x0L, ramBytes),
    miss.io.axi -> SizeMapping(ramBytes, totalBytes - ramBytes)
  )
  xbar.addConnections(io.bus -> Seq(io.ram, miss.io.axi))
  xbar.build()
}
