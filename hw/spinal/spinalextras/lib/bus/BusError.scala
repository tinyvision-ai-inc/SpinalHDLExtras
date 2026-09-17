package spinalextras.lib.bus

import spinal.core._
import spinal.lib._
import spinalextras.lib.bus.general.BusSlaveProvider
import spinalextras.lib.logging.{FlowLogger, FlowLoggerConfig, GlobalLogger}

import scala.collection.mutable.ArrayBuffer

/**
 * Bus error log: same collector as EventLogger ([[GlobalLogger]] / [[FlowLogger]]),
 * but a dedicated instance so EventLogger at 0xe0006000 is not starved.
 *
 * '''Hookup is automatic on each bus, not each block.''' Decode miss, timeout,
 * and BusIf holes call [[BusError.reportFlow]] / [[BusError.unimplTap]].
 * UNIMPL from every BusIf on a bus merges to one channel (`dbus_unimpl`).
 * The plugin only calls [[BusErrorLogger.create_logger_port]] after elaboration.
 *
 * Peripheral UNIMPL, SLVERR, and TIMEOUT also complete the CPU cycle with
 * `rsp.error` / WB `ERR` (load access fault `mcause` 5, store `mcause` 7).
 * On-chip RAM does not: those accesses stay posted and never set `error`.
 *
 * CSR map is the EventLogger map (ctrl, captured, `readStreamNonBlocking`,
 * occupancy, dropped, per-channel counts). FlowLogger stamps time; the event
 * payload does not.
 *
 * '''cause''' values (3 bits). AXI RESP is only 2 bits; UNIMPL/TIMEOUT/PROTO
 * travel on AXI as SLVERR or DECERR (see [[BusErrorCause.axiResp]]).
 *
 *  - '''DECERR''' -- no slave decoded the address (AXI DECERR).
 *  - '''SLVERR''' -- selected slave failed (PSLVERR, WB ERR).
 *  - '''UNIMPL''' -- mapped window, unused offset (AXI SLVERR).
 *  - '''TIMEOUT''' -- cmd or rsp watchdog (AXI SLVERR).
 *  - '''PROTO''' -- illegal burst/size for this bridge (AXI DECERR).
 */
object BusErrorCause {
  def width = 3
  def DECERR  = U(1, width bits)
  def SLVERR  = U(2, width bits)
  def UNIMPL  = U(3, width bits)
  def TIMEOUT = U(4, width bits)
  def PROTO   = U(5, width bits)

  def axiResp(cause: UInt): Bits = {
    val r = Bits(2 bits)
    when(cause === DECERR || cause === PROTO) {
      r := B"11" // AXI DECERR
    } elsewhen(cause === SLVERR || cause === UNIMPL || cause === TIMEOUT) {
      r := B"10" // AXI SLVERR
    } otherwise {
      r := B"00"
    }
    r
  }
}

object BusErrorMaster {
  def width = 2
  val DBus = 0
  val IBus = 1
  val UsbAxi = 2
  val MmiAxi = 3
}

object BusErrorSentinel {
  def DECERR  = B(0xDEADBEEFL, 32 bits)
  def UNIMPL  = B(0xA5A5A5A5L, 32 bits)
  def SLVERR  = B(0x5A5A5A5AL, 32 bits)
  def TIMEOUT_RSP = B(0xDEADBEEFL, 32 bits)
  def TIMEOUT_CMD = B(0xCAFED00DL, 32 bits)

  /** Repeat a 32-bit sentinel across a wider AXI/PMB data word. */
  def tile(width: Int, lane: Bits): Bits = {
    val n = (width + 31) / 32
    val l = lane.resize(32 bits)
    Cat(Seq.fill(n)(l)).resize(width)
  }

  def isLane(data: Bits, lane: Bits): Bool = {
    val n = data.getWidth / 32
    if (n <= 1) data.resize(32 bits) === lane.resize(32 bits)
    else (0 until n).map(i => data(i * 32, 32 bits) === lane.resize(32 bits)).reduceBalancedTree(_ && _)
  }
}

object BusError {
  val Address = 0xe0008000L
  /** FlowLogger CSR window (ctrl through per-channel counts). */
  val Window = 0x400

  /** Elaboration-time. YAML `withBusErrorLogger: false` leaves this off so
    * taps do not topify into a FlowLogger RAM. Miss/timeout still complete. */
  var loggingEnabled: Boolean = false

  def enableLogging(): Unit = { loggingEnabled = true }
  def disableLogging(): Unit = { loggingEnabled = false }

  def reportFlow(f: Flow[BusErrorEvent]): Unit = {
    if (!loggingEnabled) return
    val n = f.getName()
    assert(n != null && n.nonEmpty, "BusError.reportFlow requires Flow.setName")
    val pair = FlowLogger.asFlow(f)
    BusErrorLogger.get().signals += ((pair._1, pair._2, ClockDomain.current, Set("bus-error")))
  }

  private case class UnimplSrc(flow: Flow[Bits], bus: Int, origin: Component)
  private val unimplSrcs = ArrayBuffer[UnimplSrc]()

  private def under(host: Component, c: Component): Boolean = {
    var x = c
    while (x != null) {
      if (x eq host) {
        return true
      }
      x = x.parent
    }
    false
  }

  /** BusIf hole on this interconnect; merged to one FlowLogger channel per bus
    * when [[BusErrorLogger.build]] runs in a parent of this Flow. TinyClunx USB
    * Wishbone holes are siblings of SpineX, so they complete locally and do not
    * enter the SpineX FIFO. Tests and SpineX BusIf holes do. */
  def unimplTap(flow: Flow[BusErrorEvent], bus: Int = BusErrorMaster.DBus): Unit = {
    if (!loggingEnabled) return
    val n = flow.getName()
    assert(n != null && n.nonEmpty, "BusError.unimplTap requires Flow.setName")
    val bits = flow.map(_.asBits).setName(n)
    unimplSrcs += UnimplSrc(bits, bus, Component.current)
  }

  def channelName(bus: Int): String = bus match {
    case BusErrorMaster.IBus => "ibus_unimpl"
    case BusErrorMaster.UsbAxi => "usb_axi_unimpl"
    case BusErrorMaster.MmiAxi => "mmi_axi_unimpl"
    case _ => "dbus_unimpl"
  }

  def bindUnimplMerges(): Unit = {
    val host = Component.current
    val local = unimplSrcs.filter(s => under(host, s.origin))
    unimplSrcs.clear()
    if (local.isEmpty) return
    local.groupBy(_.bus).foreach { case (bus, srcs) =>
      val chName = channelName(bus)
      val streams = srcs.zipWithIndex.map { case (s, i) =>
        val drop = Bool()
        s.flow.toStream(drop).queue(1).setName(s"${chName}_q_$i")
      }
      val merged = if (streams.size == 1) streams.head
        else StreamArbiterFactory.lowerFirst.noLock.on(streams)
      val out = Flow(BusErrorEvent())
      out.setName(chName)
      out.valid := merged.valid
      out.payload.assignFromBits(merged.payload)
      merged.ready := True
      reportFlow(out)
    }
    unimplSrcs.clear()
  }
}

/** Second [[GlobalLogger]] so EventLogger and bus-error do not share `built`. */
object BusErrorLogger {
  var inst: Option[GlobalLogger] = None

  def get(): GlobalLogger = {
    if (inst.isEmpty || inst.get.topComponent != Component.toplevel) {
      inst = Some(new GlobalLogger())
    }
    inst.get
  }

  def build(sysBus: BusSlaveProvider, address: BigInt, depth: Int, name: String,
            ctrlStreams: Option[(Stream[Bits], Flow[Bits])] = None,
            localDepth: Int = 0, irq: Bool = null): Unit = {
    BusError.bindUnimplMerges()
    get().build(sysBus, address, depth, name, ctrlStreams, tags = Set(), localDepth = localDepth, irq = irq,
      atToplevel = false,
      flowLoggerConfig = FlowLoggerConfig(localDepth = localDepth, stageAllFlows = localDepth > 0))
  }

  def create_logger_port(sysBus: BusSlaveProvider, address: BigInt, depth: Int,
                         name: String = "BusErrorLogger",
                         ctrlStreams: Option[(Stream[Bits], Flow[Bits])] = None,
                         localDepth: Int = 0,
                         irq: Bool = null): Unit = {
    sysBus.retain()
    get().retain()
    val host = Component.current
    host.addPrePopTask(() => {
      BusErrorLogger.build(sysBus, address, depth, name, ctrlStreams, localDepth = localDepth, irq = irq)
      get().release()
      sysBus.release()
    })
  }
}

case class BusErrorEvent() extends Bundle {
  val address = UInt(32 bits)
  val write = Bool()
  val masterId = UInt(BusErrorMaster.width bits)
  val cause = UInt(BusErrorCause.width bits)
  val axiResp = Bits(2 bits)
  val data = Bits(32 bits)

  def assign(addr: UInt, wr: Bool, master: Int, c: UInt, dat: Bits): Unit = {
    address := addr.resized
    write := wr
    masterId := U(master, BusErrorMaster.width bits)
    cause := c.resized
    axiResp := BusErrorCause.axiResp(c)
    data := dat.resized
  }

  def assign(addr: UInt, wr: Bool, master: Int, c: UInt): Unit = {
    assign(addr, wr, master, c, B(0, 32 bits))
  }
}
