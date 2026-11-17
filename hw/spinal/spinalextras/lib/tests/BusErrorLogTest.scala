package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config}
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.regif.AccessType.RW
import spinal.lib.bus.regif.ClassName
import spinal.lib.bus.simple.{PipelinedMemoryBus, PipelinedMemoryBusConfig}
import spinal.lib.sim.StreamMonitor
import spinalextras.lib.Config
import spinalextras.lib.bus._
import spinalextras.lib.bus.simple.{Axi4ToPipelinedMemoryBus, Axi4ToPipelinedMemoryBusConfig, PipelinedMemoryBusInterface}
import spinalextras.lib.misc.PipelinedMemoryBusTimeout
import spinalextras.lib.soc.spinex.plugins.PmbHangSlave

import scala.collection.mutable
import scala.util.Random
import scala.language.postfixOps

object BusErrorSim {
  def idxWidth(nSources: Int): Int = log2Up(nSources + 1)

  case class Rec(idx: Int, addr: Long, write: Boolean, master: Int, cause: Int, data: Long)

  def decode(bits: BigInt, nSources: Int): Option[Rec] = {
    val iw = idxWidth(nSources)
    val idxMask = (1 << iw) - 1
    val idx = (bits & idxMask).toInt
    if (idx == idxMask) return None
    val payload = bits >> iw
    val addr = (payload & 0xFFFFFFFFL).toLong
    val meta = payload >> 32
    val write = (meta & 1) != 0
    val master = ((meta >> 1) & 3).toInt
    val cause = ((meta >> 3) & 7).toInt
    val data = ((meta >> 8) & 0xFFFFFFFFL).toLong
    Some(Rec(idx, addr, write, master, cause, data))
  }

  def poke(f: Flow[BusErrorEvent], addr: Long, wr: Boolean, cause: Int, master: Int, data: Long = 0L): Unit = {
    f.valid #= true
    f.payload.address #= addr
    f.payload.write #= wr
    f.payload.cause #= cause
    f.payload.masterId #= master
    f.payload.axiResp #= (if (cause == 1 || cause == 5) 3 else 2)
    f.payload.data #= data
  }

  def idle(f: Flow[BusErrorEvent]): Unit = { f.valid #= false }

  def axiIdle(axi: Axi4): Unit = {
    axi.aw.valid #= false
    axi.w.valid #= false
    axi.ar.valid #= false
    axi.r.ready #= false
    axi.b.ready #= false
  }

  def axiRead32(axi: Axi4, cd: ClockDomain, addr: Long): (Long, Int) = {
    axi.ar.addr #= addr
    axi.ar.len #= 0
    axi.ar.size #= 2
    axi.ar.burst #= 1
    axi.ar.id #= 0
    axi.ar.valid #= true
    axi.r.ready #= true
    cd.waitSamplingWhere(axi.ar.ready.toBoolean)
    axi.ar.valid #= false
    cd.waitSamplingWhere(axi.r.valid.toBoolean)
    val data = axi.r.payload.data.toBigInt.toLong & 0xFFFFFFFFL
    val resp = axi.r.payload.resp.toInt
    (data, resp)
  }

  def pmbCmd(bus: PipelinedMemoryBus, cd: ClockDomain, addr: Long, write: Boolean, data: Long = 0L): Unit = {
    bus.cmd.valid #= true
    bus.cmd.write #= write
    bus.cmd.address #= addr
    bus.cmd.data #= data
    bus.cmd.mask #= 0xF
    cd.waitSamplingWhere(bus.cmd.ready.toBoolean)
    bus.cmd.valid #= false
  }

  def pmbRead(bus: PipelinedMemoryBus, cd: ClockDomain, addr: Long): Long = {
    bus.cmd.valid #= true
    bus.cmd.write #= false
    bus.cmd.address #= addr
    bus.cmd.data #= 0
    bus.cmd.mask #= 0xF
    cd.waitSamplingWhere(bus.cmd.ready.toBoolean)
    val sameCycle = bus.rsp.valid.toBoolean
    val sameData = bus.rsp.data.toLong & 0xFFFFFFFFL
    bus.cmd.valid #= false
    if (sameCycle) sameData
    else {
      cd.waitSamplingWhere(bus.rsp.valid.toBoolean)
      bus.rsp.data.toLong & 0xFFFFFFFFL
    }
  }

  /** Vex DBusSimplePlugin samples rsp the cycle after cmd.ready, not the fire cycle. */
  def pmbReadAfterCmd(bus: PipelinedMemoryBus, cd: ClockDomain, addr: Long): (Long, Boolean) = {
    bus.cmd.valid #= true
    bus.cmd.write #= false
    bus.cmd.address #= addr
    bus.cmd.data #= 0
    bus.cmd.mask #= 0xF
    cd.waitSamplingWhere(bus.cmd.ready.toBoolean)
    bus.cmd.valid #= false
    cd.waitSamplingWhere(bus.rsp.valid.toBoolean)
    (bus.rsp.data.toLong & 0xFFFFFFFFL, bus.rsp.error.toBoolean)
  }

  def pmbWriteAfterCmd(bus: PipelinedMemoryBus, cd: ClockDomain, addr: Long, data: Long): Boolean = {
    pmbCmd(bus, cd, addr, write = true, data)
    cd.waitSamplingWhere(bus.rsp.valid.toBoolean)
    bus.rsp.error.toBoolean
  }

  def attachLog(dut: Component, log: Stream[Bits], nSources: Int, seen: mutable.ArrayBuffer[Rec]): Unit = {
    StreamMonitor(log, dut.clockDomain) { p =>
      decode(p.toBigInt, nSources).foreach(seen += _)
    }
  }
}

class BusErrorLogDut(depth: Int = 16) extends Component {
  BusError.enableLogging()
  val io = new Bundle {
    val a = slave(Flow(BusErrorEvent()))
    val b = slave(Flow(BusErrorEvent()))
    val log = master(Stream(Bits(95 bits)))
    val irq = out Bool()
  }
  io.a.setName("evt_a")
  io.b.setName("evt_b")
  BusError.reportFlow(io.a)
  BusError.reportFlow(io.b)
  val irq = False
  irq.setName("bus_error_irq")
  irq.allowOverride()
  val inFlow = Flow(Bits(32 bits))
  inFlow.setIdle()
  BusErrorLogger.build(null, 0, depth, "BusErrorLogger",
    Some((io.log, inFlow)), localDepth = 2, irq = irq)
  io.irq := irq
}

class PmbMissCompleterDut extends Component {
  withAutoPull()
  BusError.enableLogging()
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(32, 32))
    val log = master(Stream(Bits(95 bits)))
    val irq = out Bool()
  }
  val miss = new PmbMissCompleter(PipelinedMemoryBusConfig(32, 32), BusErrorMaster.DBus, eventName = "pmb_decode_miss_d")
  miss.io.bus <> io.bus
  val irq = False
  irq.setName("bus_error_irq")
  irq.allowOverride()
  val inFlow = Flow(Bits(32 bits))
  inFlow.setIdle()
  BusErrorLogger.build(null, 0, 16, "BusErrorLogger", Some((io.log, inFlow)), localDepth = 2, irq = irq)
  io.irq := irq
}

class PmbTimeoutCmdDut extends Component {
  withAutoPull()
  BusError.enableLogging()
  val cfg = PipelinedMemoryBusConfig(32, 32)
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(cfg))
    val log = master(Stream(Bits(95 bits)))
  }
  val hang = new PmbHangSlave(cfg)
  val tmo = new PipelinedMemoryBusTimeout(cfg, 200 ns)
  tmo.io.pmb_m <> hang.io.bus
  tmo.io.pmb_s <> io.bus
  val inFlow = Flow(Bits(32 bits))
  inFlow.setIdle()
  BusErrorLogger.build(null, 0, 16, "BusErrorLogger", Some((io.log, inFlow)), localDepth = 2)
}

class PmbTimeoutRspDut extends Component {
  withAutoPull()
  BusError.enableLogging()
  val cfg = PipelinedMemoryBusConfig(32, 32)
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(cfg))
    val log = master(Stream(Bits(95 bits)))
  }
  val hang = new Component {
    val io = new Bundle { val bus = slave(PipelinedMemoryBus(cfg)) }
    io.bus.cmd.ready := True
    io.bus.rsp.valid := False
    io.bus.rsp.data.assignDontCare()
    io.bus.rsp.error := False
  }
  val tmo = new PipelinedMemoryBusTimeout(cfg, 200 ns)
  tmo.io.pmb_m <> hang.io.bus
  tmo.io.pmb_s <> io.bus
  val inFlow = Flow(Bits(32 bits))
  inFlow.setIdle()
  BusErrorLogger.build(null, 0, 16, "BusErrorLogger", Some((io.log, inFlow)), localDepth = 2)
}

class PmbUnimplDut extends Component {
  BusError.enableLogging()
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(32, 32))
    val log = master(Stream(Bits(95 bits)))
  }
  io.bus.setName("unimpl_bus")
  implicit val moduleName: ClassName = ClassName("unimpl_test")
  val bif = PipelinedMemoryBusInterface(io.bus, spinal.lib.bus.misc.SizeMapping(0, 0x100))
  val keep = bif.newRegAt(0, "keep")
  keep.field(UInt(32 bits), RW)
  val inFlow = Flow(Bits(32 bits))
  inFlow.setIdle()
  BusErrorLogger.build(null, 0, 16, "BusErrorLogger", Some((io.log, inFlow)), localDepth = 2)
}

class DualUnimplDut extends Component {
  BusError.enableLogging()
  val io = new Bundle {
    val a = slave(Flow(BusErrorEvent()))
    val b = slave(Flow(BusErrorEvent()))
    val log = master(Stream(Bits(95 bits)))
  }
  io.a.setName("hole_a")
  io.b.setName("hole_b")
  BusError.unimplTap(io.a, BusErrorMaster.DBus)
  BusError.unimplTap(io.b, BusErrorMaster.DBus)
  val inFlow = Flow(Bits(32 bits))
  inFlow.setIdle()
  BusErrorLogger.build(null, 0, 16, "BusErrorLogger", Some((io.log, inFlow)), localDepth = 2)
}

class PmbMissNoLogDut extends Component {
  BusError.disableLogging()
  val io = new Bundle {
    val bus = slave(PipelinedMemoryBus(32, 32))
  }
  val miss = new PmbMissCompleter(PipelinedMemoryBusConfig(32, 32), BusErrorMaster.DBus, eventName = "pmb_decode_miss_d")
  miss.io.bus <> io.bus
}

class AxiMissLogDut extends Component {
  BusError.enableLogging()
  val axiConfig = Axi4Config(addressWidth = 16, dataWidth = 32, idWidth = 2,
    useRegion = false, useLock = false, useQos = false, useCache = false, useProt = false)
  val io = new Bundle {
    val axi = slave(Axi4(axiConfig))
    val log = master(Stream(Bits(95 bits)))
  }
  io.axi.setName("axi")
  val miss = new AxiMissCompleter(axiConfig)
  miss.io.axi <> io.axi
  val ev = AXIBusLogger.busError(io.axi, BusErrorMaster.UsbAxi, "usb_axi_err")
  BusError.reportFlow(ev)
  val inFlow = Flow(Bits(32 bits))
  inFlow.setIdle()
  BusErrorLogger.build(null, 0, 16, "BusErrorLogger", Some((io.log, inFlow)), localDepth = 2)
}

class AxiUnimplLogDut extends Component {
  BusError.enableLogging()
  val axiConfig = Axi4Config(addressWidth = 32, dataWidth = 32, idWidth = 2,
    useRegion = false, useLock = false, useQos = false, useCache = false, useProt = false)
  val io = new Bundle {
    val axi = slave(Axi4(axiConfig))
    val log = master(Stream(Bits(95 bits)))
  }
  io.axi.setName("axi")
  val bridge = Axi4ToPipelinedMemoryBus(Axi4ToPipelinedMemoryBusConfig(axiConfig, 4, readResponseFifoLatency = 0))
  bridge.io.axi <> io.axi
  implicit val moduleName: ClassName = ClassName("axi_unimpl")
  val bif = PipelinedMemoryBusInterface(bridge.io.pmb, SizeMapping(0, 0x100))
  val keep = bif.newRegAt(0, "keep")
  keep.field(UInt(32 bits), RW)
  val ev = AXIBusLogger.busError(io.axi, BusErrorMaster.MmiAxi, "mmi_axi_err")
  BusError.reportFlow(ev)
  val inFlow = Flow(Bits(32 bits))
  inFlow.setIdle()
  BusErrorLogger.build(null, 0, 16, "BusErrorLogger", Some((io.log, inFlow)), localDepth = 2)
}

class BusErrorLogTest extends AnyFunSuite {
  test("two events in one cycle both land with matching scoreboard") {
    Config.sim.doSim(new BusErrorLogDut(16).setDefinitionName("BusErrorLogConcurrent")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      BusErrorSim.idle(dut.io.a)
      BusErrorSim.idle(dut.io.b)
      dut.io.log.ready #= false
      dut.clockDomain.waitSampling(4)

      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 2, seen)

      BusErrorSim.poke(dut.io.a, 0x1000, wr = false, cause = 1, master = 0, data = 0xDEADBEEFL)
      BusErrorSim.poke(dut.io.b, 0x2000, wr = true, cause = 4, master = 1, data = 0xCAFED00DL)
      dut.clockDomain.waitSampling()
      BusErrorSim.idle(dut.io.a)
      BusErrorSim.idle(dut.io.b)
      dut.clockDomain.waitSamplingWhere(dut.io.irq.toBoolean)
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(24)

      val addrs = seen.map(_.addr).toSet
      assert(addrs.contains(0x1000L), seen)
      assert(addrs.contains(0x2000L), seen)
      assert(seen.exists(r => r.addr == 0x1000L && r.cause == 1 && !r.write && r.data == 0xDEADBEEFL), seen)
      assert(seen.exists(r => r.addr == 0x2000L && r.cause == 4 && r.write && r.data == 0xCAFED00DL), seen)
    }
  }

  test("irq follows occupancy; drain clears irq") {
    Config.sim.doSim(new BusErrorLogDut(16).setDefinitionName("BusErrorLogIrq")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      BusErrorSim.idle(dut.io.a)
      BusErrorSim.idle(dut.io.b)
      dut.io.log.ready #= false
      dut.clockDomain.waitSampling(4)

      BusErrorSim.poke(dut.io.a, 0x10, wr = false, cause = 2, master = 0)
      dut.clockDomain.waitSampling()
      BusErrorSim.idle(dut.io.a)
      dut.clockDomain.waitSamplingWhere(dut.io.irq.toBoolean)
      dut.io.log.ready #= true
      dut.clockDomain.waitSamplingWhere(!dut.io.irq.toBoolean)
    }
  }

  test("backpressure holds both sources then drains in order") {
    Config.sim.doSim(new BusErrorLogDut(16).setDefinitionName("BusErrorLogStall")) { dut =>
      SimTimeout(100 us)
      dut.clockDomain.forkStimulus(100 MHz)
      BusErrorSim.idle(dut.io.a)
      BusErrorSim.idle(dut.io.b)
      dut.io.log.ready #= false
      dut.clockDomain.waitSampling(4)

      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 2, seen)
      var stallCycles = 0
      dut.clockDomain.onSamplings {
        if (dut.io.log.valid.toBoolean && !dut.io.log.ready.toBoolean) stallCycles += 1
      }

      val expected = mutable.ArrayBuffer[(Long, Int)]()
      for (i <- 0 until 8) {
        val addr = 0x1000L + i * 4
        BusErrorSim.poke(dut.io.a, addr, wr = false, cause = 1, master = 0)
        expected += ((addr, 1))
        dut.clockDomain.waitSampling()
        BusErrorSim.idle(dut.io.a)
        dut.clockDomain.waitSampling()
      }
      assert(stallCycles >= 4, s"planned stall never happened, stallCycles=$stallCycles")
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(40)
      assert(seen.size >= 8, seen)
      val got = seen.filter(_.idx == 0).map(_.addr)
      assert(expected.map(_._1).forall(got.contains), s"got=$got expected=$expected")
    }
  }

  test("randomized soak with random ready; all accepted events match") {
    val seed = 0xC0FFEE
    println(s"BusErrorLogSoak seed=$seed")
    Config.sim.doSim(new BusErrorLogDut(32).setDefinitionName("BusErrorLogSoak"), "BusErrorLogSoak", seed) { dut =>
      SimTimeout(500 us)
      dut.clockDomain.forkStimulus(100 MHz)
      val rnd = new Random(seed)
      BusErrorSim.idle(dut.io.a)
      BusErrorSim.idle(dut.io.b)
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)

      val pending = mutable.Queue[BusErrorSim.Rec]()
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 2, seen)

      for (i <- 0 until 40) {
        dut.io.log.ready #= rnd.nextInt(100) >= 20
        val useA = rnd.nextBoolean()
        val addr = (0x40000000L + (rnd.nextInt(0x1000) << 2)) & 0xFFFFFFFFL
        val cause = Seq(1, 2, 3, 4, 5)(rnd.nextInt(5))
        val wr = rnd.nextBoolean()
        val master = if (useA) 0 else 1
        val data = rnd.nextInt() & 0xFFFFFFFFL
        if (useA) BusErrorSim.poke(dut.io.a, addr, wr, cause, master, data)
        else BusErrorSim.poke(dut.io.b, addr, wr, cause, master, data)
        pending += BusErrorSim.Rec(if (useA) 0 else 1, addr, wr, master, cause, data)
        dut.clockDomain.waitSampling()
        BusErrorSim.idle(dut.io.a)
        BusErrorSim.idle(dut.io.b)
        dut.clockDomain.waitSampling()
      }
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(80)

      assert(seen.size >= 40, s"dropped events under 20% stall, seen=${seen.size}")
      val got = seen.map(r => (r.addr, r.cause, r.write, r.data)).toSet
      pending.foreach { e =>
        assert(got.contains((e.addr, e.cause, e.write, e.data)), s"missing $e in $got")
      }
    }
  }

  test("overflow drops some events when fifo is stuck") {
    Config.sim.doSim(new BusErrorLogDut(4).setDefinitionName("BusErrorLogDrop")) { dut =>
      SimTimeout(100 us)
      dut.clockDomain.forkStimulus(100 MHz)
      BusErrorSim.idle(dut.io.a)
      BusErrorSim.idle(dut.io.b)
      dut.io.log.ready #= false
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 2, seen)
      for (i <- 0 until 24) {
        BusErrorSim.poke(dut.io.a, 0x8000L + i, wr = false, cause = 1, master = 0)
        dut.clockDomain.waitSampling()
        BusErrorSim.idle(dut.io.a)
      }
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(40)
      assert(seen.size < 24, s"expected drops, seen=${seen.size}")
      assert(seen.nonEmpty)
    }
  }

  test("PMB decode miss completes read with DECERR sentinel and logs") {
    Config.sim.doSim(new PmbMissCompleterDut().setDefinitionName("PmbMissRead")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.bus.cmd.valid #= false
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 1, seen)
      val (data, err) = BusErrorSim.pmbReadAfterCmd(dut.io.bus, dut.clockDomain, 0x12345678L)
      assert(data == 0xDEADBEEFL, f"sentinel 0x$data%08x")
      assert(err, "DECERR must set rsp.error")
      dut.clockDomain.waitSampling(16)
      assert(seen.exists(r => r.cause == 1 && r.addr == 0x12345678L && r.data == 0xDEADBEEFL), seen)
    }
  }

  test("PMB decode miss write completes with rsp.error") {
    Config.sim.doSim(new PmbMissCompleterDut().setDefinitionName("PmbMissWrite")) { dut =>
      SimTimeout(20 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.bus.cmd.valid #= false
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 1, seen)
      val err = BusErrorSim.pmbWriteAfterCmd(dut.io.bus, dut.clockDomain, 0xAABB0000L, 0x11)
      assert(err, "DECERR write must set rsp.error")
      dut.clockDomain.waitSampling(16)
      assert(seen.exists(r => r.cause == 1 && r.write && r.addr == 0xAABB0000L), seen)
    }
  }

  test("two outstanding miss reads both complete") {
    Config.sim.doSim(new PmbMissCompleterDut().setDefinitionName("PmbMissTwoReads")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.bus.cmd.valid #= false
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val a = BusErrorSim.pmbRead(dut.io.bus, dut.clockDomain, 0x1L)
      val b = BusErrorSim.pmbRead(dut.io.bus, dut.clockDomain, 0x2L)
      assert(a == 0xDEADBEEFL && b == 0xDEADBEEFL)
    }
  }

  test("timeout on hung cmd returns TIMEOUT_CMD sentinel") {
    Config.sim.doSim(new PmbTimeoutCmdDut().setDefinitionName("PmbTimeoutCmd")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.bus.cmd.valid #= false
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 1, seen)
      val (data, err) = BusErrorSim.pmbReadAfterCmd(dut.io.bus, dut.clockDomain, 0xE0008600L)
      assert(data == 0xCAFED00DL, f"got 0x$data%08x")
      assert(err, "TIMEOUT_CMD must set rsp.error")
      dut.clockDomain.waitSampling(16)
      assert(seen.exists(_.cause == 4), seen)
    }
  }

  test("timeout on hung write cmd returns TIMEOUT_CMD with rsp.error") {
    Config.sim.doSim(new PmbTimeoutCmdDut().setDefinitionName("PmbTimeoutCmdWrite")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.bus.cmd.valid #= false
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 1, seen)
      val err = BusErrorSim.pmbWriteAfterCmd(dut.io.bus, dut.clockDomain, 0xE0008600L, 0x55)
      assert(err, "TIMEOUT_CMD write must set rsp.error")
      dut.clockDomain.waitSampling(16)
      assert(seen.exists(r => r.cause == 4 && r.write), seen)
    }
  }

  test("timeout on hung rsp returns TIMEOUT_RSP sentinel") {
    Config.sim.doSim(new PmbTimeoutRspDut().setDefinitionName("PmbTimeoutRsp")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.bus.cmd.valid #= false
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 1, seen)
      val (data, err) = BusErrorSim.pmbReadAfterCmd(dut.io.bus, dut.clockDomain, 0xE0008604L)
      assert(data == 0xDEADBEEFL, f"got 0x$data%08x")
      assert(err, "TIMEOUT_RSP must set rsp.error")
      dut.clockDomain.waitSampling(16)
      assert(seen.exists(r => r.cause == 4 && r.data == 0xDEADBEEFL), seen)
    }
  }

  test("BusIf hole is UNIMPL with 0xA5A5A5A5") {
    Config.sim.doSim(new PmbUnimplDut().setDefinitionName("PmbUnimpl")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.bus.cmd.valid #= false
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 1, seen)
      val (data, err) = BusErrorSim.pmbReadAfterCmd(dut.io.bus, dut.clockDomain, 0x40)
      assert(data == 0xA5A5A5A5L, f"got 0x$data%08x")
      assert(err, "UNIMPL read must set rsp.error")
      dut.clockDomain.waitSampling(16)
      assert(seen.exists(r => r.cause == 3 && r.data == 0xA5A5A5A5L), seen)
    }
  }

  test("BusIf hole write emits rsp.error") {
    Config.sim.doSim(new PmbUnimplDut().setDefinitionName("PmbUnimplWrite")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.bus.cmd.valid #= false
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 1, seen)
      val err = BusErrorSim.pmbWriteAfterCmd(dut.io.bus, dut.clockDomain, 0x40, 0x22)
      assert(err, "UNIMPL write must set rsp.error")
      dut.clockDomain.waitSampling(16)
      assert(seen.exists(r => r.cause == 3 && r.write), seen)
    }
  }

  test("two dBus holes in one cycle both appear on dbus_unimpl") {
    Config.sim.doSim(new DualUnimplDut().setDefinitionName("DualUnimpl")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      BusErrorSim.idle(dut.io.a)
      BusErrorSim.idle(dut.io.b)
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 1, seen)
      BusErrorSim.poke(dut.io.a, 0x10, wr = false, cause = 3, master = 0, data = 0xA5A5A5A5L)
      BusErrorSim.poke(dut.io.b, 0x20, wr = true, cause = 3, master = 0, data = 0xA5A5A5A5L)
      dut.clockDomain.waitSampling()
      BusErrorSim.idle(dut.io.a)
      BusErrorSim.idle(dut.io.b)
      dut.clockDomain.waitSampling(24)
      assert(seen.exists(_.addr == 0x10L), seen)
      assert(seen.exists(_.addr == 0x20L), seen)
    }
  }

  test("logger off still completes miss and does not require FlowLogger") {
    Config.sim.doSim(new PmbMissNoLogDut().setDefinitionName("PmbMissNoLog")) { dut =>
      SimTimeout(20 us)
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.bus.cmd.valid #= false
      dut.clockDomain.waitSampling(4)
      val data = BusErrorSim.pmbRead(dut.io.bus, dut.clockDomain, 0x99)
      assert(data == 0xDEADBEEFL)
    }
  }

  test("AXI decode miss is DECERR with 0xDEADBEEF and usb_axi_err log") {
    Config.sim.doSim(new AxiMissLogDut().setDefinitionName("AxiMissLog")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      BusErrorSim.axiIdle(dut.io.axi)
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 1, seen)
      val (data, resp) = BusErrorSim.axiRead32(dut.io.axi, dut.clockDomain, 0x1000)
      assert(resp == 3, s"resp=$resp")
      assert(data == 0xDEADBEEFL, f"got 0x$data%08x")
      dut.clockDomain.waitSampling(16)
      assert(seen.exists(r => r.cause == 1 && r.data == 0xDEADBEEFL && r.master == 2), seen)
    }
  }

  test("AXI BusIf hole is SLVERR UNIMPL with 0xA5A5A5A5") {
    Config.sim.doSim(new AxiUnimplLogDut().setDefinitionName("AxiUnimplLog")) { dut =>
      SimTimeout(50 us)
      dut.clockDomain.forkStimulus(100 MHz)
      BusErrorSim.axiIdle(dut.io.axi)
      dut.io.log.ready #= true
      dut.clockDomain.waitSampling(4)
      val seen = mutable.ArrayBuffer[BusErrorSim.Rec]()
      BusErrorSim.attachLog(dut, dut.io.log, 2, seen)
      val (data, resp) = BusErrorSim.axiRead32(dut.io.axi, dut.clockDomain, 0x40)
      assert(resp == 2, s"resp=$resp")
      assert(data == 0xA5A5A5A5L, f"got 0x$data%08x")
      dut.clockDomain.waitSampling(16)
      assert(seen.exists(r => r.cause == 3 && r.data == 0xA5A5A5A5L && r.master == 3), seen)
    }
  }
}
