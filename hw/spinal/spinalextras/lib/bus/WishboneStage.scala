package spinalextras.lib.bus

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig}
import spinal.lib.sim.ScoreboardInOrder
import spinal.lib.wishbone.sim.{WishboneDriver, WishboneMonitor, WishboneSequencer, WishboneTransaction}
import spinalextras.lib.Config
import spinalextras.lib.logging.{GlobalLogger, WishboneBusLogger}

case class WishboneCmd(config: WishboneConfig) extends Bundle {
  val WE        = Bool()
  val ADR       = UInt(config.addressWidth bits)
  val DAT_MOSI  = Bits(config.dataWidth bits)

  val SEL       = if(config.useSEL)   Bits(config.selWidth bits) else null
  val LOCK      = if(config.useLOCK)  Bool()                     else null
  val CTI       = if(config.useCTI)   Bits(3 bits)               else null

  val TGD_MOSI  = if(config.useTGD)   Bits(config.tgdWidth bits) else null
  val TGA       = if(config.useTGA)   Bits(config.tgaWidth bits) else null
  val TGC       = if(config.useTGC)   Bits(config.tgcWidth bits) else null
  val BTE       = if(config.useBTE)   Bits(2 bits)               else null

  def connect(bus : Wishbone): Unit = {
    val el = Map(elements:_*)
    val bus_el = Map(bus.elements:_*)
    for ((k,v) <- el) {
      if(v != null)
        v <> bus_el(k)
    }
  }
}

case class WishboneRsp(config: WishboneConfig) extends Bundle {
  val DAT_MISO  = Bits(config.dataWidth bits)
  val TGD_MISO  = if(config.useTGD)   Bits(config.tgdWidth bits) else null
  val ERR       = if(config.useERR)   Bool()                     else null
  val RTY       = if(config.useRTY)   Bool()                     else null

  def connect(bus : Wishbone): Unit = {
    val el = Map(elements:_*)
    val bus_el = Map(bus.elements:_*)
    for ((k,v) <- el) {
      if(v != null)
        v <> bus_el(k)
    }
  }
}

case class WishboneStream(config : WishboneConfig) extends Bundle with IMasterSlave {
  val cmd = Stream(WishboneCmd(config))
  val rsp = Flow(WishboneRsp(config))

  override def asMaster(): Unit = {
    master(cmd)
    slave(rsp)
  }

  def cmdM2sPipe(): WishboneStream = {
    val ret = cloneOf(this)
    /* keep: this register is a timing cut. Without it, synthesis can
     * retime the slave read mux back onto the decoder address. */
    this.cmd.m2sPipe(keep = true) >> ret.cmd
    this.rsp << ret.rsp
    ret
  }

  def cmdS2mPipe(): WishboneStream = {
    val ret = cloneOf(this)
    this.cmd.s2mPipe() >> ret.cmd
    this.rsp << ret.rsp
    ret
  }

  def rspPipe(): WishboneStream = {
    val ret = cloneOf(this)
    this.cmd >> ret.cmd
    this.rsp << ret.rsp.stage()

    ret
  }

  def <<(m : WishboneStream) : Unit = {
    val s = this
    assert(m.config.addressWidth >= s.config.addressWidth)
    assert(m.config.dataWidth == s.config.dataWidth)
    s.cmd << m.cmd
    m.rsp >> s.rsp
  }
  def >>(s : WishboneStream) : Unit = s << this

}

case class Wb2WishboneStream_s2m(config : WishboneConfig, rspPipe : Boolean) extends Component {
  val io = new Bundle {
    val bus = slave(Wishbone(config))
    val stream = master(WishboneStream(config))
  }

  val stream = cloneOf(io.stream)

  stream.cmd.connect(io.bus)
  stream.rsp.connect(io.bus)

  stream.cmd.valid := io.bus.masterHasRequest
  io.bus.ACK := stream.rsp.valid

  if(rspPipe) {
    io.stream <> stream.rspPipe()
  } else {
    io.stream <> stream
  }

}

case class Wb2WishboneStream_m2s(config : WishboneConfig) extends Component {
  val io = new Bundle {
    val bus = master(Wishbone(config))
    val stream = slave(WishboneStream(config))
  }

  val stream = cloneOf(io.stream)

  stream.cmd.connect(io.bus)
  stream.rsp.connect(io.bus)

  io.bus.CYC := stream.cmd.valid
  io.bus.STB := stream.cmd.valid

  stream.cmd.ready := io.bus.isRequestAck
  stream.rsp.valid := io.bus.isResponse

  io.stream <> stream
}

object WishboneStream {
  def apply(bus : Wishbone, rspPipe : Boolean) : WishboneStream = {
    require(bus.isMasterInterface || bus.isSlaveInterface)
    if(bus.isMasterInterface) {
      val dut = Wb2WishboneStream_m2s(bus.config)
      dut.io.bus <> bus
      if (rspPipe) dut.io.stream.rspPipe() else dut.io.stream
    } else {
      val dut = Wb2WishboneStream_s2m(bus.config, rspPipe = rspPipe)
      dut.io.bus <> bus
      dut.io.stream
    }
  }

  def apply(stream : WishboneStream, rspPipe : Boolean) : Wishbone = {
    //assert(stream.isMasterInterface || stream.isSlaveInterface)
    if(!stream.isSlaveInterface) {
      val dut = Wb2WishboneStream_m2s(stream.config)
      dut.io.stream <> (if (rspPipe) stream.rspPipe() else stream)
      dut.io.bus
    } else {
      val dut = Wb2WishboneStream_s2m(stream.config, rspPipe)
      dut.io.stream <> stream
      dut.io.bus
    }
  }
}

/** One slave cycle per master cycle. Stream.m2sPipe reloads while the
  * master still holds CYC through ACK, so a registered-ACK slave sees
  * the same access twice. Descriptor post and completion pop then fire
  * twice and the bulk ring loses a transfer. */
case class WbSingleIssueCut(config: WishboneConfig) extends Component {
  val io = new Bundle {
    val up = slave(Wishbone(config))
    val down = master(Wishbone(config))
  }

  val req = io.up.CYC && io.up.STB
  val busy = RegInit(False)
  val adr = Reg(io.up.ADR) init 0
  val we = Reg(Bool()) init False
  val datMosi = Reg(io.up.DAT_MOSI) init 0
  val sel = if (config.useSEL) Reg(io.up.SEL) init 0 else null
  /* keep: this address register is the 75 MHz cut in front of the
   * UsbEngine read mux. Retiming must not pull that mux back across it. */
  KeepAttribute(adr, datMosi, we)
  if (sel != null) KeepAttribute(sel)

  val done = io.down.isRequestAck
  when(!busy && req) {
    busy := True
    adr := io.up.ADR
    we := io.up.WE
    datMosi := io.up.DAT_MOSI
    if (sel != null) sel := io.up.SEL
  }
  when(done) {
    busy := False
  }

  io.down.CYC := busy
  io.down.STB := busy
  io.down.ADR := adr
  io.down.WE := we
  io.down.DAT_MOSI := datMosi
  if (sel != null) io.down.SEL := sel
  if (config.useLOCK) io.down.LOCK := False
  if (config.useCTI) io.down.CTI := 0
  if (config.useBTE) io.down.BTE := 0
  if (config.useTGA) io.down.TGA := 0
  if (config.useTGC) io.down.TGC := 0
  if (config.useTGD) io.down.TGD_MOSI := 0

  io.up.ACK := io.down.ACK && busy
  io.up.DAT_MISO := io.down.DAT_MISO
  if (config.useERR) io.up.ERR := io.down.ERR && busy
  if (config.useRTY) io.up.RTY := False
  if (config.useSTALL) io.up.STALL := False
  if (config.useTGD) io.up.TGD_MISO := 0
}

object WishboneStage {
  def apply(bus : Wishbone): Wishbone = {
    apply(bus, m2s_stage = true, s2m_stage = false)
  }

  /**
   * Takes in / returns a master-driven signal.
   * Stages through [[WishboneStream]] so ERR rides with ACK on the rsp Flow.
   * The PMB adapters cannot carry write ERR: PMB has no write response.
   */
  def apply(bus: Wishbone, m2s_stage: Boolean, s2m_stage: Boolean = false): Wishbone = {
    if(!m2s_stage && !s2m_stage)
      return bus

    if (m2s_stage && !s2m_stage) {
      val cut = WbSingleIssueCut(bus.config)
      cut.io.up <> bus
      val out_bus = cut.io.down
      GlobalLogger(
        Set("debug-wb"),
        WishboneBusLogger.flows(bus, out_bus.setName("out_bus"))
      )
      return out_bus
    }

    val inConv = Wb2WishboneStream_s2m(bus.config, rspPipe = false)
    inConv.io.bus <> bus
    val staged = (m2s_stage, s2m_stage) match {
      case (true, true) => inConv.io.stream.cmdM2sPipe().cmdS2mPipe().rspPipe()
      case (false, true) => inConv.io.stream.cmdS2mPipe().rspPipe()
      case _ => inConv.io.stream
    }
    val outConv = Wb2WishboneStream_m2s(bus.config)
    outConv.io.stream <> staged
    val out_bus = outConv.io.bus

    GlobalLogger(
      Set("debug-wb"),
      WishboneBusLogger.flows(bus, out_bus.setName("out_bus"))
    )

    out_bus
  }
}


