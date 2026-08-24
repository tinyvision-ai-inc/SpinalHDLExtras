package spinalextras.lib.mipi.lattice

import spinal.core._
import spinal.lib._
import spinal.lib.bus.regif.BusIf
import spinalextras.lib.blackbox.lattice.lifcl.{CsiErrorPulses, dphy_rx}
import spinalextras.lib.bus.LMMI
import spinalextras.lib.mipi._

import scala.language.postfixOps

case class MIPIToPixel(cfg : MIPIConfig,
                       sync_cd : ClockDomain,
                       pixel_cd : ClockDomain,
                       byte_cd : ClockDomain = null,
                       sensor_name : String = "",
                       clock_suffix : Boolean = true,
                       is_continous_clock : Option[Boolean] = None,
                       with_lmmi : Boolean = true,
                       /** Soft-DPHY RX FIFO (pktdly + fifo misc). Independent of continuous clock. */
                       with_rx_fifo : Boolean = false,
                       /** Soft-DPHY CRC_CHECK_IN ports. Match the regenerated IP .cfg. */
                       enable_crc_check : Boolean = true,
                       /** Soft-DPHY LANE_ALIGN. Inside the encrypted core; must match .cfg. */
                       with_lane_align : Boolean = true,
                       /** PulseCC error/activity + clk_byte_hs for the shared CSI error bank. */
                       with_error_pulses : Boolean = true,
                       /** Soft-DPHY / byte2pixel SignalLogger taps. Off with withDebugRegisters. */
                       enable_logging : Boolean = true,
                       /** Soft-DPHY MISC + byte2pixel debug_signals ports (b2p attach_bus). */
                       enable_misc_signals : Boolean = true
                 ) extends Component {
  val io = new Bundle {
    val mipi = slave(MIPIIO(cfg.numRXLanes))
    val pll_lock = in(Bool())

    val tx_rdy = in(Bool()) default(True)

    val pixelFlow = master(Flow(Fragment(Vec(Bits(cfg.PIX_WIDTH bits), cfg.outputLanes))))

    /**
     * Header events in the Soft-DPHY **byte** clock domain (`lp_en`/`sp_en`).
     * Cross to the stats/CPU domain with StreamCC (see TinyvisionCameraBoard).
     */
    val mipi_header = master Flow(MIPIPacketHeader())

    /** Frame-valid in pixel_cd (for PTS latch via level CDC into the UVC clock). */
    val frame_valid = out Bool()

    val dphy_lmmi = with_lmmi generate slave(LMMI(8, 8))

    /** Soft-DPHY recovered HS byte clock (ClockMeasure / debug mux). */
    val clk_byte_hs = with_error_pulses generate (out Bool())
    /** Error/activity rises PulseCC'd into this component's clock. */
    val errorPulses = with_error_pulses generate out(new CsiErrorPulses())
  }
  val byte_freq = cfg.dphyByteFreq

  if(sensor_name != "") {
    io.mipi.setPartialName(s"${sensor_name}_mipi")
    io.pixelFlow.setPartialName(s"${sensor_name}_pixelFlow")
  }

  noIoPrefix()
  // No-LMMI Soft-DPHY hardcodes settle / pkt delay in the Soft-IP wrapper
  // Only expose those parallel ports when LMMI is on (DYN_*).
  val mipi_to_bytes = new dphy_rx(cfg,
    sync_cd = sync_cd,
    byte_cd = byte_cd,
    clock_suffix = clock_suffix,
    is_continous_clock = is_continous_clock,
    with_lmmi = with_lmmi,
    with_rx_fifo = with_rx_fifo,
    enable_crc_check = enable_crc_check,
    with_lane_align = with_lane_align,
    enable_logging = enable_logging,
    enable_misc_signals = enable_misc_signals,
    cfg_datsettle_cyc = with_lmmi,
    cfg_fifo_read_delay = with_lmmi && with_rx_fifo,
  )
  if(with_lmmi) {
    io.dphy_lmmi <> mipi_to_bytes.io.lmmi
    mipi_to_bytes.io.lmmi_clk_i := ClockDomain.current.readClockWire
    mipi_to_bytes.io.lmmi_resetn_i := !ClockDomain.current.isResetActive
  }
  mipi_to_bytes.assignMIPI(io.mipi)

  mipi_to_bytes.io.pll_lock_i := io.pll_lock
  mipi_to_bytes.io.tx_rdy_i := io.tx_rdy
  // ref_dt_i is left to its port default (cfg.refDt.id). When attach_bus() is called it
  // is overridden by the runtime-writable ref_dt register (see dphy_rx.attach_bus).
  // Soft-DPHY LMMI variant has no parallel ref_dt / vcx / dropnull pins.

  if (mipi_to_bytes.io.rxcsr_dropnull_i != null) {
    mipi_to_bytes.io.rxcsr_dropnull_i := False
  }
  if (mipi_to_bytes.io.rxcsr_vcx_on_i != null) {
    mipi_to_bytes.io.rxcsr_vcx_on_i := False
  }

  val bytes_to_pixels = byte2pixel(cfg, pixel_cd = pixel_cd, byte_cd = mipi_to_bytes.byte_cd())

  val mipiHdr = mipi_to_bytes.MIPIPacketHeader
  bytes_to_pixels.assignMIPIHeader(mipiHdr)
  bytes_to_pixels.assignMIPIBytes(mipi_to_bytes.MIPIBytes)

  io.mipi_header << mipiHdr

  // PixelFlow2Fragment / io.pixelFlow must run in pixel_cd. MIPIToPixel is often
  // elaborated under the CPU ClockDomain; without this area the fragment path is
  // tagged CPU while byte2pixel drives pixel_cd → PhaseCheckCrossClock.
  new ClockingArea(pixel_cd) {
    io.frame_valid := bytes_to_pixels.io.pixelFlow.frame_valid
    io.pixelFlow <> PixelFlow2Fragment(bytes_to_pixels.io.pixelFlow).map(f => {
      val outFlow = Fragment(Vec(Bits(cfg.PIX_WIDTH bits), cfg.outputLanes))
      outFlow.last := f.last
      outFlow.fragment.assignFromBits(f.fragment)
      outFlow
    })
  }

  def byte_clock_domain() : ClockDomain = {
    mipi_to_bytes.byte_cd()
  }

  val input_rate = cfg.rxGear * cfg.numRXLanes * cfg.dphyByteFreq.toDouble
  val sink_rate = cfg.DT_WIDTH * pixel_cd.frequency.getValue.toDouble
  require(input_rate <= sink_rate, s"Configuration doesn't work; pixel clock can't keep up with the output ${input_rate} >= ${sink_rate}")

  def attach_csi_bus(busSlaveFactory: BusIf, withErrorCounters: Boolean = true): Unit = {
    mipi_to_bytes.attach_bus(busSlaveFactory, withErrorCounters)
  }

  def attach_b2p_bus(busSlaveFactory: BusIf): Unit = {
    bytes_to_pixels.attach_bus(busSlaveFactory)
  }

  if (with_error_pulses) {
    io.clk_byte_hs := mipi_to_bytes.io.clk_byte_hs_o
    io.errorPulses := mipi_to_bytes.errorPulsesInByteClock().pulseCc(
      byte_clock_domain(), ClockDomain.current)
  }

  /** Byte-clock rises PulseCC'd into this component's clock. */
  def errorPulsesSys(clockOut: ClockDomain = ClockDomain.current): CsiErrorPulses = {
    require(with_error_pulses, "errorPulsesSys requires with_error_pulses")
    io.errorPulses
  }

  def clkByteHs: Bool = {
    require(with_error_pulses, "clkByteHs requires with_error_pulses")
    io.clk_byte_hs
  }

  /** Attach Soft-DPHY CSI CSRs then byte2pixel debug counters on the same BusIf. */
  def attach_bus(busSlaveFactory: BusIf, withErrorCounters: Boolean = true): Unit = {
    attach_csi_bus(busSlaveFactory, withErrorCounters)
    attach_b2p_bus(busSlaveFactory)
  }
}
