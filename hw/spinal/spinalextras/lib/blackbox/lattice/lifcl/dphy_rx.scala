package spinalextras.lib.blackbox.lattice.lifcl


import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.BusSlaveFactory
import spinal.lib.bus.regif.AccessType._
import spinal.lib.bus.regif.{BusIf, RegInst, SymbolName}
import spinalextras.lib.bus.LMMI
import spinalextras.lib.Constraints
import spinalextras.lib.logging.{FlowLogger, GlobalLogger, SignalLogger}
import spinalextras.lib.mipi.{MIPIConfig, MIPIIO, MIPIPacketHeader}
import spinalextras.lib.misc.{AsyncToSyncReset, GlobalSignals}

import scala.language.postfixOps

/**
 * Soft-DPHY error/activity pulses in the **byte** clock (already rise-detected).
 * Same order / names as [[dphy_rx]] WC offsets. Level signals (`hs_d_en`, `payload_en`,
 * `cd_*`) are rises, not free-running cycle counts — 16-bit saturating counters stay useful.
 */
class CsiErrorPulses extends Bundle {
  val hs_sync_rise = Bool()
  val term_clk_en_rise = Bool()
  val term_d_en_o0 = Bool()
  val hs_d_en_o = Bool()
  val cd_clk_o = Bool()
  val cd_d0_o = Bool()
  val ecc_check_o = Bool()
  val ecc_1bit_error_o = Bool()
  val ecc_2bit_error_o = Bool()
  val ecc_byte_error_o = Bool()
  val payload_en_o = Bool()
  val lp_en_o = Bool()
  val lp_av_en_o = Bool()
  val sp_en_o = Bool()
  val payload_crcvld_o = Bool()
  val fifo_ovflw_err_o = Bool()
  val rxque_full_o = Bool()
  val rxfullfr0_o = Bool()
  val rxfullfr1_o = Bool()
  val crc_check_o = Bool()
  val crc_error_o = Bool()

  def items: Seq[(Int, String, Bool)] = Seq(
    (dphy_rx.OffHsSyncRise, "hs_sync_rise", hs_sync_rise),
    (dphy_rx.OffTermClkEn, "term_clk_en_rise", term_clk_en_rise),
    (dphy_rx.OffTermD0, "term_d_en_o0", term_d_en_o0),
    (dphy_rx.OffHsDEn, "hs_d_en_o", hs_d_en_o),
    (dphy_rx.OffCdClk, "cd_clk_o", cd_clk_o),
    (dphy_rx.OffCdD0, "cd_d0_o", cd_d0_o),
    (dphy_rx.OffEccCheck, "ecc_check_o", ecc_check_o),
    (dphy_rx.OffEcc1bit, "ecc_1bit_error_o", ecc_1bit_error_o),
    (dphy_rx.OffEcc2bit, "ecc_2bit_error_o", ecc_2bit_error_o),
    (dphy_rx.OffEccByte, "ecc_byte_error_o", ecc_byte_error_o),
    (dphy_rx.OffPayloadEn, "payload_en_o", payload_en_o),
    (dphy_rx.OffLpEn, "lp_en_o", lp_en_o),
    (dphy_rx.OffLpAvEn, "lp_av_en_o", lp_av_en_o),
    (dphy_rx.OffSpEn, "sp_en_o", sp_en_o),
    (dphy_rx.OffPayloadCrcVld, "payload_crcvld_o", payload_crcvld_o),
    (dphy_rx.OffFifoOvflw, "fifo_ovflw_err_o", fifo_ovflw_err_o),
    (dphy_rx.OffRxqueFull, "rxque_full_o", rxque_full_o),
    (dphy_rx.OffRxfullfr0, "rxfullfr0_o", rxfullfr0_o),
    (dphy_rx.OffRxfullfr1, "rxfullfr1_o", rxfullfr1_o),
    (dphy_rx.OffCrcCheck, "crc_check_o", crc_check_o),
    (dphy_rx.OffCrcError, "crc_error_o", crc_error_o)
  )

  def pulseCc(clockIn: ClockDomain, clockOut: ClockDomain): CsiErrorPulses = {
    val ret = new CsiErrorPulses
    items.zip(ret.items).foreach { case ((_, _, src), (_, _, dst)) =>
      dst := PulseCCByToggle(src, clockIn, clockOut)
    }
    ret
  }
}

class dphy_rx(cfg : MIPIConfig,
              enable_packet_parser : Boolean = true,
              enable_misc_signals : Boolean = true,
              enable_fifo_misc_signals : Option[Boolean] = None,
              enable_logging : Boolean = true,
              sync_cd: ClockDomain = null,
              byte_cd: ClockDomain = null,
              clock_suffix : Boolean = true,
              cfg_datsettle_cyc : Boolean = true,
              cfg_fifo_read_delay : Boolean = true,
              var ip_name : String = null,
              is_continous_clock : Option[Boolean] = None,
              /** When false, Soft-DPHY is the no-LMMI variant; encoded in [[ip_name]]. */
              with_lmmi : Boolean = true,
              /** Soft-DPHY RX FIFO. Independent of continuous clock; default off. */
              with_rx_fifo : Boolean = false,
              /** Soft-DPHY CRC_CHECK_IN. When false, crc_check_o / crc_error_o are omitted. */
              enable_crc_check : Boolean = true,
              /** Soft-DPHY LANE_ALIGN (2-lane deskew). Not encoded in [[ip_name]]; must match .cfg. */
              with_lane_align : Boolean = true) extends BlackBox {
  val is_soft_phy = true
  val config_for_continous_clock = is_continous_clock.getOrElse(byte_cd == null)

  val byte_freq: HertzNumber = cfg.dphyByteFreq
  val byte_cd_freq = if(byte_cd != null) byte_cd.frequency.getValue else byte_freq
  val rx_line_rate = cfg.rx_line_rate
  val dphy_clk_freq = rx_line_rate / 2

  if(is_soft_phy && rx_line_rate > (1034 MHz)) {
    println(s"Warning: dphy_rx rx line rate of ${rx_line_rate} is too fast for some grades of device.")
  }

  val cfg_has_fifo = with_rx_fifo
  // Soft-DPHY 2.0.0 LMMI variant does not expose RX_FIFO_MISC pins; settle/pktdly/ref_dt
  // go through LMMI instead of parallel CSR ports.
  val _enable_fifo_misc_signals =
    if (with_lmmi) false else enable_fifo_misc_signals.getOrElse(cfg_has_fifo)
  val _parallel_csr_ports = !with_lmmi

  if(ip_name == null) {
    def clockString(f : HertzNumber): String = {
      val (base, units) = f.decompose
      var baseString = f"$base%.3f"
      while(baseString.endsWith("0"))
        baseString = baseString.stripSuffix("0")
      baseString = baseString.stripSuffix(".").replace(".", "p")
      f"${baseString}${units}"
    }
    val sync_f = if(sync_cd == null || sync_cd.frequency.isInstanceOf[UnknownFrequency]) "" else s"_sync${clockString(sync_cd.frequency.getValue)}"
    val byte_f = s"_byte${clockString(byte_freq)}"
    val clock_suffix_str = if(clock_suffix) s"${sync_f}${byte_f}" else ""
    val cont_string = if (config_for_continous_clock) "cont_" else ""
    // Prefer nolmmi_ suffix when LMMI is off (Radiant IP naming on this branch);
    // default-with-LMMI keeps the historical unsuffixed Soft-DPHY name.
    // RX FIFO / LANE_ALIGN are independent (YAML withRxFifo / withLaneAlign); not encoded in the name.
    val lmmi_string = if (with_lmmi) "" else "nolmmi_"
    ip_name = s"dphy_rx_${cont_string}${lmmi_string}${cfg.numRXLanes}x${cfg.rxGear}${clock_suffix_str}"
  }

  def solve_datasettle(byte_freq : HertzNumber, ui_freq : HertzNumber): Int = {
    val TCLK_BYTE = byte_freq.toTime

    // Note: The 0.5 is an attempt to match what the radiant UI does, this isn't from the datasheet.
    val UI = ui_freq.toTime / 2
    (((85 ns) + (UI * 6)).toDouble  / TCLK_BYTE.toDouble + 0.5).ceil.toInt - (if(is_soft_phy) 3 else 0)
  }

  val default_datsettlecyc = cfg.dataSettleCyc.getOrElse(solve_datasettle(byte_freq, dphy_clk_freq))
  /** Match Radiant no-LMMI RX_FIFO_PKT_DLY when the dyn pin is absent. */
  val default_pktdly = if (cfg_has_fifo) dphy_rx.DefaultPktDly else 0

  val io = new Bundle {
    /** Soft-DPHY LMMI clock / reset (host / Wishbone domain). */
    val lmmi_clk_i = with_lmmi generate in(Bool())
    val lmmi_resetn_i = with_lmmi generate in(Bool())
    /**
     * Soft-DPHY LMMI register port. Leaf names match Lattice Soft-DPHY 2.0.0
     * (`lmmi_request_i`, …) — flattened (no `lmmi_` parent prefix).
     */
    val lmmi = with_lmmi generate {
      val b = slave(LMMI(8, 8))
      b.setPartialName("")
      b.cmd.valid.setName("lmmi_request_i")
      b.cmd.ready.setName("lmmi_ready_o")
      b.cmd.write.setName("lmmi_wr_rdn_i")
      b.cmd.offset.setName("lmmi_offset_i")
      b.cmd.data.setName("lmmi_wdata_i")
      b.rsp.valid.setName("lmmi_rdata_valid_o")
      b.rsp.payload.setName("lmmi_rdata_o")
      b
    }
    /**
     * This signal is tied to 0 when it is not exposed.
     * Drive this signal when it is exposed:
     * • 1’b0 – No extended virtual channel ID; uses 24-bit
     * Hamming code.
     * • 1’b1 – Packet header ECC byte[7:6] is used as
     * extended virtual channel ID; uses 26-bit Hamming
     * code.
     * Absent on Soft-DPHY LMMI variant (programmed via LMMI).
     */
    val rxcsr_vcx_on_i = (enable_packet_parser && _parallel_csr_ports) generate (in(Bool()) default (False))

    /**
     * This signal is tied to 0 when it is not exposed.
     * Drive this signal when it is exposed:
     * • 1’b0 – Null and Blanking packets trigger an
     * assertion of lp_en. Payload is also transmitted
     * out. The output signal lp_av_en stays low.
     * • 1’b1 – Null and Blanking packets are ignored by
     * the IP
     * Absent on Soft-DPHY LMMI variant (programmed via LMMI).
     */
    val rxcsr_dropnull_i = (enable_packet_parser && _parallel_csr_ports) generate (in(Bool()) default (False))
    val pll_lock_i = in Bool()
    val sync_clk_i = in Bool()
    val sync_rst_i = in Bool()
    /**
     * Indicates the state of gddr_sync.
     * Default is 1’d0.
     */
    val ready_o = out Bool()

    val clk_byte_o = out Bool()
    val clk_byte_hs_o = out Bool()

    /**
     * Low asserted reset for the nets in the clk_lp_hs_ctrl
     * clock domain. The signal driving this port must be
     * synchronized to the clk_lp_hs_ctrl.
     */
    val reset_lp_n_i = !config_for_continous_clock generate in(Bool())
    /**
     * Clocks the logic that detects the Rx D-PHY clock lane
     * LP <-> HS transitions. The minimum frequency for
     * clk_lp_ctrl_i is 40 MHz, as the minimum TLPX is 50 ns
     * (1/25 ns = 40 MHz).
     */
    val clk_lp_ctrl_i = !config_for_continous_clock generate in(Bool())

    /**
     * Continuously running byte clock. This is div8 (in Gear
     * 16) or div4 (in Gear 8) of the input D-PHY clock. This
     * also clocks the logic that detects the Rx D-PHY data
     * lane transitions (lp_hs_ctrl_d0-3 modules). This is used
     * by the word_align, lane_align, and capture_control
     * modules. Payload output is also in this clock domain.
     */
    val clk_byte_fr_i = in Bool()
    val reset_n_i = in Bool()
    val reset_byte_fr_n_i = in Bool()
    val clk_p_io = inout(Analog(Bool()))
    val clk_n_io = inout(Analog(Bool()))
    val d_p_io = inout(Analog(Bits(cfg.numRXLanes bits)))
    val d_n_io = inout(Analog(Bits(cfg.numRXLanes bits)))
    val lp_d_rx_p_o = out(Analog(Bits(cfg.numRXLanes bits)))
    val lp_d_rx_n_o = out(Analog(Bits(cfg.numRXLanes bits)))
    val bd_o = out(Bits(cfg.GEARED_LANES bits))

    /**
     * Indicates the successful detection of the
     * synchronization code ‘B8 in the data lanes. This signal
     * asserts from the start of synchronization pattern ‘B8 up
     * to the last data captured before detecting LP-11 state
     * of any lane (for Soft D-PHY) or data lane 0 (for Hard DPHY).
     * Default is 1’d0
     */
    val hs_sync_o = out Bool()

    val tx_rdy_i = in(Bool()) default (True)

    /**
     * Controls the tHS-SETTLE protocol timing parameter.
     * Check the t-HSZERO parameter of the D-PHY
     * transmitter to ensure the tHS-SETTLE setting can
     * properly detect the Start-of-Transmit pattern.
     */
    /** Parallel settle; Soft-DPHY LMMI variant uses LMMI reg 0x36 instead. */
    val rxcsr_datsettlecyc_i =
      (cfg_datsettle_cyc && _parallel_csr_ports) generate (in(UInt(8 bits)) default (default_datsettlecyc))

    /** Parallel pktdly; Soft-DPHY LMMI variant uses LMMI regs 0x37/0x38 instead. */
    val rxcsr_rxfifo_pktdly_i =
      (cfg_fifo_read_delay && cfg_has_fifo && _parallel_csr_ports) generate (in(UInt(16 bits)) default (default_pktdly))

    def byte_clock_domain(): ClockDomain = {
      if(byte_cd == null) {
        new ClockDomain(clk_byte_hs_o, reset_byte_fr_n_i, config = ClockDomain.current.config.copy(resetKind = SYNC, resetActiveLevel = LOW),
          frequency = FixedFrequency(byte_freq))
      } else {
        byte_cd
      }
    }

    val packet_parser = enable_packet_parser generate new Bundle {
      /**
       * Asserts with lp_en_o if long packet received is the
       * same as the input reference data type ref_dt_i.
       */
      val lp_av_en_o = out Bool()

      /**
       * Signifies the arrival of long packet data. This asserts
       * when a valid long packet data type is received.
       */
      val lp_en_o = out Bool()

      /**
       * Signifies the arrival of valid payload data without the
       * CRC.
       * Default is 1’d0.
       */
      val payload_en_o, sp_en_o = out Bool()

      val payload_o = out(Bits(cfg.GEARED_LANES bits))
      val dt_o = out(UInt(6 bits))
      val ecc_o = out(Bits(6 bits))
      val vc_o, vcx_o = out(UInt(2 bits))
      val wc_o = out(UInt(16 bits))
      val payload_bytevld_o = out(Bits(8 bits))
      val payload_crc_o = out(Bits(16 bits))
      val payload_crcvld_o = out Bool()

      /**
       * Soft-DPHY CRC_CHECK status (present when CRC_CHECK_IN is enabled).
       * crc_check_o pulses when a long-packet CRC is evaluated;
       * crc_error_o asserts with a CRC mismatch.
       */
      val crc_check_o = enable_crc_check generate (out Bool())
      val crc_error_o = enable_crc_check generate (out Bool())

      val ecc_info = master(Flow(Vec(Bool(), 3)))
      ecc_info.valid.setName("ecc_check_o")
      ecc_info.payload(0).setName("ecc_1bit_error_o")
      ecc_info.payload(1).setName("ecc_2bit_error_o")
      ecc_info.payload(2).setName("ecc_byte_error_o")

      /** Parallel ref_dt; Soft-DPHY LMMI variant uses LMMI reg 0x27 instead. */
      val ref_dt_i = _parallel_csr_ports generate (in(Bits(6 bits)) default(cfg.refDt.id))
    }.setPartialName("")

    val dphy_rxdatawidth_hs_o = out(Bits(cfg.numRXLanes bits))
    val dphy_cfg_num_lanes_o = out(Bits(2 bits))

    val misc_signals = enable_misc_signals generate new Bundle {
      /**
       * Active-high enable signal for the line termination of the D-PHY
       * clock lane. This is asserted on detection of transition from
       * LP-11 to LP-01 of the clock lane, and de-asserted upon
       * detection of LP-11 after a high-speed mode.
       * Default is 1’d1 if D-PHY Clock Mode == Continuous and 1’d0 if
       * D-PHY Clock Mode == Non-Continuous.
       */
      val term_clk_en_o = out Bool()

      /**
       * Active-high enable signal for the line termination of the D-PHY
       * clock lane. This is asserted on detection of transition from
       * LP-11 to LP-01 of the lanes, and de-asserted upon detection of
       * LP-11 after a high-speed mode.
       * Default is {NUM_LANES{1’d0}}.
       */
      val term_d_en_o = out Bits(cfg.numRXLanes bits)

      /**
       * Active-high high-speed mode enable signal for data lane d0.
       * For Hard D-PHY IP, this signal is also used for HS mode enable
       * for other data lanes.
       * Default is 1’d0.
       */
      val hs_d_en_o = out Bool()

      /**
       * Contention detection indicator on lane 0.
       */
      val cd_d0_o = out Bool()


      /**
       * Contention detection indicator on clock lane
       */

      val cd_clk_o = out Bool()
      /**
       * 2-bit state encoding of the D-PHY clock controller:
       * 2'b00 – Idle state
       * 2'b01 – LP11 state
       * 2'b10 – LP01 state
       * 2'b11 – HS state
       * Default is 2’d0
       */
      val lp_hs_state_clk_o = out(Bits(2 bits))

      /**
       * 2-bit state encoding of the D-PHY data lane 0 controller:
       * 2'b00 – Idle state
       * 2'b01 – LP11 state
       * 2'b10 – LP01 state
       * 2'b11 – HS state
       * Default is 2’d0.
       */
      val lp_hs_state_d_o = out Bits(2 bits)
    }.setPartialName("")

    val fifo_misc_signals = _enable_fifo_misc_signals generate new Bundle {
      /**
       * State Machine for reading data from FIFO.
       * SINGLE Mode:
       * 2’b00 – IDLE state
       * 2’b01 – Read data from buffer instance 0
       * QUEUE Mode:
       * 2’b00 – IDLE state
       * 2’b01 – Read data from buffer instance 0
       * 2’b11 – Read data done
       * PINGPONG Mode:
       * 2’b00 – IDLE state
       * 2’b01 – Read data from buffer instance 0
       * 2’b10 – Read data from buffer instance 1
       * 2’b11 – Read data done
       * Default is 2’d0.
       */
      val rxdatsyncfr_state_o = out Bits(2 bits)

      /**
       * FIFO empty flag of instance 0/1.
       * Default is 1’d1.
       */
      val rxemptyfr0_o, rxemptyfr1_o = out Bool()

      /**
       * FIFO full of instance 0/1.
       * Default is 1’d0.
       */
      val rxfullfr0_o, rxfullfr1_o = out Bool()

      /**
       * State Machine of RX Queue:
       * 2’b00 – IDLE state
       * 2’b01 – Pop entry from queue
       * 2’b10 – Wait for read data from buffer is done
       * 2’b11 – One delay cycle before Idle
       * Default is 2’d0.
       */
      val rxque_curstate_o = out Bits(2 bits)

      /**
       * RX Queue empty flag.
       * Default is 1’d1.
       */
      val rxque_empty_o = out Bool()

      /**
       * RX Queue full flag.
       * Default is 1’d0.
       */
      val rxque_full_o = out Bool()

      /**
       * An error flag that indicates a write happened when there is still
       * an outstanding transfer in the RX FIFO. This flag is cleared
       * when a new HS transfer happens.
       * Default is 1’d0
       */
      val fifo_dly_err_o = out Bool()

      /**
       * An error flag that indicates a read happened when the FIFO is
       * empty. This happens if the TX clock is faster than RX clock and
       * there is not enough data in the FIFO. This flag is cleared when
       * a new HS transfer happens. Increase the FIFO delay setting to
       * give time for data to accumulate in the buffer.
       * Default is 1’d0.
       */
      val fifo_undflw_err_o = out Bool()

      /**
       * An error flag that indicates a write happens when the FIFO is
       * full. This happens if the TX cannot flush out the FIFO fast
       * enough. This flag is cleared when a new HS transfer happens.
       * Decrease the delay setting, increase the FIFO depth, or both.
       * Default is 1’d0.
       */
      val fifo_ovflw_err_o = out Bool()

    }.setPartialName("")
  }

  def assignMIPI(mipi : MIPIIO) = {
    io.clk_p_io := mipi.clk_p
    io.clk_n_io := mipi.clk_n
    io.d_p_io := mipi.data_p
    io.d_n_io := mipi.data_n
  }

  def MIPIPacketHeader = {
    val bytes = new Flow(new MIPIPacketHeader())
    bytes.virtual_channel_ext := io.packet_parser.vcx_o
    bytes.ecc := io.packet_parser.ecc_o
    bytes.checksum := io.packet_parser.payload_crc_o
    bytes.datatype := io.packet_parser.dt_o
    bytes.virtual_channel := io.packet_parser.vc_o
    bytes.word_count := io.packet_parser.wc_o
    bytes.valid := io.packet_parser.lp_en_o || io.packet_parser.sp_en_o
    bytes.is_long_packet := io.packet_parser.lp_en_o
    bytes.is_long_av_packet := io.packet_parser.lp_av_en_o
    bytes.setName("MIPIPacketHeader")
  }

  def MIPIBytes = {
    val bytes = new Flow(Bits(cfg.GEARED_LANES bits))
    bytes.payload := io.packet_parser.payload_o
    bytes.valid := io.packet_parser.payload_en_o
    bytes
  }

  def attachClockDomains(sync_cd: ClockDomain, byte_cd: ClockDomain): Unit = {
    if(sync_cd != null) {
      io.reset_n_i := ~sync_cd.isResetActive
      io.sync_clk_i := sync_cd.readClockWire
      io.sync_rst_i := sync_cd.isResetActive
    }

    if(byte_cd != null) {
      io.reset_byte_fr_n_i := ~byte_cd.isResetActive
      io.clk_byte_fr_i := byte_cd.readClockWire
      if(io.clk_lp_ctrl_i != null)
        io.clk_lp_ctrl_i := byte_cd.readClockWire
      if(io.reset_lp_n_i != null)
        io.reset_lp_n_i := ~byte_cd.isResetActive
    } else {
      io.reset_byte_fr_n_i := ~AsyncToSyncReset(io.clk_byte_hs_o, sync_cd.isResetActive)
      io.clk_byte_fr_i := io.clk_byte_hs_o
    }
  }

  def byte_cd(): ClockDomain = {
    io.byte_clock_domain()
  }

  noIoPrefix()
  setDefinitionName(ip_name)

  // Soft-DPHY recovered HS byte clock - always register for SDC.
  // Continuous mode: this is the freerunning byte clock as well.
  // Discontinuous (HS_LP): freerunning byte comes from PLL (additionalClocks);
  // clk_byte_hs_o is still a real HS-domain clock (ClockMeasure, Soft-DPHY HS path).
  addPrePopTask(() => {
    Constraints.create_clock(io.clk_byte_hs_o, byte_freq)
  })

  Component.push(parent)
  attachClockDomains(sync_cd, byte_cd)

  enable_logging generate new ClockingArea(byte_cd()) {
    GlobalLogger(
      FlowLogger.flows(MIPIPacketHeader),
      SignalLogger.concat("MIPI_misc_debug" + (if (io.fifo_misc_signals != null) "_w_fifo" else ""),
        io.dphy_cfg_num_lanes_o, io.dphy_rxdatawidth_hs_o,
        io.ready_o,
        io.misc_signals,
        if(io.fifo_misc_signals != null) io.fifo_misc_signals.elements.filterNot(_._1.contains("empty")) else Seq()
      ),
      if(io.fifo_misc_signals != null) {
        SignalLogger.concat("MIPI_misc_error",
          io.fifo_misc_signals.elements.filterNot(e => e._1.contains("empty") || e._1.contains("state"))
        )
      } else Seq()
    )
  }

  /**
   * Fixed CSI Soft-DPHY CSR map (byte offsets from window base).
   * Keep in sync with Zephyr tvai_csi.h.
   *
   *   +0x00 signature
   *   +0x04 / +0x08 clk_meas and +0x18.. error counters: mapped by the camera
   *           CSR helper (shared, muxed by cam_sel @ +0x70). withErrorCounters ignored.
   *   +0x0c rxcsr_datsettlecyc (RW)
   *   +0x10 ref_dt (RW) — filter for lp_av_en_o
   *   +0x14 rxcsr_rxfifo_pktdly (RW)
   *   +0x64 last_lp_dt (RO) — last long-packet dt_o on the wire
   *   +0x68 / +0x6c crc_check_o / crc_error_o (WC via the shared counter bank)
   */
  def attach_bus(busSlaveFactory: BusIf, withErrorCounters: Boolean = true): Unit = {
    Component.current.withAutoPull()
    withAutoPull()

    val base = busSlaveFactory.getRegPtr()

    val sig_reg = busSlaveFactory.newRegAt(base + dphy_rx.OffSig, "dphy sig")(SymbolName("dphy_sig"))
    val signature = sig_reg.field(Bits(32 bit), ROV, BigInt("F000A802", 16), "ip sig")

    def crossClock(reg: RegInst, field: UInt, newClock: ClockDomain, init: Int): UInt = {
      val stream = Stream(cloneOf(field))
      stream.valid := reg.hitDoWrite
      stream.payload := field
      val toggledCC = stream.ccToggle(field.clockDomain, newClock)
      new ClockingArea(io.byte_clock_domain()) {
        toggledCC.ready := RegNext(toggledCC.valid)
        val r = RegNextWhen(toggledCC.payload, toggledCC.valid, init = U(init))
      }.r
    }

    if (io.rxcsr_datsettlecyc_i != null) {
      val dphy_data_ctrl = busSlaveFactory.newRegAt(base + dphy_rx.OffDatSettle, "rxcsr_datsettlecyc")(SymbolName("rxcsr_datsettlecyc"))
      val dphy_data_settle =
        dphy_data_ctrl.field(io.rxcsr_datsettlecyc_i.clone(), RW,
          "Controls the tHS-SETTLE protocol timing parameter. Check the t-HSZERO parameter of the D-PHY transmitter to ensure the tHS-SETTLE setting can properly detect the Start-of-Transmit pattern.") init (default_datsettlecyc)
      GlobalSignals.externalize(io.rxcsr_datsettlecyc_i) :=
        crossClock(dphy_data_ctrl, dphy_data_settle, io.byte_clock_domain(), default_datsettlecyc)
    } else {
      val dphy_data_ctrl = busSlaveFactory.newRegAt(base + dphy_rx.OffDatSettle, "rxcsr_datsettlecyc")(SymbolName("rxcsr_datsettlecyc"))
      val dphy_data_settle = dphy_data_ctrl.field(UInt(8 bits), RO,
        "Soft-IP DATA_SETTLE_CYC (no DYN_DATSETTLE pin)")
      dphy_data_settle := U(default_datsettlecyc, 8 bits)
    }

    if (enable_packet_parser) {
      if (io.packet_parser.ref_dt_i != null) {
        val default_ref_dt = cfg.refDt.id
        val ref_dt_ctrl = busSlaveFactory.newRegAt(base + dphy_rx.OffRefDt, "ref_dt")(SymbolName("ref_dt"))
        val ref_dt = ref_dt_ctrl.field(UInt(io.packet_parser.ref_dt_i.getWidth bits), RW,
          "MIPI reference data type. Long packets whose data type matches this value assert lp_av_en_o. Resets to the refDt from the config.") init (default_ref_dt)
        GlobalSignals.externalize(io.packet_parser.ref_dt_i) :=
          crossClock(ref_dt_ctrl, ref_dt, io.byte_clock_domain(), default_ref_dt).asBits
      }

      /*
       * Observed long-packet DT from the sensor (Soft-DPHY dt_o @ lp_en_o).
       * Independent of ref_dt — use to verify shrimp MIPI DT vs filter.
       */
      val last_lp_dt_reg = busSlaveFactory.newRegAt(base + dphy_rx.OffLastLpDt, "last_lp_dt")(
        SymbolName("last_lp_dt"))
      val last_lp_dt = last_lp_dt_reg.field(UInt(6 bits), RO,
        "Last MIPI data type from a long packet (wire dt_o when lp_en_o). Compare to ref_dt.")
      val lastLpDtByte = new ClockingArea(io.byte_clock_domain()) {
        val r = Reg(UInt(6 bits)) init 0
        when(io.packet_parser.lp_en_o) {
          r := io.packet_parser.dt_o
        }
        r
      }.r
      last_lp_dt := BufferCC(lastLpDtByte)
    }

    if (io.rxcsr_rxfifo_pktdly_i != null) {
      val pktdelay_ctrl = busSlaveFactory.newRegAt(base + dphy_rx.OffPktDly, "rxcsr_rxfifo_pktdly")(SymbolName("rxcsr_rxfifo_pktdly"))
      val pktdelay =
        pktdelay_ctrl.field(io.rxcsr_rxfifo_pktdly_i.clone(), RW, "Packet delay on fifo") init (default_pktdly)
      GlobalSignals.externalize(io.rxcsr_rxfifo_pktdly_i) :=
        crossClock(pktdelay_ctrl, pktdelay, io.byte_clock_domain(), default_pktdly)
    } else {
      val pktdelay_ctrl = busSlaveFactory.newRegAt(base + dphy_rx.OffPktDly, "rxcsr_rxfifo_pktdly")(SymbolName("rxcsr_rxfifo_pktdly"))
      val pktdelay = pktdelay_ctrl.field(UInt(16 bits), RO,
        "Soft-IP RX_FIFO_PKT_DLY (no DYN_RXFIFO_PKTDLY pin)")
      pktdelay := U(default_pktdly, 16 bits)
    }

    val _ = withErrorCounters
  }

  /**
   * Rise-detect Soft-DPHY status in the byte clock (shared CSI error bank).
   * ECC/CRC error flags are only valid while *_check_o is asserted (Lattice IPUG).
   */
  def errorPulsesInByteClock(): CsiErrorPulses = {
    val p = new CsiErrorPulses
    val byteCd = io.byte_clock_domain()
    new ClockingArea(byteCd) {
      def rise(sig: Bool): Bool = sig.rise(False)

      p.hs_sync_rise := rise(io.hs_sync_o)
      if (io.misc_signals != null) {
        p.term_clk_en_rise := rise(io.misc_signals.term_clk_en_o)
        p.term_d_en_o0 := rise(io.misc_signals.term_d_en_o(0))
        p.hs_d_en_o := rise(io.misc_signals.hs_d_en_o)
        p.cd_clk_o := rise(io.misc_signals.cd_clk_o)
        p.cd_d0_o := rise(io.misc_signals.cd_d0_o)
      } else {
        p.term_clk_en_rise := False
        p.term_d_en_o0 := False
        p.hs_d_en_o := False
        p.cd_clk_o := False
        p.cd_d0_o := False
      }

      if (io.packet_parser != null) {
        val eccCheck = io.packet_parser.ecc_info.valid
        p.ecc_check_o := rise(eccCheck)
        p.ecc_1bit_error_o := rise(eccCheck && io.packet_parser.ecc_info.payload(0))
        p.ecc_2bit_error_o := rise(eccCheck && io.packet_parser.ecc_info.payload(1))
        p.ecc_byte_error_o := rise(eccCheck && io.packet_parser.ecc_info.payload(2))
        p.payload_en_o := rise(io.packet_parser.payload_en_o)
        p.lp_en_o := rise(io.packet_parser.lp_en_o)
        p.lp_av_en_o := rise(io.packet_parser.lp_av_en_o)
        p.sp_en_o := rise(io.packet_parser.sp_en_o)
        p.payload_crcvld_o := rise(io.packet_parser.payload_crcvld_o)
        if (io.packet_parser.crc_check_o != null) {
          val crcCheck = io.packet_parser.crc_check_o
          p.crc_check_o := rise(crcCheck)
          p.crc_error_o := rise(crcCheck && io.packet_parser.crc_error_o)
        } else {
          p.crc_check_o := False
          p.crc_error_o := False
        }
      } else {
        p.ecc_check_o := False
        p.ecc_1bit_error_o := False
        p.ecc_2bit_error_o := False
        p.ecc_byte_error_o := False
        p.payload_en_o := False
        p.lp_en_o := False
        p.lp_av_en_o := False
        p.sp_en_o := False
        p.payload_crcvld_o := False
        p.crc_check_o := False
        p.crc_error_o := False
      }

      if (io.fifo_misc_signals != null) {
        p.fifo_ovflw_err_o := rise(io.fifo_misc_signals.fifo_ovflw_err_o)
        p.rxque_full_o := rise(io.fifo_misc_signals.rxque_full_o)
        p.rxfullfr0_o := rise(io.fifo_misc_signals.rxfullfr0_o)
        p.rxfullfr1_o := rise(io.fifo_misc_signals.rxfullfr1_o)
      } else {
        p.fifo_ovflw_err_o := False
        p.rxque_full_o := False
        p.rxfullfr0_o := False
        p.rxfullfr1_o := False
      }
    }
    p
  }
}

object dphy_rx {
  val OffSig = 0x00
  val OffClkMeasLong = 0x04
  val OffClkMeasShort = 0x08
  val OffDatSettle = 0x0c
  val OffRefDt = 0x10
  val OffPktDly = 0x14
  val OffHsSyncRise = 0x18
  val OffTermClkEn = 0x1c
  val OffTermD0 = 0x20
  val OffHsDEn = 0x24
  val OffCdClk = 0x28
  val OffCdD0 = 0x2c
  val OffEccCheck = 0x30
  val OffEcc1bit = 0x34
  val OffEcc2bit = 0x38
  val OffEccByte = 0x3c
  val OffPayloadEn = 0x40
  val OffLpEn = 0x44
  val OffLpAvEn = 0x48
  val OffSpEn = 0x4c
  val OffPayloadCrcVld = 0x50
  val OffFifoOvflw = 0x54
  val OffRxqueFull = 0x58
  val OffRxfullfr0 = 0x5c
  val OffRxfullfr1 = 0x60
  /** RO: last long-packet MIPI DT observed on the wire (after WC block @ 0x60). */
  val OffLastLpDt = 0x64
  /** WC: Soft-DPHY CRC evaluation / mismatch pulses (CRC_CHECK_IN). */
  val OffCrcCheck = 0x68
  val OffCrcError = 0x6c
  /** RW: selects which camera feeds the shared error-counter / clk_meas bank. Change clears. */
  val OffCamSel = 0x70
  val WindowBytes = 0x100
  /** Wishbone window for Soft-DPHY LMMI (256 byte offsets × 4-byte word spacing). */
  val LmmiWindowBytes = 0x400
  /** Radiant RX_FIFO_PKT_DLY for no-LMMI FIFO (create-radiant-project.py). */
  val DefaultPktDly = 16
}