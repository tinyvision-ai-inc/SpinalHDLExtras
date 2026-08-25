package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinalextras.lib.bus.LMMI

import scala.language.postfixOps

/**
 * Nexus CONFIG_CLKRST_CORE. Passes fabric LMMI clock/reset into the
 * configuration block (Radiant OSC IP `lscc_osc` when `LMMI_CLK_EN` is on).
 *
 * LMMI_RST is an active-low passthrough of LMMI_LRST_N despite the name.
 */
class CONFIG_CLKRST_CORE extends BlackBox {
  val generic = new Generic {
    val MCJTAGGSRNDIS = "EN"
    val MCLMMIGSRNDIS = "EN"
    val MCSEDCGSRNDIS = "EN"
    val MCWDTGSRNDIS = "EN"
    val SMCLK_DIV = "3"
  }

  val io = new Bundle {
    val HSE_CLK = out Bool()
    val JTAG_LRST_N = in Bool()
    val LMMI_CLK = in Bool()
    val LMMI_CLK_O = out Bool()
    val LMMI_LRST_N = in Bool()
    val LMMI_RST = out Bool()
    val MBISTCLK = in Bool()
    val OSCCLK = in Bool()
    val SEDC_CLK = in Bool()
    val SEDC_LRST_N = in Bool()
    val SEDC_RST = out Bool()
    val CFG_CLK = out Bool()
    val SMCLK_RST = out Bool()
    val WDT_CLK = out Bool()
    val WDT_LRST_N = in Bool()
    val WDT_RST = out Bool()
  }

  noIoPrefix()
}

object CONFIG_CLKRST_CORE {
  def apply(oscClk: Bool, lmmiClk: Bool, lmmiResetn: Bool, sedcClk: Bool = False): CONFIG_CLKRST_CORE = {
    val u = new CONFIG_CLKRST_CORE()
    u.io.OSCCLK := oscClk
    u.io.LMMI_CLK := lmmiClk
    u.io.LMMI_LRST_N := lmmiResetn
    u.io.JTAG_LRST_N := True
    u.io.MBISTCLK := False
    u.io.SEDC_CLK := sedcClk
    u.io.SEDC_LRST_N := True
    u.io.WDT_LRST_N := True
    u
  }
}

/**
 * Nexus CONFIG_LMMI primitive (LIFCL / CrossLink-NX). LMMI host access to the
 * configuration engine. LMMI_EN must be "EN". LMMICLK fmax is 50 MHz.
 *
 * SMCLK / RSTSMCLK are hardware-inferred; connect them from CONFIG_CLKRST_CORE
 * when that block is instantiated.
 */
class CONFIG_LMMI(lmmiEn: String = "EN") extends BlackBox {
  addGeneric("LMMI_EN", lmmiEn)

  val io = new Bundle {
    val LMMICLK = in Bool()
    val LMMIREQUEST = in Bool()
    val LMMIWRRD_N = in Bool()
    val LMMIOFFSET = in UInt (8 bits)
    val LMMIWDATA = in Bits (8 bits)
    val LMMIRDATA = out Bits (8 bits)
    val LMMIREADY = out Bool()
    val LMMIRDATAVALID = out Bool()
    val LMMIRESETN = in Bool()
    val RSTSMCLK = in Bool()
    val SMCLK = in Bool()
  }

  noIoPrefix()

  def attachLmmi(lmmi: LMMI): Unit = {
    io.LMMIREQUEST := lmmi.cmd.valid
    io.LMMIWRRD_N := lmmi.cmd.write
    io.LMMIOFFSET := lmmi.cmd.offset
    io.LMMIWDATA := lmmi.cmd.data
    lmmi.cmd.ready := io.LMMIREADY
    lmmi.rsp.valid := io.LMMIRDATAVALID
    lmmi.rsp.payload := io.LMMIRDATA
  }
}

object CONFIG_LMMI {
  def apply(lmmiClk: Bool, lmmiResetn: Bool, smClk: Bool, rstSmClk: Bool, lmmi: LMMI): CONFIG_LMMI = {
    val u = new CONFIG_LMMI()
    u.io.LMMICLK := lmmiClk
    u.io.LMMIRESETN := lmmiResetn
    u.io.SMCLK := smClk
    u.io.RSTSMCLK := rstSmClk
    u.attachLmmi(lmmi)
    u
  }
}
