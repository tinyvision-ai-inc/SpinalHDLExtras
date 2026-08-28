package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._

/**
 * Behavioral CONFIG_CLKRST_CORE (Radiant OSC LMMI path). Not the encrypted
 * primitive. LMMI_CLK_O follows LMMI_CLK; LMMI_RST is the active-low
 * passthrough of LMMI_LRST_N. Fabric must not use LMMI_CLK_O as a SLICE clock
 * (map DRC 71003036); sim may still observe CLK_O == CLK.
 */
class ConfigClkrstCoreModel extends Component {
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

  io.LMMI_CLK_O := io.LMMI_CLK
  io.LMMI_RST := io.LMMI_LRST_N
  io.CFG_CLK := io.OSCCLK
  io.SMCLK_RST := io.LMMI_LRST_N
  io.HSE_CLK := io.OSCCLK
  io.SEDC_RST := io.SEDC_LRST_N
  io.WDT_CLK := False
  io.WDT_RST := io.WDT_LRST_N
}
