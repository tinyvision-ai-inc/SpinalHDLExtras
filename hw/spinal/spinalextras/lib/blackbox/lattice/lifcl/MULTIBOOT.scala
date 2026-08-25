package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._

import scala.language.postfixOps

/**
 * Nexus MULTIBOOT primitive. With SOURCESEL="EN", MSPIMADDR is the SPI flash
 * byte address used on the next PROGRAMN / LSC_REFRESH. AUTOREBOOT is unused
 * and must be tied low. Instantiating this disables the static Deployment-Tool
 * next-pattern walk.
 */
class MULTIBOOT(sourceSel: String = "EN") extends BlackBox {
  addGeneric("SOURCESEL", sourceSel)
  addAttribute("syn_noprune", "1")

  val io = new Bundle {
    val AUTOREBOOT = in Bool()
    val MSPIMADDR = in UInt (32 bits)
  }

  noIoPrefix()
}

object MULTIBOOT {
  def apply(bootAddr: UInt): MULTIBOOT = {
    val u = new MULTIBOOT()
    u.io.AUTOREBOOT := False
    u.io.MSPIMADDR := bootAddr
    u
  }
}
