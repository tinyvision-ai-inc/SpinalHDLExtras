package spinalextras.lib.lattice
import scala.io.Source
import spinal.core._
import spinalextras.lib.Constraints
import spinalextras.lib.soc.spinex.plugins.IdentificationPlugin

import java.io.{File, PrintWriter}
import java.util.Calendar

object IPX {
  def generate_ipx[T <: Component](report : SpinalReport[T]): Unit = {

    val file = new PrintWriter(s"${report.globalData.config.targetDirectory}/${report.toplevelName}.ipx")

    file.write(
      s"""<?xml version="1.0" ?>
        |<RadiantModule generator="ipgen" module="${report.toplevel.definitionName}" name="${report.toplevelName}" source_format="Verilog" version="${IdentificationPlugin.getGitVersion()}" date="${Calendar.getInstance().getTime}">
        | <Package>
        |""".stripMargin)

    val names = report.generatedSourcesPaths.map { elem =>
      elem.substring(report.globalData.config.targetDirectory.length + 1)
    }
    names.foreach { name =>
      file.write(s"""  <File name="${name}" type="top_level_verilog"/>\n""")
    }
    // IPGenerator writes {top}_top.sv after SpinalReport; it is not in generatedSourcesPaths.
    val topSv = s"${report.toplevelName}_top.sv"
    if (!names.contains(topSv) && new File(s"${report.globalData.config.targetDirectory}/$topSv").exists()) {
      file.write(s"""  <File name="${topSv}" type="top_level_verilog"/>\n""")
    }

    val sdc_file = s"${report.toplevelName}.sdc"
    val targetDir = report.globalData.config.targetDirectory
    Constraints.write_file(report, s"$targetDir/$sdc_file")

    val map_pdc = s"${report.toplevelName}_map_constraints.pdc"
    Constraints.write_map_pdc_file(report, s"$targetDir/$map_pdc")
    // Legacy chip-top stub; CDC is IP SDC + board merge (not a second POSTSYN SDC).
    new File(s"$targetDir/${report.toplevelName}_chip_constraints.sdc").delete()
    new File(s"$targetDir/${report.toplevelName}_map_constraints.tcl").delete()

    file.write(
      s"""
        |  <File name="${sdc_file}" type="tcl_constraints" stage="${sdc_file}=lse:presyn,synplify:presyn"/>
        | </Package>
        |</RadiantModule>
        |""".stripMargin)

    file.close()
  }
}