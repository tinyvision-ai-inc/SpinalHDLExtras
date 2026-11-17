package spinalextras.lib

import spinal.core._
import spinal.core.internals.{PhaseContext, PhaseNetlist}
import spinal.lib.bus.amba3.apb.Apb3CC
import spinal.lib.bus.amba4.axi.{Axi4CC, Axi4ReadOnlyCC, Axi4WriteOnlyCC}
import spinal.lib.{
  BufferCC,
  FlowCCByToggle,
  FlowCCUnsafeByToggle,
  KeepAttribute,
  PulseCCByToggle,
  StreamCCByToggle,
  StreamFifoCC
}
import spinalextras.lib.misc.Obfuscater

import java.io.PrintWriter
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Constraints {
  val clocks = new mutable.ArrayBuffer[(Data, HertzNumber)]()
  /* dest, source, mul, div, destFreq. mul/div are reduced destHz/sourceHz. */
  val generated_clocks = new mutable.ArrayBuffer[(Data, Data, Long, Long, HertzNumber)]()
  val max_skews = new mutable.ArrayBuffer[(Seq[Data], TimeNumber)]()
  val clock_groups = new mutable.ArrayBuffer[(Seq[Data], Boolean)]()
  val constraints = new mutable.ArrayBuffer[(Seq[Data], Map[String, String])]()
  val false_paths = new ArrayBuffer[Data]()
  val min_delay = new ArrayBuffer[(Seq[Data], TimeNumber)]()

  var false_path_all_clocks = false
  val verbatim_constraints = new ArrayBuffer[() => String]()

  def SetFalsePathAllClocks(): Unit = {
    false_path_all_clocks = true
  }

  private def portLeaf(data: Data): String = {
    data.getRtlPath().split('/').last.replaceAll("\\[.*\\]", "")
  }

  private def isToplevelPort(data: Data, toplevel: Component): Boolean = {
    val leaf = portLeaf(data)
    toplevel.getAllIo.exists(_.getName() == leaf)
  }

  private def clockNameFor(data: Data, toplevel: Component): String = {
    val leaf = portLeaf(data)
    // Soft-DPHY byte clocks share the leaf clk_byte_hs_o; two CSI pipes need
    // distinct create_clock names (RtlPath), not two clocks named clk_byte_hs_o.
    if (isToplevelPort(data, toplevel) || isPllGeneratedDest(data))
      leaf
    else
      data.getRtlPath().replace('/', '_')
  }

  /** Tcl object query for a clock net/pin/port in the emitted Verilog hierarchy. */
  private def clockTarget(data: Data, toplevel: Component): String = {
    val path = data.getRtlPath()
    if (data.component == toplevel) {
      s"[get_ports {$path}]"
    } else {
      s"[get_pins {$path}]"
    }
  }

  // Flatten + obfuscate drop Spinal hierarchy. PLL / Soft-DPHY blackbox pin
  // names (CLKOP, clk_byte_hs_o) and top ports survive; do not glob CLKOS as
  // a net prefix (that also matches CLKOS2 / CLKOS3).
  // Use dest/source RtlPath (e.g. pLL_1/CLKOP), not a hardcoded instance.
  // */CLKOP alone matches AXI CLKOP ports and CPE segfaults.
  private def generatedClockDest(dest: Data, toplevel: Component): String = {
    val leaf = portLeaf(dest)
    if (isPllGeneratedDest(dest)) {
      s"[get_pins -hierarchical {*${dest.getRtlPath()}}]"
    } else if (toplevel.getAllIo.exists(_.getName() == "spiflash_clk") && !isPllGeneratedDest(dest)) {
      "[get_ports {spiflash_clk}]"
    } else if (isToplevelPort(dest, toplevel) || toplevel.getAllIo.exists(_.getName() == leaf)) {
      s"[get_ports {${leaf}}]"
    } else {
      val parts = dest.getRtlPath().split('/')
      if (parts.length >= 2) {
        s"[get_nets -hierarchical {*${parts.head}_${parts.last}}]"
      } else {
        clockTarget(dest, toplevel)
      }
    }
  }

  private def generatedClockSource(dest: Data, source: Data, toplevel: Component): String = {
    if (isPllGeneratedDest(dest)) {
      s"[get_pins -hierarchical {*${source.getRtlPath()}}]"
    } else {
      generated_clocks.collectFirst {
        case (d, _, _, _, _) if portLeaf(d) == "CLKOS3" =>
          s"[get_pins -hierarchical {*${d.getRtlPath()}}]"
      }.orElse {
        if (toplevel.getAllIo.exists(_.getName() == "clk")) Some("[get_ports {clk}]") else None
      }.getOrElse(clockTarget(source, toplevel))
    }
  }

  /* CPE renames IP CLKOP → u_<inst>_CLKOP. Bare names miss; globs match. */
  private def pllClockGetClocksGlob(pllNames: Seq[String]): String = {
    val globs = pllNames.map {
      case "CLKOP" => "*CLKOP"
      case n if n.startsWith("CLKOS") => s"*$n"
      case n => s"*$n"
    }.distinct
    s"[get_clocks {${globs.mkString(" ")}}]"
  }

  private def hierarchicalClockTarget(data: Data, toplevel: Component): String = {
    val leaf = portLeaf(data)
    if (isToplevelPort(data, toplevel)) {
      s"[get_ports {${leaf}}]"
    } else if (isSoftDphyByteClock(leaf)) {
      s"[get_pins -hierarchical {*/${leaf}}]"
    } else {
      s"[get_pins -hierarchical {*/${data.getRtlPath()}}]"
    }
  }

  private def netWildcard(path: String): String = s"[get_nets {${path}/*}]"

  private def skipDuplicateJtagClock(data: Data, toplevel: Component): Boolean = {
    toplevel.getAllIo.exists(_.getName() == "jtag_tck") &&
      (data.hasTag(Constraints.JtagPadClock) || data.getRtlPath().endsWith("jtag_tck"))
  }

  // PLL.scala dests keep CLKOP/CLKOS* (blackbox IO). XipFlashPlugin SCK does not.
  private def isPllGeneratedDest(data: Data): Boolean = {
    val leaf = portLeaf(data)
    leaf == "CLKOP" || leaf == "CLKOS" || leaf == "CLKOS2" || leaf == "CLKOS3" ||
      leaf == "CLKOS4" || leaf == "CLKOS5"
  }

  private def isSoftDphyByteClock(leaf: String): Boolean = {
    leaf == "clk_byte_hs_o" || leaf == "clk_byte_o"
  }

  def write_file[T <: Component](report: SpinalReport[T], path : String): Unit = {
    val file = new PrintWriter(path)

    report.globalData.config.defaultClockDomainFrequency match {
      case f : FixedFrequency => {
        val defaultClock = f.getValue
        // Board SDC owns the pad (clk_in). Nested IP get_ports {clk} is a miss
        // at chip top (clk_2 / clk_sys_60mhz_p). PLL generated clocks use REFCK.
        file.println(s"# clk ${defaultClock.decompose} (board pad clock; IP uses PLL REFCK)")
      }
      case _ => {}
    }

    def hasPort(n: String) = report.toplevel.getAllIo.exists(_.getName() == n)
    def presentPorts(ns: String*) = ns.filter(hasPort)

    for ((data, freq) <- clocks) {
      if (generated_clocks.exists(_._1 == data)) {
        file.println(s"# skip ${clockNameFor(data, report.toplevel)} (generated clock)")
      } else if (skipDuplicateJtagClock(data, report.toplevel)) {
        file.println(s"# skip ${clockNameFor(data, report.toplevel)} (board jtag_tck)")
      } else {
        val cname = clockNameFor(data, report.toplevel)
        file.println(s"# ${cname} ${freq.decompose}")
        file.println(s"create_clock -name {${cname}} -period ${freq.toTime.toDouble * 1e9} ${hierarchicalClockTarget(data, report.toplevel)}")
      }
    }

    for ((dest, source, mul, div, destFreq) <- generated_clocks) {
      val isSpiPad =
        hasPort("spiflash_clk") && !isPllGeneratedDest(dest) && !isSoftDphyByteClock(portLeaf(dest))
      if (isSpiPad) {
        /* Pad SCK (UAB). create_generated_clock from a PLL output keeps SCK
         * related to CLKOS2; CPE then renames CLKOP so the SPI↔PLL async group
         * misses and STA demands a ~3 ns related edge. create_clock + async
         * group is the honest model. */
        val periodNs = destFreq.toTime.toDouble * 1e9
        file.println(s"# spiflash_clk ${destFreq.decompose} (pad; not PLL-generated)")
        file.println(s"create_clock -name {spiflash_clk} -period ${periodNs} [get_ports {spiflash_clk}]")
      } else {
        val cname = clockNameFor(dest, report.toplevel)
        file.println(
          s"create_generated_clock -name {${cname}} -source ${generatedClockSource(dest, source, report.toplevel)} -multiply_by ${mul} -divide_by ${div} ${generatedClockDest(dest, report.toplevel)}"
        )
      }
    }

    if (hasPort("jtag_tck")) {
      file.println("create_clock -name {jtag_tck} -period 100 [get_ports {jtag_tck}]")
    }

    val spiGenNames = generated_clocks
      .filterNot { case (dest, _, _, _, _) => isPllGeneratedDest(dest) }
      .map { case (dest, _, _, _, _) =>
        if (hasPort("spiflash_clk")) "spiflash_clk" else clockNameFor(dest, report.toplevel)
      }
    val pllGenNames = generated_clocks
      .filter { case (dest, _, _, _, _) => isPllGeneratedDest(dest) }
      .map { case (dest, _, _, _, _) => clockNameFor(dest, report.toplevel) }
    val spiClockName = spiGenNames.headOption.orElse(
      if (hasPort("spiflash_clk")) Some("spiflash_clk") else None
    )
    if (spiClockName.nonEmpty && pllGenNames.nonEmpty) {
      file.println(
        s"set_clock_groups -asynchronous -group [get_clocks {${spiClockName.get}}] -group ${pllClockGetClocksGlob(pllGenNames)}"
      )
    }

    // set_clock_uncertainty and pad false_paths (led/uart/i2c) belong on the
    // board SDC. IP emit of get_ports {led} etc. is applied at chip top by CPE
    // and MT447's when the board does not promote that name.

    if (hasPort("jtag_tck")) {
      for (n <- presentPorts("jtag_tdi", "jtag_tms")) {
        file.println(s"set_input_delay -clock [get_clocks {jtag_tck}] -max 10.0 [get_ports {$n}]")
        file.println(s"set_input_delay -clock [get_clocks {jtag_tck}] -min 2.0 [get_ports {$n}]")
      }
      if (hasPort("jtag_tdo")) {
        file.println("set_output_delay -clock [get_clocks {jtag_tck}] -max 10.0 [get_ports {jtag_tdo}]")
        file.println("set_output_delay -clock [get_clocks {jtag_tck}] -min 2.0 [get_ports {jtag_tdo}]")
      }
    }
    // Pad I/O delays belong on the board SDC. Emit them from the IP only when
    // the generated SCK is still the `spiflash_clk` port (unobfuscated top).
    if (spiClockName.contains("spiflash_clk") && hasPort("spiflash_dq")) {
      file.println("set_input_delay -clock [get_clocks {spiflash_clk}] -clock_fall -max 6.0 [get_ports {spiflash_dq*}]")
      file.println("set_input_delay -clock [get_clocks {spiflash_clk}] -clock_fall -min 1.5 [get_ports {spiflash_dq*}]")
      file.println("set_output_delay -clock [get_clocks {spiflash_clk}] -max 2.0 [get_ports {spiflash_dq*}]")
      file.println("set_output_delay -clock [get_clocks {spiflash_clk}] -min -3.0 [get_ports {spiflash_dq*}]")
    }
    if (spiClockName.contains("spiflash_clk") && hasPort("spiflash_cs_n")) {
      file.println("set_output_delay -clock [get_clocks {spiflash_clk}] -max 5.0 [get_ports {spiflash_cs_n}]")
      file.println("set_output_delay -clock [get_clocks {spiflash_clk}] -min -3.0 [get_ports {spiflash_cs_n}]")
    }
    // Do not emit Spinal-hierarchy leftovers (cpol/cpha, brightness_ret,
    // mipi_to_bytes_cd_d0_o_regNext). Those names die at obfuscate/flatten;
    // pad false_paths and CDC globs cover the same intent.

    //    for ((clks, async) <- clock_groups) {
    //      file.println(s"set_clock_groups ${clks.map("-group [get_clocks {" + _.name +"}]").mkString(" ")} ${if(async) "-asynchronous" else ""}")
    //    }

    if (hasPort("jtag_tck") && presentPorts("jtag_tdi", "jtag_tms").size == 2) {
      // Radiant set_max_skew accepts nets, not get_ports. Omit jtag_tdo*:
      // at IP CPE scope that glob is empty and 1026001 (CPE then segfaults).
      file.println("set_max_skew [get_nets {jtag_tck* jtag_tdi* jtag_tms*}] 10.0")
    }
    if (hasPort("spiflash_clk") && hasPort("spiflash_cs_n") && hasPort("spiflash_dq")) {
      file.println("set_max_skew [get_nets {spiflash_clk* spiflash_cs_n* spiflash_dq*}] 1.0")
    }

    for ((datas, delay) <- min_delay) {
      file.println(s"set_min_delay -through [get_nets {${datas.map(_.getRtlPath() + "*").mkString(" ")}}] ${delay.toDouble * 1e9}")
    }

    def set_false_path(d : Data): Unit = {
      KeepAttribute(d)
      d.addAttribute("syn_keep", 1).addAttribute("nomerge", "")
      file.println(s"set_false_path -through ${netWildcard(d.getRtlPath())}")
    }

    for(false_path <- false_paths) {
      set_false_path(false_path)
    }

    // Mark through-TIG CDC leaves (cdc_* + KeepName). Emit only globs that
    // match this top: CPE 1026001 on an empty collection then segfaults.
    // Do not KeepName Axi4CC / StreamFifoCC (blocks flatten). Nested BufferCC
    // in those wrappers still get marked.
    val usedCdcGlobs = scala.collection.mutable.LinkedHashSet[String]()
    var hasFifoCcRam = false
    var hasStreamCcPop = false
    var hasFlowCcPop = false
    Constraints.walkCdcComponents(report.toplevel) {
      case c if Constraints.isCdcThroughLeaf(c) =>
        // Names/KeepName already applied in PhaseCdcAnchor; refresh for SDC.
        Constraints.markCdcAnchor(c)
        usedCdcGlobs ++= Constraints.cdcGlobsForNamed(c)
        c match {
          case _: StreamCCByToggle[_] => hasStreamCcPop = true
          case _: FlowCCByToggle[_] | _: FlowCCUnsafeByToggle[_] => hasFlowCcPop = true
          case _ =>
        }
      case c: StreamFifoCC[_] =>
        Constraints.markFifoRam(c)
        hasFifoCcRam = true
      case _ =>
    }

    for (g <- usedCdcGlobs) {
      file.println(s"set_false_path -through [get_nets -hierarchical {$g}]")
    }
    // BufferCC dest flops (buffers_0 + buffers_1). Synplify escapes '.' in
    // get_pins (…\.ff_inst…); get_cells is what expands in TWR.
    if (usedCdcGlobs.contains("*cdc_BufferCC*")) {
      for (g <- Constraints.bufferCcDestFlopCellGlobs) {
        file.println(s"set_false_path -to [get_cells -hierarchical {$g}]")
      }
    }
    // StreamCCByToggle pop-side payload regs (push clk → pop m2sPipe).
    if (hasStreamCcPop) {
      for (g <- Constraints.streamCcPopDataCellGlobs) {
        file.println(s"set_false_path -to [get_cells -hierarchical {$g}]")
      }
    }
    // FlowCC* output m2sPipe payload (inputArea.data → flow_m2sPipe).
    if (hasFlowCcPop) {
      for (g <- Constraints.flowCcPopDataCellGlobs) {
        file.println(s"set_false_path -to [get_cells -hierarchical {$g}]")
      }
    }
    // Async FIFO inferred-RAM (push addr → pop ram_spinal_port1).
    // */ram_spinal_port1* excludes same-clock logic_ram_spinal_port1.
    if (hasFifoCcRam) {
      for (g <- Constraints.fifoRamCellGlobs) {
        file.println(s"set_false_path -to [get_cells -hierarchical {$g}]")
      }
    }

    // USB23 HIP: AXI/LMMI inputs sampled inside (hold). INTERRUPT false-path
    // lives in the board Soft-DPHY SDC (-hierarchical); IP emit becomes
    // <ip_inst>/*/INTERRUPT and misses nested USB23_1.
    if (report.toplevel.getAllIo.exists(_.getName().startsWith("usb23"))) {
      for (pin <- Constraints.usb23HoldPinGlobs) {
        file.println(s"set_false_path -hold -to [get_pins -hierarchical {$pin}]")
      }
    }

    for ((datas, tags) <- constraints) {
      for(data <- datas) {
        file.println(s"ldc_set_port -iobuf {${tags.map(x => s"${x._1}=${x._2}").mkString(" ")}} [get_ports {${data.getRtlPath()}*}]")
      }
    }

    for (c <- verbatim_constraints) {
      file.println(c())
    }

    file.close()
  }


}

object Constraints {
  var constraints = new Constraints

  var toplevel : Component = null

  /** Same net as top `jtag_tck`. write_file already create_clocks the pad. */
  object JtagPadClock extends SpinalTag

  private def withCdcPrefix(n: String): String =
    if (n.contains("cdc_")) n else s"cdc_$n"

  private def stableCdcName(c: Component): String = {
    val raw = Option(c.getName()).getOrElse(c.getClass.getSimpleName)
    val cleaned = raw.replaceAll("[^A-Za-z0-9_]+", "_").stripPrefix("_")
    c match {
      // setName (not setPartialName): bufferCC_78 must become cdc_BufferCC_78
      // so *cdc_BufferCC* matches. setPartialName leaves the Spinal default.
      case _: BufferCC[_] =>
        if (cleaned.contains("cdc_")) cleaned
        else {
          val suf = cleaned.replaceAll("(?i)^buffercc_?", "")
          if (suf.isEmpty) "cdc_BufferCC" else s"cdc_BufferCC_$suf"
        }
      // Validator / SDC glob is *cdc_*ccToggle*. Unnamed StreamCCByToggle
      // would otherwise become cdc_StreamCCByToggle (no ccToggle substring).
      case _: StreamCCByToggle[_] =>
        val base = withCdcPrefix(if (cleaned.nonEmpty) cleaned else "StreamCC")
        if (base.toLowerCase.contains("cctoggle")) base else s"${base}_ccToggle"
      case _ =>
        if (cleaned.contains("cdc_")) cleaned
        else {
          val base = if (cleaned.nonEmpty) cleaned else c.getClass.getSimpleName
          s"cdc_$base"
        }
    }
  }

  /** Through-TIG CDC leaves: rename to cdc_* + KeepName (survives obfuscate). */
  def isCdcThroughLeaf(c: Component): Boolean = c match {
    case _: BufferCC[_]            => true
    case _: StreamCCByToggle[_]    => true
    case _: FlowCCByToggle[_]      => true
    case _: FlowCCUnsafeByToggle[_] => true
    case _                         => false
  }

  /**
   * CDC wrappers / async FIFOs. Keep hierarchy + KeepName so children and RAM
   * pins stay findable; do not use the cdc_ prefix ( *cdc_* through would TIG
   * same-clock toggle / FIFO payload). Nested BufferCC still get markCdcAnchor.
   */
  def isCdcContainer(c: Component): Boolean = c match {
    case _: StreamFifoCC[_]    => true
    case _: PulseCCByToggle    => true
    case _: Axi4CC             => true
    case _: Axi4ReadOnlyCC     => true
    case _: Axi4WriteOnlyCC    => true
    case _: Apb3CC             => true
    case _                     => false
  }

  def cdcGlobFor(c: Component): String = cdcGlobsForNamed(c).head

  /**
   * Through-net globs from the post-[[markCdcAnchor]] instance name.
   * Synplify get_nets is case-sensitive: `*cdc_*flowCC*` does not match
   * `cdc_FlowCCUnsafeByToggle`. FlowCCByToggle under JTAG keeps a user
   * `*_ccToggle` name — emit that glob instead of an empty FlowCC miss.
   */
  def cdcGlobsForNamed(c: Component): Seq[String] = {
    val n = Option(c.getName()).getOrElse("")
    c match {
      case _: BufferCC[_] => Seq("*cdc_BufferCC*")
      // Instance is cdc_<orig>_ccToggle, not cdc_StreamCC.
      case _: StreamCCByToggle[_] => Seq("*cdc_*ccToggle*")
      case _: FlowCCByToggle[_] | _: FlowCCUnsafeByToggle[_] =>
        if (n.toLowerCase.contains("cctoggle")) Seq("*cdc_*ccToggle*")
        else Seq("*cdc_*FlowCC*")
      case _ => Seq("*cdc_BufferCC*")
    }
  }

  def markCdcAnchor(c: Component): Unit = {
    keep_chain(c)
    Obfuscater.KeepName(c)
    c.setName(stableCdcName(c))
  }

  /** Keep StreamFifoCC Mem name `ram` so ram_spinal_port1 survives obfuscate. */
  def markFifoRam(c: StreamFifoCC[_]): Unit = {
    c.ram.addTag(Obfuscater.KeepName)
    c.ram.setName("ram")
  }

  /**
   * KeepName Stream/Flow CDC pipe payload regs so obfuscate does not rename
   * them out from under the popArea_stream_rData and flow_m2sPipe get_cells
   * TIGs. Stream.m2sPipe uses rData; Flow.m2sPipe(holdPayload) uses a Reg
   * named m2sPipe via setCompositeName.
   */
  def markCrossClockPipeData(c: Component): Unit = {
    c.dslBody.walkDeclarations {
      case bt: BaseType =>
        val n = bt.getName()
        if (n != null && (n.contains("_rData") || n.contains("_m2sPipe"))) {
          bt.addTag(Obfuscater.KeepName)
        }
      case _ =>
    }
  }

  // Spinal markCdcAnchor names: cdc_BufferCC / cdc_FlowCC* / cdc_*_ccToggle.
  // Do not glob bare *cdc_* — that also hits Lattice Soft-DPHY lscc_csr_cdc_*
  // and CPE can segfault expanding the collection.
  val cdcNetGlobs = Seq(
    "*cdc_BufferCC*",
    "*cdc_*ccToggle*",
    "*cdc_*FlowCC*"
  )

  // USB23 HIP INTERRUPT async out: board Soft-DPHY SDC only (keeps
  // -hierarchical). Do not emit from IP SDC — CPE scopes to
  // <ip_inst>/*/INTERRUPT and misses nested USB23_1.

  // USB23 HIP input leaves (databook hold TIGs). Use leaf globs (*/PIN), not
  // *USB23*/PIN: CPE scopes IP SDC under the IP instance and strips
  // -hierarchical, so <ip_inst>/*USB23*/X… is empty (USB23 is nested).
  // */X… becomes <ip_inst>/*/X… and expands to USB23_1.USB23_inst (and PLL
  // LMMIWDATA). LMMIWDATA — not LiteX LMMIDATA.
  val usb23HoldPinGlobs = Seq(
    "*/XMAWREADY",
    "*/XMWREADY",
    "*/XMBID*",
    "*/XMBVALID",
    "*/XMBRESP*",
    "*/XMARREADY",
    "*/XMRID*",
    "*/XMRVALID",
    "*/XMRLAST",
    "*/XMRDATA*",
    "*/LMMIWDATA*"
  )

  // Synplify: get_pins *….ff_inst/DF* is rewritten to *…\.ff_inst* and never
  // matches. get_cells works (TWR 2026-09-03). Leading */ excludes logic_ram_*.
  val fifoRamCellGlobs = Seq("*/ram_spinal_port1*")
  // BufferCC dest-side: buffers_0 is first sample (HIP INTERRUPT end);
  // buffers_1 includes async-reset LSR (asyncAssertSyncDeassert) not covered
  // by -through buffers_1/Q alone.
  val bufferCcDestFlopCellGlobs = Seq("*/buffers_0*", "*/buffers_1*")
  // StreamCCByToggle pop m2sPipe holds push-clock payload; -through on the
  // ccToggle nets misses paths that *end* at these regs (TWR 2026-09-03).
  val streamCcPopDataCellGlobs = Seq("*/popArea_stream_rData*")
  // FlowCC* output m2sPipe (holdPayload Reg via setCompositeName →
  // outputArea_flow_m2sPipe_*). Do not use */flow_m2sPipe*: Synplify wants
  // the leaf to start right after /, so that glob never matches and the
  // push→pop payload CDC stays timed (MT447).
  val flowCcPopDataCellGlobs = Seq("*flow_m2sPipe*")

  def addAttributeIfNeeded(d : Component, n : String, v : String): Unit = {
    if (!d.getTagsOf[Attribute].exists(a => a.getName == n)) {
      d.addAttribute(n, v)
    }
  }
  def keep_chain(d : Component): Unit = {
    if (d != null) {
      addAttributeIfNeeded(d, "syn_keep", "1")
      addAttributeIfNeeded(d, "nomerge", "")
      addAttributeIfNeeded(d, "keep_hierarchy", "TRUE")
      // Do not walk parents: that keep_hierarchy's Axi4CC / the top and
      // blocks Yosys flatten; CPE then dies expanding */CLKOP.
    }
  }

  def keep_key_heirarchy(d : Component): Unit = {
    walkCdcComponents(d) {
      case c if isCdcThroughLeaf(c) =>
        markCdcAnchor(c)
        c match {
          case _: StreamCCByToggle[_] | _: FlowCCByToggle[_] | _: FlowCCUnsafeByToggle[_] =>
            markCrossClockPipeData(c)
          case _ =>
        }
      case c: StreamFifoCC[_] => markFifoRam(c)
      case _ =>
    }
    for (fp <- constraints.false_paths) {
      KeepAttribute(fp)
      keep_chain(fp.component)
    }
  }

  /**
   * Central CDC keep / rename for Radiant globs + Obfuscater. Registered on
   * [[Config.spinal]] so every `spinal.lib` BufferCC / StreamCC / … under an
   * IPGen top gets KeepName without call-site KeepName or a SpinalHDL fork.
   * Must run before [[PhaseObfuscater]].
   */
  class PhaseCdcAnchor extends PhaseNetlist {
    override def impl(pc: PhaseContext): Unit = {
      keep_key_heirarchy(pc.topLevel)
    }
  }

  /** Extras Obfuscater after [[PhaseCdcAnchor]]. Only when `obfuscate: true`. */
  class PhaseObfuscater extends PhaseNetlist {
    override def impl(pc: PhaseContext): Unit = {
      Obfuscater(pc.topLevel)
    }
  }

  /** Visit every known Spinal CDC leaf and container under `d`. */
  def walkCdcComponents(d: Component)(fn: Component => Unit): Unit = {
    d.walkComponents {
      case c if isCdcThroughLeaf(c) || isCdcContainer(c) => fn(c)
      case _ =>
    }
  }

  /** @deprecated use [[walkCdcComponents]] */
  def walk_cc_components(d : Component, fn : Any => Unit): Unit = {
    for (false_path <- constraints.false_paths) {
      fn(false_path)
    }
    walkCdcComponents(d)(c => fn(c))
  }

  def check(): Unit = {
    if(Component.toplevel != toplevel) {
      constraints = new Constraints()
      toplevel = Component.toplevel
    }
  }

  def write_file[T <: Component](report: SpinalReport[T], path : String): Unit = {
    check()
    constraints.write_file(report, path)
  }
  def add_verbatim(s: => String) : Unit = {
    check()
    constraints.verbatim_constraints.append(() => s)
  }
  def add_false_path(d: Data*): Unit = {
    check()
    constraints.false_paths.appendAll(d)
  }
  def create_clock(d: Data, f: HertzNumber) = {
    check()
    constraints.clocks.append((d, f))
  }

  def create_generated_clock(dest: Data, source: Data, destFreq: HertzNumber, sourceFreq: HertzNumber): Unit = {
    check()
    val fromHz = math.round(sourceFreq.toDouble)
    val toHz = math.round(destFreq.toDouble)
    val g = BigInt(fromHz).gcd(BigInt(toHz)).toLong
    constraints.generated_clocks.append((dest, source, toHz / g, fromHz / g, destFreq))
  }
  def set_max_skew(max_skew: TimeNumber, d: Data*) = {
    check()
    d.foreach(KeepAttribute(_))
    constraints.max_skews.append((d, max_skew))
  }
  def add_clock_group(asynchronous: Boolean, clks: Data*): Unit = {
    check()
    constraints.clock_groups.append((clks, asynchronous))
  }
  def set_constraints(tags: Map[String, String], ports: Data*): Unit = {
    check()
    constraints.constraints.append((ports, tags))
  }
  def set_min_delay(delay: TimeNumber, d: Data*): Unit = {
    check()
    constraints.min_delay.append((d, delay))
  }
}