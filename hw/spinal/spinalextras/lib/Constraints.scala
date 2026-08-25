package spinalextras.lib

import spinal.core._
import spinal.lib.{BufferCC, FlowCCByToggle, FlowCCUnsafeByToggle, KeepAttribute, StreamCCByToggle, StreamFifoCC}

import java.io.PrintWriter
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Constraints {
  val clocks = new mutable.ArrayBuffer[(Data, HertzNumber)]()
  val generated_clocks = new mutable.ArrayBuffer[(Data, Data, Long, Long)]()
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
    if (isToplevelPort(data, toplevel)) leaf
    else data.getRtlPath().replace('/', '_')
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

  private def generatedClockDest(dest: Data, toplevel: Component): String = {
    if (isToplevelPort(dest, toplevel)) {
      s"[get_ports {${portLeaf(dest)}}]"
    } else {
      val parts = dest.getRtlPath().split('/')
      if (parts.length >= 2) {
        s"[get_nets -hierarchical {*${parts.head}_${parts.last}}]"
      } else {
        clockTarget(dest, toplevel)
      }
    }
  }

  private def generatedClockSource(source: Data, toplevel: Component): String = {
    if (toplevel.getAllIo.exists(_.getName() == "clk")) {
      "[get_ports {clk}]"
    } else {
      clockTarget(source, toplevel)
    }
  }

  private def hierarchicalClockTarget(data: Data, toplevel: Component): String = {
    val path = data.getRtlPath()
    if (data.component == toplevel) {
      s"[get_ports {$path}]"
    } else {
      s"[get_pins -hierarchical {*/$path}]"
    }
  }

  private def netWildcard(path: String): String = s"[get_nets {${path}/*}]"

  private def leftoverNetWildcard(path: String): String =
    s"[get_nets -hierarchical {*$path/*}]"

  private def skipDuplicateJtagClock(data: Data, toplevel: Component): Boolean = {
    data.getRtlPath().endsWith("jtag_tck") &&
      toplevel.getAllIo.exists(_.getName() == "jtag_tck")
  }

  def write_file[T <: Component](report: SpinalReport[T], path : String): Unit = {
    val file = new PrintWriter(path)

    report.globalData.config.defaultClockDomainFrequency match {
      case f : FixedFrequency => {
        val defaultClock = f.getValue
        file.println(s"# clk ${defaultClock.decompose}")
        val clkTarget =
          if (report.toplevel.getAllIo.exists(_.getName() == "clk"))
            "[get_ports {clk}]"
          else
            "[get_nets clk]"
        file.println(s"create_clock -name {clk} -period ${defaultClock.toTime.toDouble * 1e9} $clkTarget")
      }
      case _ => {}
    }

    for ((data, freq) <- clocks) {
      if (skipDuplicateJtagClock(data, report.toplevel)) {
        file.println(s"# skip ${clockNameFor(data, report.toplevel)} (board jtag_tck)")
      } else {
        val cname = clockNameFor(data, report.toplevel)
        file.println(s"# ${cname} ${freq.decompose}")
        file.println(s"create_clock -name {${cname}} -period ${freq.toTime.toDouble * 1e9} ${hierarchicalClockTarget(data, report.toplevel)}")
      }
    }

    for ((dest, source, mul, div) <- generated_clocks) {
      val cname = clockNameFor(dest, report.toplevel)
      file.println(s"create_generated_clock -name {${cname}} -source ${generatedClockSource(source, report.toplevel)} -multiply_by ${mul} -divide_by ${div} ${generatedClockDest(dest, report.toplevel)}")
    }

    def hasPort(n: String) = report.toplevel.getAllIo.exists(_.getName() == n)
    def presentPorts(ns: String*) = ns.filter(hasPort)

    if (hasPort("jtag_tck")) {
      file.println("create_clock -name {jtag_tck} -period 100 [get_ports {jtag_tck}]")
    }

    file.println("set_clock_uncertainty 0.125 [all_clocks]")

    for (n <- presentPorts("led", "uart_txd")) {
      file.println(s"set_false_path -to [get_ports {$n}]")
    }
    for (n <- presentPorts("uart_rxd")) {
      file.println(s"set_false_path -from [get_ports {$n}]")
    }
    for (n <- presentPorts("scl", "sda")) {
      file.println(s"set_false_path -from [get_ports {$n}]")
      file.println(s"set_false_path -to [get_ports {$n}]")
    }
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
    if (hasPort("spiflash_clk") && hasPort("spiflash_dq")) {
      file.println("set_input_delay -clock [get_clocks {spiflash_clk}] -clock_fall -max 6.0 [get_ports {spiflash_dq*}]")
      file.println("set_input_delay -clock [get_clocks {spiflash_clk}] -clock_fall -min 1.5 [get_ports {spiflash_dq*}]")
      file.println("set_output_delay -clock [get_clocks {spiflash_clk}] -max 2.0 [get_ports {spiflash_dq*}]")
      file.println("set_output_delay -clock [get_clocks {spiflash_clk}] -min -3.0 [get_ports {spiflash_dq*}]")
    }
    if (hasPort("spiflash_clk") && hasPort("spiflash_cs_n")) {
      file.println("set_output_delay -clock [get_clocks {spiflash_clk}] -max 5.0 [get_ports {spiflash_cs_n}]")
      file.println("set_output_delay -clock [get_clocks {spiflash_clk}] -min -3.0 [get_ports {spiflash_cs_n}]")
    }

    //    for ((clks, async) <- clock_groups) {
    //      file.println(s"set_clock_groups ${clks.map("-group [get_clocks {" + _.name +"}]").mkString(" ")} ${if(async) "-asynchronous" else ""}")
    //    }

    for ((datas, skew) <- max_skews) {
      file.println(s"set_max_skew [get_nets {${datas.map(_.getRtlPath() + "*").mkString(" ")}}] ${skew.toDouble * 1e9}")
    }

    for ((datas, delay) <- min_delay) {
      file.println(s"set_min_delay -through [get_nets {${datas.map(_.getRtlPath() + "*").mkString(" ")}}] ${delay.toDouble * 1e9}")
    }

    def set_false_path(d : Data): Unit = {
      KeepAttribute(d)
      d.addAttribute("syn_keep", 1).addAttribute("nomerge", "")
      file.println(s"set_false_path -through ${netWildcard(d.getRtlPath())}")
    }

    val leftoverCdc = new ArrayBuffer[Component]()
    val leftoverFifoRam = new ArrayBuffer[Component]()
    def markCdc(c: Component): Unit = {
      Constraints.keep_chain(c)
      if (!Constraints.cdcGlobCovers(c.getRtlPath())) {
        leftoverCdc += c
      }
    }
    for(false_path <- false_paths) {
      set_false_path(false_path)
    }

    report.toplevel.walkComponents {
      case c: StreamFifoCC[_] => {
        Constraints.keep_chain(c)
        if (!Constraints.fifoRamGlobCovers(c.getRtlPath())) {
          leftoverFifoRam += c
        }
      }
      case c: BufferCC[_] => markCdc(c)
      case c: StreamCCByToggle[_] => markCdc(c)
      case c: FlowCCByToggle[_] => markCdc(c)
      case c: FlowCCUnsafeByToggle[_] => markCdc(c)
      case c: Component => {}
    }

    for (g <- Constraints.cdcNetGlobs) {
      file.println(s"set_false_path -through [get_nets -hierarchical {$g}]")
    }
    for (c <- leftoverCdc) {
      file.println(s"set_false_path -through ${leftoverNetWildcard(c.getRtlPath())}")
    }
    for (g <- Constraints.fifoRamPinGlobs) {
      file.println(s"set_false_path -to [get_pins -hierarchical {$g}]")
    }
    for (c <- leftoverFifoRam) {
      file.println(s"set_false_path -to [get_pins -hierarchical {${c.getRtlPath()}/${Constraints.fifoRamPinLeaf}}]")
    }
    file.println("set_false_path -to [get_pins -hierarchical {*asyncAssertSyncDeassert_buffercc*/*.ff_inst/LSR}]")

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

  val cdcNetGlobs = Seq(
    "*buffercc*/*",
    "*bufferCC*/*",
    "*streamCCByToggle*/*",
    "*flowCCUnsafeByToggle*/*"
  )

  val fifoRamPinLeaf = "ram_spinal_port1*/DF"
  val fifoRamPinGlobs = Seq(s"*streamFifoCC*/$fifoRamPinLeaf")

  def globToRegex(glob: String): String = {
    glob.split("\\*", -1).map(java.util.regex.Pattern.quote).mkString(".*")
  }

  def cdcGlobCovers(rtlPath: String): Boolean = {
    val probe = rtlPath + "/x"
    cdcNetGlobs.exists(g => probe.matches(globToRegex(g)))
  }

  def fifoRamGlobCovers(rtlPath: String): Boolean = {
    val probe = rtlPath + "/ram_spinal_port1.ff_inst/DF"
    fifoRamPinGlobs.exists(g => probe.matches(globToRegex(g)))
  }

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
      keep_chain(d.parent)
    }
  }

  def keep_key_heirarchy(d : Component): Unit = {
    walk_cc_components(d, {
      case c: Component => {
        keep_chain(c)
      }
      case c : Data => {
        KeepAttribute(c)
        keep_chain(c.component)
      }
    })
  }


  def walk_cc_components(d : Component, fn : Any => Unit): Unit = {
    for(false_path <- constraints.false_paths) {
      fn(false_path)
    }

    d.walkComponents {
      case c: StreamFifoCC[_] => {
        fn(c)
      }
      case c: BufferCC[_] => {
        fn(c.io.dataIn)
      }
      case c: StreamCCByToggle[_] => {
        fn(c)
      }
      case c: FlowCCByToggle[_] => {
        fn(c)
      }
      case c: FlowCCUnsafeByToggle[_] => {
        fn(c)
      }
      case c: Component => {
        if(c != d) {
          walk_cc_components(c, fn)
        }
      }
    }

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
    constraints.generated_clocks.append((dest, source, toHz / g, fromHz / g))
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