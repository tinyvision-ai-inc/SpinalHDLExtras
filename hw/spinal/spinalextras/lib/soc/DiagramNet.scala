package spinalextras.lib.soc

import spinal.core._
import spinal.lib.IMasterSlave
import spinal.lib.bus.misc.{AddressMapping, DefaultMapping}
import spinalextras.lib.bus.GlobalBus

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/** Canonical SoC graph for docs. Schema `tinyclunx.diagrams/v1`. No layout or colors. */
object DiagramNet {
  val schema = "tinyclunx.diagrams/v1"

  case class CsrMap(name: String, base: String)
  case class Port(
                   id: String,
                   dir: String,
                   role: String,
                   domain: Option[String] = None,
                   width: Option[Int] = None,
                   map: Option[CsrMap] = None,
                   pairs_with: Option[String] = None,
                   sync: Option[String] = None
                 )
  case class Block(
                    id: String,
                    kind: String,
                    var instance: String = "",
                    named: Nameable = null,
                    cdc: Boolean = false,
                    depth: Option[Int] = None,
                    ports: ArrayBuffer[Port] = ArrayBuffer()
                  )
  case class Link(
                   from: String,
                   to: String,
                   role: String,
                   cdc: Boolean = false,
                   width: Option[Int] = None,
                   domain: Option[String] = None,
                   map: Option[CsrMap] = None
                 )
  case class Domain(
                     id: String,
                     hz: Long,
                     source: String,
                     var resetSource: String = "",
                     var cd: Option[ClockDomain] = None
                   )
  case class Pad(id: String, port: String, hz: Long)

  private var toplevel: Component = null
  private var domains = ArrayBuffer[Domain]()
  private var pads = ArrayBuffer[Pad]()
  private var blocks = ArrayBuffer[Block]()
  private var links = ArrayBuffer[Link]()

  val pllPins: Seq[String] = Seq("CLKOP", "CLKOS", "CLKOS2", "CLKOS3", "CLKOS4", "CLKOS5")

  def check(): Unit = {
    if (Component.toplevel != toplevel) {
      toplevel = Component.toplevel
      domains = ArrayBuffer()
      pads = ArrayBuffer()
      blocks = ArrayBuffer()
      links = ArrayBuffer()
    }
  }

  def hasBlock(id: String): Boolean = {
    check()
    blocks.exists(_.id == id)
  }

  def domainId(cd: ClockDomain): Option[String] = {
    check()
    if (cd == null) None
    else domains.find(_.cd.exists(c => ClockDomain.areSynchronous(c, cd))).map(_.id)
  }

  def domainOf(id: String): Option[Domain] = {
    check()
    domains.find(_.id == id)
  }

  def domain(id: String, hz: HertzNumber, source: String, cd: ClockDomain = null, resetSource: String = ""): Unit = {
    check()
    domains.find(_.id == id) match {
      case Some(d) =>
        if (d.cd.isEmpty && cd != null) d.cd = Some(cd)
        if (d.resetSource.isEmpty && resetSource.nonEmpty) d.resetSource = resetSource
      case None =>
        domains += Domain(id, math.round(hz.toDouble), source, resetSource, Option(cd))
    }
  }

  def pad(id: String, port: String, hz: HertzNumber): Unit = {
    check()
    if (!pads.exists(_.id == id)) {
      pads += Pad(id, port, math.round(hz.toDouble))
    }
  }

  def block(id: String, kind: String, instance: String = "", named: Nameable = null,
            cdc: Boolean = false, depth: Option[Int] = None): Block = {
    check()
    blocks.find(_.id == id).getOrElse {
      val b = Block(id, kind, instance, named, cdc, depth)
      blocks += b
      b
    }
  }

  def port(blockId: String, p: Port): Unit = {
    check()
    blocks.find(_.id == blockId) match {
      case Some(b) =>
        if (!b.ports.exists(_.id == p.id)) b.ports += p
      case None =>
        SpinalWarning(s"DiagramNet.port on unknown block '$blockId'")
    }
  }

  def link(from: String, to: String, role: String,
           cdc: Boolean = false, width: Option[Int] = None,
           domain: Option[String] = None, map: Option[CsrMap] = None): Unit = {
    check()
    if (from != to && !links.exists(l => l.from == from && l.to == to && l.role == role)) {
      links += Link(from, to, role, cdc, width, domain, map)
    }
  }

  def hex(addr: BigInt): String = "0x" + addr.toString(16)

  def mappingBase(m: AddressMapping): Option[String] = m match {
    case DefaultMapping => None
    case _ => Try(hex(m.lowerBound)).toOption
  }

  /** Drive a clock pin from the domain's recorded source. Also drives pairs_with reset if set. */
  def tieClock(blockId: String, pin: String, cd: ClockDomain, cdc: Boolean = false): Unit = {
    domainId(cd).foreach { id =>
      val d = domains.find(_.id == id).get
      link(d.source, s"$blockId.$pin", "clock", cdc = cdc, domain = Some(id))
      blocks.find(_.id == blockId).foreach { b =>
        b.ports.find(_.id == pin).foreach { p =>
          p.pairs_with.foreach { rst =>
            if (d.resetSource.nonEmpty) {
              link(d.resetSource, s"$blockId.$rst", "reset", cdc = cdc, domain = Some(id))
            }
          }
        }
      }
    }
  }

  def tieReset(blockId: String, pin: String, cd: ClockDomain, cdc: Boolean = false): Unit = {
    domainId(cd).foreach { id =>
      val d = domains.find(_.id == id).get
      if (d.resetSource.nonEmpty) {
        link(d.resetSource, s"$blockId.$pin", "reset", cdc = cdc, domain = Some(id))
      }
    }
  }

  def csrMaps[T <: IMasterSlave with Nameable with Bundle](cpuBlock: String, bus: GlobalBus[T], width: Int = 32): Unit = {
    check()
    block(cpuBlock, "SpineX")
    port(cpuBlock, Port("wb", "out", "csr", width = Some(width)))
    bus.slaves.foreach { case (slaveBus, mapping, _) =>
      val name = Option(slaveBus.getName()).filter(_.nonEmpty).getOrElse("slave")
      if (name == "mmi_axi") {
        // Data window; TinyClunx records the AXI path.
      } else {
        val base = mappingBase(mapping)
        if (name == "usb" && hasBlock("lmmi_cdc")) {
          port("lmmi_cdc", Port("cpu", "in", "csr", width = Some(width),
            map = base.map(b => CsrMap(name, b))))
          link(s"$cpuBlock.wb", "lmmi_cdc.cpu", "csr", width = Some(width), cdc = true,
            map = base.map(b => CsrMap(name, b)))
        } else {
          val dstBlock = if (name == "usb" && hasBlock("usb23")) "usb23" else name
          val dstPort = if (dstBlock == "usb23") "LMMI" else "sel"
          if (dstBlock != "usb23") {
            block(dstBlock, "csr")
            port(dstBlock, Port(dstPort, "in", "csr", width = Some(width),
              map = base.map(b => CsrMap(name, b))))
          }
          link(s"$cpuBlock.wb", s"$dstBlock.$dstPort", "csr", width = Some(width),
            map = base.map(b => CsrMap(name, b)))
        }
      }
    }
  }

  /**
   * Pad + PLL outputs. `passthroughIdx` is the outputClocks index that is the
   * board clock (no PLL pin); -1 if the PLL synthesizes every output.
   */
  def capturePll(outputClocks: Seq[spinalextras.lib.misc.ClockSpecification],
                 clockDomains: Seq[ClockDomain],
                 passthroughIdx: Int,
                 inputHz: HertzNumber,
                 pllNamed: Nameable,
                 hasPor: Boolean): Unit = {
    check()
    pad("pad", "clk", inputHz)
    domain("pad60", inputHz, "pad.clk")

    block("pll", "PLL", named = pllNamed)
    port("pll", Port("REFCK", "in", "clock", domain = Some("pad60")))
    port("pll", Port("LOCK", "out", "status"))
    link("pad.clk", "pll.REFCK", "clock", domain = Some("pad60"))

    if (hasPor) {
      pad("por", "resetn", HertzNumber(BigDecimal(0)))
      port("pll", Port("PLLRESET", "in", "reset", sync = Some("async")))
      link("por.resetn", "pll.PLLRESET", "reset")
    }

    var pllOut = 0
    for (i <- outputClocks.indices) {
      val cd = clockDomains(i)
      val hz = cd.frequency.getValue
      val rstPin = s"resets_$i"
      if (i == passthroughIdx) {
        domain("pad60", hz, "pad.clk", cd, resetSource = s"pll.$rstPin")
        port("pll", Port(rstPin, "out", "reset", domain = Some("pad60"), sync = Some("sync")))
      } else {
        val pin = pllPins(pllOut)
        pllOut += 1
        val id = pin.toLowerCase
        domain(id, hz, s"pll.$pin", cd, resetSource = s"pll.$rstPin")
        port("pll", Port(pin, "out", "clock", domain = Some(id)))
        port("pll", Port(rstPin, "out", "reset", domain = Some(id),
          pairs_with = Some(pin), sync = Some("sync")))
      }
    }
  }

  def captureDphy(id: String, named: Nameable, sensor: String,
                  byteCd: ClockDomain, syncCd: ClockDomain, pixelCd: ClockDomain,
                  continuous: Boolean): Unit = {
    block(id, "Soft-DPHY", named = named)
    val byteDom = domainId(byteCd)
    val syncDom = domainId(syncCd)
    val pixDom = domainId(pixelCd)
    port(id, Port("clk_byte_fr_i", "in", "clock", domain = byteDom, pairs_with = Some("reset_byte_fr_n_i")))
    port(id, Port("reset_byte_fr_n_i", "in", "reset", domain = byteDom, pairs_with = Some("clk_byte_fr_i"), sync = Some("sync")))
    if (!continuous) {
      port(id, Port("clk_lp_ctrl_i", "in", "clock", domain = byteDom))
    }
    port(id, Port("sync_clk_i", "in", "clock", domain = syncDom, pairs_with = Some("reset_n_i")))
    port(id, Port("reset_n_i", "in", "reset", domain = syncDom, pairs_with = Some("sync_clk_i"), sync = Some("async")))
    port(id, Port("pll_lock_i", "in", "status"))
    port(id, Port("pixel", "out", "video", domain = pixDom, width = Some(64)))
    port(id, Port("mipi", "in", "video"))
    tieClock(id, "clk_byte_fr_i", byteCd)
    if (!continuous) {
      domainId(byteCd).foreach { d =>
        domainOf(d).foreach(dom => link(dom.source, s"$id.clk_lp_ctrl_i", "clock", domain = Some(d)))
      }
    }
    tieClock(id, "sync_clk_i", syncCd)
    link("pll.LOCK", s"$id.pll_lock_i", "status")
    if (sensor.nonEmpty) {
      link(s"$sensor.mipi", s"$id.mipi", "video")
    }
  }

  private def esc(s: String): String =
    "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""

  private def optField(name: String, v: Option[String]): String =
    v.map(x => s""", "$name": ${esc(x)}""").getOrElse("")

  private def optInt(name: String, v: Option[Int]): String =
    v.map(x => s""", "$name": $x""").getOrElse("")

  private def optBool(name: String, v: Boolean): String =
    if (v) s""", "$name": true""" else ""

  private def mapJson(m: CsrMap): String =
    s"""{"name": ${esc(m.name)}, "base": ${esc(m.base)}}"""

  private def portJson(p: Port): String = {
    val map = p.map.map(m => s""", "map": ${mapJson(m)}""").getOrElse("")
    s"""{"id": ${esc(p.id)}, "dir": ${esc(p.dir)}, "role": ${esc(p.role)}${optField("domain", p.domain)}${optInt("width", p.width)}$map${optField("pairs_with", p.pairs_with)}${optField("sync", p.sync)}}"""
  }

  private def resolveInstance(b: Block): String = {
    if (b.named != null && b.named.isNamed) b.named.getName()
    else b.instance
  }

  private def blockJson(b: Block): String = {
    val inst = resolveInstance(b)
    val instF = if (inst.nonEmpty) s""", "instance": ${esc(inst)}""" else ""
    val depth = optInt("depth", b.depth)
    val cdc = optBool("cdc", b.cdc)
    val ports = b.ports.map(portJson).mkString("[", ", ", "]")
    s"""{"id": ${esc(b.id)}, "kind": ${esc(b.kind)}$instF$cdc$depth, "ports": $ports}"""
  }

  private def linkJson(l: Link): String = {
    val map = l.map.map(m => s""", "map": ${mapJson(m)}""").getOrElse("")
    s"""{"from": ${esc(l.from)}, "to": ${esc(l.to)}, "role": ${esc(l.role)}${optBool("cdc", l.cdc)}${optInt("width", l.width)}${optField("domain", l.domain)}$map}"""
  }

  def generate[T <: Component](report: SpinalReport[T], obfuscate: Boolean = false): Unit = {
    check()
    if (obfuscate) {
      println("DiagramNet: skip dump (obfuscated RTL)")
      return
    }
    if (domains.isEmpty && blocks.isEmpty) {
      return
    }
    val design = report.toplevelName
    val path = s"${report.globalData.config.targetDirectory}/${design}.diagrams.json"
    val w = new PrintWriter(path)
    try {
      w.println("{")
      w.println(s"""  "schema": ${esc(schema)},""")
      w.println(s"""  "design": ${esc(design)},""")
      w.println(s"""  "domains": [${domains.map(d => s"""{"id": ${esc(d.id)}, "hz": ${d.hz}, "source": ${esc(d.source)}}""").mkString(", ")}],""")
      w.println(s"""  "pads": [${pads.map(p => s"""{"id": ${esc(p.id)}, "port": ${esc(p.port)}, "hz": ${p.hz}}""").mkString(", ")}],""")
      w.println(s"""  "blocks": [""")
      w.println(blocks.map(b => "    " + blockJson(b)).mkString(",\n"))
      w.println("  ],")
      w.println(s"""  "links": [""")
      w.println(links.map(l => "    " + linkJson(l)).mkString(",\n"))
      w.println("  ]")
      w.println("}")
    } finally {
      w.close()
    }
    println(s"DiagramNet: $path")
  }
}
