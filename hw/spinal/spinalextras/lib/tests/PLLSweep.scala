package spinalextras.lib.tests

import org.scalatest.funsuite.AnyFunSuite
import spinal.core.HertzNumber
import spinalextras.lib.blackbox.lattice.lifcl.{PLLClockConfig, PLLConfig => NexusPll}
import spinalextras.lib.misc.ClockSpecification
import spinalextras.lib.misc.ClockToleranceType
import spinalextras.lib.misc.ClockToleranceType.{AtLeast, AtMost, Centered}

import java.io.{OutputStream, PrintStream}
import scala.collection.mutable.ArrayBuffer

class PLLSweepTest extends AnyFunSuite {
  test("PLL.scala output matches FPGA-TN-02095 across one to five clocks") {
    assume(sys.env.get("PLL_SWEEP").contains("1"), "set PLL_SWEEP=1 to run the divider sweep")
    val report = PLLSweep.run()
    assert(report.ok, report.toString)
  }
}

/**
 * Sweep [[NexusPll.createClockConfig]] and check every solution against
 * FPGA-TN-02095-2.7.
 *
 * Fractional-N (§14.7), F a 12-bit word packed into the top of 15-bit
 * SSC_F_CODE the way Radiant's PLL IP does (`F << 3`):
 *   f_OUT = f_CLKI * (N + F/4096) / (M * O)
 *
 * Integer feedback (§14.4.2), taken after the feedback output's divider:
 *   f_VCO = f_CLKI * N * O_fb / M
 *   f_OUT = f_VCO / O
 *
 * O is the divider register plus one (Appendix C, `divide value = DIVA + 1`).
 * Phase-detector and VCO limits are CrossLink-NX datasheet Table 3.34.
 *
 * `sbt test` skips this unless PLL_SWEEP=1. Run the whole grid with
 * `sbt "runMain spinalextras.lib.tests.PLLSweep"`.
 */
object PLLSweep {
  val FracDenom: Double = NexusPll.clkfb_frac_denom.toDouble
  val PfdMinHz = 18e6
  val PfdMaxIntegerHz = 500e6
  val PfdMaxFracHz = 100e6
  val VcoMinHz = 800e6
  val VcoMaxHz = 1600e6
  val FoutMinHz = 6.25e6
  val FoutMaxHz = 800e6

  final case class Failure(msg: String)
  final case class Report(solved: Int, unsolved: Int, refused: Int, failures: Seq[Failure]) {
    def ok: Boolean = failures.isEmpty
    override def toString: String =
      s"solved $solved  unsolved $unsolved  refused $refused  failures ${failures.size}" +
        (if (failures.isEmpty) "" else "\n" + failures.map("  " + _.msg).mkString("\n"))
  }

  /** One requested PLL output. `kind` selects the ClockSpecification window. */
  final case class Out(mhz: Double, tolerance: Double = 0.01, kind: ClockToleranceType = Centered, phase: Double = 0) {
    def toSpec: ClockSpecification =
      ClockSpecification(HertzNumber(mhz * 1e6), phaseOffset = phase, tolerance = tolerance, toleranceType = kind)
    override def toString: String = f"$mhz%.4g@$tolerance%.4g/$kind"
  }

  /** f_OUT = f_CLKI * (N + F/4096) / (M * O), the TN-02095 §14.7 point. */
  def fracHz(finHz: Double, m: Int, n: Int, f: Int, o: Int): Double =
    finHz * (n + f / 4096.0) / (m.toDouble * o)

  def fracSpec(finHz: Double, m: Int, n: Int, f: Int, o: Int): ClockSpecification =
    ClockSpecification(HertzNumber(fracHz(finHz, m, n, f, o)), tolerance = 0)

  def packCheck(finHz: Double, cfg: PLLClockConfig): Option[String] = {
    try {
      val bb = quiet(NexusPll.create(ClockSpecification(HertzNumber(finHz), tolerance = 0), cfg))
      val f = cfg.CLKFB_DIV_FRAC
      val word = Integer.parseInt(bb.SSC_F_CODE.drop(2), 2)
      val shift = NexusPll.ssc_f_code_shift
      if (!cfg.CLKFB_FRAC_MODE) Some("create() saw integer mode")
      else if (f == 0) Some("F is 0; the SSC_F_CODE shift is not observable")
      else if (word != (f << shift)) Some(s"SSC_F_CODE $word != $f << $shift")
      else None
    } catch {
      case e: Exception => Some(s"create() ${e.getClass.getSimpleName}: ${e.getMessage}")
    }
  }

  def mhz(hz: Double): String = f"${hz / 1e6}%.6f MHz"

  def spec(mhz: Double, tolerance: Double): ClockSpecification =
    ClockSpecification(HertzNumber(mhz * 1e6), tolerance = tolerance)

  private def quiet[T](body: => T): T = {
    val sink = new PrintStream(new OutputStream { override def write(b: Int): Unit = () })
    val prevOut = System.out
    val prevErr = System.err
    try {
      System.setOut(sink)
      System.setErr(sink)
      Console.withOut(sink)(Console.withErr(sink)(body))
    } finally {
      System.setOut(prevOut)
      System.setErr(prevErr)
    }
  }

  /** Lattice frequency of one solved request. Returns a failure message, or None. */
  def checkSolved(finHz: Double, finTolerance: Double, requests: Seq[ClockSpecification], cfg: PLLClockConfig): Option[String] = {
    val m = cfg.CLKI_DIV
    val n = cfg.CLKFB_DIV
    if (m <= 0 || cfg.OUTPUTS.length != requests.length)
      return Some(s"bad solution M=$m outputs ${cfg.OUTPUTS.length} for ${requests.length} requests")

    val pfd = finHz / m
    val freqs = if (cfg.CLKFB_FRAC_MODE) {
      if (pfd < PfdMinHz || pfd > PfdMaxFracHz)
        return Some(f"fractional PFD ${mhz(pfd)} outside $PfdMinHz%.0f..$PfdMaxFracHz%.0f")
      val encoded = cfg.CLKFB_DIV_FRAC << NexusPll.ssc_f_code_shift
      val decoded = encoded >> NexusPll.ssc_f_code_shift
      val bits = Integer.parseInt(NexusPll.to_bin_string(encoded, NexusPll.ssc_f_code_bits).drop(2), 2)
      if (decoded != cfg.CLKFB_DIV_FRAC || bits != encoded || (encoded & ((1 << NexusPll.ssc_f_code_shift) - 1)) != 0)
        return Some(s"SSC_F_CODE packing F=${cfg.CLKFB_DIV_FRAC} -> 0b${encoded.toBinaryString} does not round-trip")
      val nEff = n + decoded / FracDenom
      if (nEff < 16.0 || nEff > 128.0)
        return Some(f"fractional N $nEff%.4f outside 16..128")
      val vco = finHz * nEff / m
      if (vco < VcoMinHz || vco > VcoMaxHz)
        return Some(f"fractional VCO ${mhz(vco)} outside 800..1600 MHz")
      cfg.OUTPUTS.map { out =>
        val o = out.DIV + 1
        if (o < 1 || o > 128) return Some(s"output divider $o outside 1..128")
        vco / o
      }
    } else {
      if (pfd < PfdMinHz || pfd > PfdMaxIntegerHz)
        return Some(f"integer PFD ${mhz(pfd)} outside 18..500 MHz")
      if (cfg.CLK_REF < 0 || cfg.CLK_REF >= cfg.OUTPUTS.length)
        return Some(s"integer CLK_REF ${cfg.CLK_REF} is not an output")
      val oFb = cfg.OUTPUTS(cfg.CLK_REF).DIV + 1
      val vco = finHz * n * oFb / m
      if (vco < VcoMinHz || vco > VcoMaxHz)
        return Some(f"integer VCO ${mhz(vco)} outside 800..1600 MHz")
      cfg.OUTPUTS.map { out =>
        val o = out.DIV + 1
        if (o < 1 || o > 128) return Some(s"output divider $o outside 1..128")
        vco / o
      }
    }

    val problems = requests.zip(cfg.OUTPUTS).zip(freqs).flatMap { case ((req, out), hz) =>
      val target = req.freq.toDouble
      val self = math.abs(hz - out.ACTUAL_FREQ) / math.max(target, 1.0)
      val edge = math.max(target, 1.0) * 1e-9
      val msgs = ArrayBuffer[String]()
      if (hz < FoutMinHz * (1 - 1e-6) || hz > FoutMaxHz * (1 + 1e-6))
        msgs += f"model ${mhz(hz)} outside 6.25..800 MHz"
      NexusPll.outputWindow(req, finTolerance) match {
        case None =>
          msgs += f"model ${mhz(hz)} but the request misses 6.25..800 MHz"
        case Some((lo, hi)) =>
          if (hz < lo - edge || hz > hi + edge)
            msgs += f"model ${mhz(hz)} outside ${mhz(lo)}..${mhz(hi)} (${req.toleranceType} tol ${req.tolerance * 100}%.4f%%)"
      }
      if (self > 1e-6)
        msgs += s"model ${mhz(hz)} disagrees with solver ACTUAL_FREQ ${mhz(out.ACTUAL_FREQ)}"
      msgs
    }
    if (problems.isEmpty) None
    else {
      val mode = if (cfg.CLKFB_FRAC_MODE) s"frac N=${cfg.CLKFB_DIV}+${cfg.CLKFB_DIV_FRAC}/4096" else s"int N=${cfg.CLKFB_DIV} ref=${cfg.CLK_REF}"
      Some(s"fin ${mhz(finHz)} $mode M=$m: " + problems.mkString("; "))
    }
  }

  sealed trait Attempt
  case class Ok(cfg: PLLClockConfig) extends Attempt
  case class No(msg: String) extends Attempt
  case class Bad(msg: String) extends Attempt

  def solveSpecs(finHz: Double, finTolerance: Double, reqs: Seq[ClockSpecification]): Attempt = {
    val fin = ClockSpecification(HertzNumber(finHz), tolerance = finTolerance)
    val cfg = try quiet(NexusPll.createClockConfig(fin, reqs: _*)) catch {
      case e: IllegalArgumentException => return No(e.getMessage)
    }
    checkSolved(finHz, finTolerance, reqs, cfg) match {
      case Some(msg) => Bad(msg)
      case None => Ok(cfg)
    }
  }

  def solveReqs(finMhz: Double, outs: Seq[Out], finTolerance: Double = 0): Attempt =
    solveSpecs(finMhz * 1e6, finTolerance, outs.map(_.toSpec))

  def run(): Report = {
    val failures = ArrayBuffer[Failure]()
    var solved = 0
    var unsolved = 0
    var refused = 0

    def describe(finMhz: Double, outs: Seq[Out]): String =
      s"$finMhz -> ${outs.mkString(",")}"

    def accept(label: String, attempt: Attempt, check: PLLClockConfig => Option[String]): Unit = {
      attempt match {
        case Ok(cfg) =>
          check(cfg) match {
            case Some(msg) =>
              failures += Failure(s"$label $msg")
              println(s"FAIL $label $msg")
            case None => solved += 1
          }
        case No(_) =>
          failures += Failure(s"$label UNSOLVED")
          println(s"FAIL $label UNSOLVED")
        case Bad(msg) =>
          failures += Failure(s"$label $msg")
          println(s"FAIL $label $msg")
      }
    }

    /** Must produce a solution inside each window. `check` pins mode, M, N, F, or phase. */
    def must(label: String, finMhz: Double, outs: Seq[Out], finTolerance: Double = 0,
             check: PLLClockConfig => Option[String] = _ => None): Unit =
      accept(label, solveReqs(finMhz, outs, finTolerance), check)

    def mustHz(label: String, finHz: Double, reqs: Seq[ClockSpecification], finTolerance: Double = 0,
               check: PLLClockConfig => Option[String] = _ => None): Unit =
      accept(label, solveSpecs(finHz, finTolerance, reqs), check)

    /** Outside the PLL's frequency or output-count range. A solution is a failure. */
    def mustNot(label: String, finMhz: Double, outs: Seq[Out], finTolerance: Double = 0): Unit = {
      solveReqs(finMhz, outs, finTolerance) match {
        case Ok(_) =>
          failures += Failure(s"$label solved but should be refused: ${describe(finMhz, outs)}")
          println(s"FAIL $label solved ${describe(finMhz, outs)}")
        case No(_) => refused += 1
        case Bad(msg) =>
          failures += Failure(s"$label $msg")
          println(s"FAIL $label $msg")
      }
    }

    def mustReject(label: String)(body: => Unit): Unit = {
      try {
        body
        failures += Failure(s"$label was accepted")
        println(s"FAIL $label was accepted")
      } catch {
        case _: IllegalArgumentException | _: AssertionError => refused += 1
      }
    }

    def outsOf(mhzs: Seq[Double], tolerance: Double = 0.01): Seq[Out] = mhzs.map(Out(_, tolerance))

    /** Divider pick for one fixed VCO. `expectDiv` 0 means the window must miss. */
    def divider(label: String, vcoHz: Long, out: Out, expectDiv: Int): Unit = {
      val vco = spinalextras.lib.misc.Rational(vcoHz, 1)
      val (_, cfg, ok) = NexusPll.create_clock_config(vco, 0.0, out.toSpec)
      val div = if (ok) cfg.DIV + 1 else 0
      if (div != expectDiv) {
        val mhzGot = cfg.ACTUAL_FREQ / 1e6
        val got = if (ok) f"$div ($mhzGot%.6f MHz)" else "invalid"
        failures += Failure(s"$label divider $got, expected $expectDiv, $out from VCO ${vcoHz / 1e6} MHz")
        println(s"FAIL $label divider $got expected $expectDiv")
      } else if (expectDiv == 0) refused += 1
      else solved += 1
    }

    val t0 = System.nanoTime()

    // 1 GHz VCO. O=10 is 100 MHz, O=11 is 90.909, O=9 is 111.111.
    // AtLeast 95 MHz @ 8%: nearest divider is 90.909 (below the floor);
    // 100 MHz is the closest divider that stays inside [95, 102.6].
    divider("atleast-not-nearest", 1000000000L, Out(95, 0.08, AtLeast), 10)
    // AtMost 108 MHz @ 10%: nearest is 111.111 (above the cap); 100 MHz is inside.
    divider("atmost-not-nearest", 1000000000L, Out(108, 0.10, AtMost), 10)
    divider("centered-nearest", 1000000000L, Out(94, 0.10), 11)
    divider("centered-exact", 1000000000L, Out(100, 0), 10)
    divider("fout-800", 800000000L, Out(800, 0), 1)
    divider("fout-6.25", 800000000L, Out(6.25, 0), 128)
    divider("above-fout-div", 1000000000L, Out(900, 0.10), 0)
    divider("below-fout-div", 800000000L, Out(5, 0.10), 0)
    // Exact flir_uab clocks. Tolerance 0: only a hit on 75 and 90 counts.
    must("flir", 60, outsOf(Seq(75, 90), 0), check = cfg => {
      if (cfg.CLKFB_FRAC_MODE || cfg.CLKI_DIV != 2 || cfg.CLKFB_DIV != 3)
        Some(s"expected integer M=2 N=3, got frac=${cfg.CLKFB_FRAC_MODE} M=${cfg.CLKI_DIV} N=${cfg.CLKFB_DIV}")
      else None
    })

    // 24 MHz has one legal integer M (floor(24/18) = 1). Dropping that M
    // makes the search empty and this falls through to fractional.
    must("int-m-floor", 24, Seq(Out(96, 0)), check = cfg => {
      if (cfg.CLKFB_FRAC_MODE || cfg.CLKI_DIV != 1 || cfg.CLKFB_DIV != 4)
        Some(s"expected integer M=1 N=4, got frac=${cfg.CLKFB_FRAC_MODE} M=${cfg.CLKI_DIV} N=${cfg.CLKFB_DIV}")
      else None
    })
    // Smallest M is 1, so the phase detector sits on the 500 MHz ceiling.
    must("pfd-500", 500, Seq(Out(500, 0)), check = cfg => {
      if (cfg.CLKFB_FRAC_MODE || cfg.CLKI_DIV != 1)
        Some(s"expected integer M=1, got frac=${cfg.CLKFB_FRAC_MODE} M=${cfg.CLKI_DIV}")
      else None
    })

    // Not an integer ratio. F is nonzero; several (M, N, F) hit the same fout.
    val fracFin = 60e6
    mustHz("frac-nonzero", fracFin, Seq(fracSpec(fracFin, 1, 16, 1, 16)), check = cfg => {
      if (!cfg.CLKFB_FRAC_MODE || cfg.CLKFB_DIV_FRAC == 0)
        Some(s"expected nonzero F, got frac=${cfg.CLKFB_FRAC_MODE} F=${cfg.CLKFB_DIV_FRAC}")
      else None
    })
    // Unique solution: M=1 N=88 F=1 O=8. F=1 makes a missed << 3 obvious.
    val fracHi = 18e6
    mustHz("frac-f1", fracHi, Seq(fracSpec(fracHi, 1, 88, 1, 8)), check = cfg => {
      if (!cfg.CLKFB_FRAC_MODE || cfg.CLKI_DIV != 1 || cfg.CLKFB_DIV != 88 || cfg.CLKFB_DIV_FRAC != 1)
        Some(s"expected frac M=1 N=88 F=1, got frac=${cfg.CLKFB_FRAC_MODE} M=${cfg.CLKI_DIV} N=${cfg.CLKFB_DIV} F=${cfg.CLKFB_DIV_FRAC}")
      else packCheck(fracHi, cfg)
    })

    def phaseRef(cfg: PLLClockConfig, phases: Seq[Double], expectRef: Int): Option[String] = {
      val bad = cfg.OUTPUTS.zip(phases).flatMap { case (out, p) =>
        if (math.abs(out.ACTUAL_PHASE - p) > 1e-4) Some(s"phase ${out.ACTUAL_PHASE} deg != $p") else None
      }
      if (cfg.CLKFB_FRAC_MODE) Some(s"expected integer feedback, M=${cfg.CLKI_DIV}")
      else if (cfg.CLK_REF != expectRef) Some(s"CLK_REF ${cfg.CLK_REF} expected $expectRef")
      else if (bad.nonEmpty) Some(bad.mkString("; "))
      else None
    }

    // 90° on every output cannot be integer feedback. 90° on one output
    // forces CLK_REF onto the phase-0 output.
    must("phase-forces-frac", 60, Seq(Out(100, 0, phase = 90)), check = cfg => {
      if (!cfg.CLKFB_FRAC_MODE) Some(s"integer feedback M=${cfg.CLKI_DIV} ref=${cfg.CLK_REF}")
      else if (math.abs(cfg.OUTPUTS.head.ACTUAL_PHASE - 90) > 1e-4) Some(f"phase ${cfg.OUTPUTS.head.ACTUAL_PHASE}%.4f != 90")
      else None
    })
    must("phase-ref-second", 60, Seq(Out(100, 0, phase = 90), Out(80, 0)), check = cfg => {
      phaseRef(cfg, Seq(90.0, 0.0), 1)
    })
    must("phase-ref-first", 60, Seq(Out(80, 0), Out(100, 0, phase = 90)), check = cfg => {
      phaseRef(cfg, Seq(0.0, 90.0), 0)
    })

    // Not exact, so AtLeast and AtMost are different windows.
    must("atleast-offgrid", 60, Seq(Out(73.1, 0.02, AtLeast)))
    must("atmost-offgrid", 60, Seq(Out(73.1, 0.02, AtMost)))

    // Reference tolerance widens a point request. 70.123 MHz is off the grid.
    mustNot("offgrid-exact", 60, Seq(Out(70.123, 0)))
    must("offgrid-slack", 60, Seq(Out(70.123, 0)), finTolerance = 0.01)
    // 810 MHz is above fOUT max; 2% reference slack still has to stay <= 800.
    must("fout-cap-slack", 60, Seq(Out(810, 0)), finTolerance = 0.02)
    // 6.1 MHz is below fOUT min. 5% reference slack reaches 6.25 MHz.
    mustNot("fout-floor-point", 60, Seq(Out(6.1, 0, AtMost)))
    must("fout-floor-slack", 60, Seq(Out(6.1, 0, AtMost)), finTolerance = 0.05)
    // Centered 6 MHz ±10% crosses 6.25 MHz.
    must("fout-floor-overlap", 60, Seq(Out(6.0, 0.10)))

    val refs = Seq(24.0, 27.0, 48.0, 50.0, 60.0, 100.0, 125.0)
    val singles = Seq(
      6.25, 8.0, 10, 12, 12.5, 16, 20, 24, 25, 27, 30, 33.333, 40, 48, 50,
      60, 70, 75, 80, 90, 100, 108, 120, 125, 133.333, 148.5, 150, 156.25,
      200, 250, 270, 300, 312.5, 400, 500, 600, 750, 800
    )
    for (fin <- refs; fout <- singles if fout != fin)
      must("1out", fin, Seq(Out(fout)))

    val pairFreqs = Seq(10.0, 20, 25, 40, 50, 75, 80, 90, 100, 120, 150, 200)
    for (pair <- pairFreqs.combinations(2)) {
      must("2out", 60, outsOf(pair))
      must("2out-rev", 60, outsOf(pair.reverse))
    }
    for (pair <- pairFreqs.combinations(2))
      must("2out-100", 100, outsOf(pair))

    val tripleFreqs = Seq(25.0, 50, 75, 100, 125, 150)
    for (triple <- tripleFreqs.combinations(3))
      must("3out", 60, outsOf(triple))
    must("4out", 60, outsOf(Seq(25, 50, 75, 100)))
    must("5out", 60, outsOf(Seq(25, 50, 75, 100, 125)))
    must("5out-wide", 50, outsOf(Seq(20, 25, 40, 50, 100)))

    // Exact integer ratios, tolerance 0. Every permutation is a different feedback pin.
    val exactPairs = Seq(
      (60.0, Seq(30.0, 120)), (60.0, Seq(40.0, 80)), (60.0, Seq(50.0, 100)),
      (60.0, Seq(75.0, 100)), (60.0, Seq(75.0, 150)), (48.0, Seq(24.0, 96)),
      (48.0, Seq(64.0, 192)), (50.0, Seq(25.0, 100)), (50.0, Seq(100.0, 200)),
      (100.0, Seq(50.0, 200)), (100.0, Seq(25.0, 125)), (27.0, Seq(54.0, 108)),
      (24.0, Seq(48.0, 72, 96)), (20.0, Seq(40.0, 80, 100, 200))
    )
    for ((fin, freqs) <- exactPairs; perm <- freqs.permutations)
      must("exact", fin, outsOf(perm, 0))

    // Same clocks, three different windows. The tight one has to stay exact.
    val tolLadder = Seq(0.0, 0.005, 0.02, 0.05, 0.10)
    for (tol <- tolLadder) {
      must(f"tol-$tol%.3f", 60, Seq(Out(75, tol), Out(90, tol)))
      must(f"tol-single-$tol%.3f", 48, Seq(Out(100, tol)))
    }
    must("mix-exact-and-loose", 60, Seq(Out(75, 0), Out(88, 0.05), Out(120, 0.01)))
    must("mix-four-windows", 60, Seq(Out(50, 0), Out(75, 0.005), Out(90, 0.01), Out(110, 0.05)))
    // 125 and 200 exact force VCO = 1000 MHz, and 80 MHz ±2% is not a divisor of that VCO.
    mustNot("mix-tight-impossible", 100, Seq(Out(125, 0.005), Out(200, 0), Out(80, 0.02)))
    Seq(125.0, 200, 100).permutations.foreach(p => must("mix-tight-exact", 100, outsOf(p, 0)))

    // AtLeast stays at or above the request. AtMost stays at or below it.
    must("atleast-100", 60, Seq(Out(100, 0.05, AtLeast)))
    must("atmost-100", 60, Seq(Out(100, 0.05, AtMost)))
    must("atleast-pair", 60, Seq(Out(75, 0), Out(90, 0.05, AtLeast)))
    must("atmost-pair", 60, Seq(Out(75, 0), Out(100, 0.05, AtMost)))
    must("both-sides", 48, Seq(Out(100, 0.08, AtLeast), Out(80, 0.08, AtMost)))

    // Incommensurate sets. 1% and 2% must still land inside those windows.
    val awkward = Seq(33.333, 70.0, 74.25, 91, 108, 148.5, 156.25, 270)
    // 148.5 ±1% and 156.25 ±1% (and ±2%) share no VCO in 800–1600 MHz. ±5% does.
    val awkNoVco = Set(148.5, 156.25)
    for (pair <- awkward.combinations(2)) {
      if (pair.toSet == awkNoVco) {
        mustNot("awk-1pct-impossible", 60, outsOf(pair, 0.01))
        mustNot("awk-1pct-impossible-rev", 60, outsOf(pair.reverse, 0.01))
        mustNot("awk-2pct-impossible", 60, outsOf(pair, 0.02))
        must("awk-5pct", 60, outsOf(pair, 0.05))
        must("awk-5pct-rev", 60, outsOf(pair.reverse, 0.05))
      } else {
        must("awk-1pct", 60, outsOf(pair, 0.01))
        must("awk-1pct-rev", 60, outsOf(pair.reverse, 0.01))
      }
    }
    for (fin <- Seq(24.0, 48, 125))
      for (pair <- Seq(70.0, 91, 108, 148.5).combinations(2))
        must("awk-ref", fin, outsOf(pair, 0.02))
    // 75, 90 and 120 have no common VCO at or under 1600 MHz. ±1% does (dividers 19/16/12).
    Seq(75.0, 90, 120).permutations.foreach { p =>
      mustNot("exact-impossible", 60, outsOf(p, 0))
      must("tight-1pct", 60, outsOf(p, 0.01))
    }
    must("awk-triple", 48, Seq(Out(70, 0.02), Out(90, 0.02), Out(110, 0.02)))
    must("4out-loose", 60, Seq(70, 90, 110, 130).map(f => Out(f, 0.02)))
    must("5out-loose", 27, Seq(40, 74.25, 100, 148.5, 200).map(f => Out(f, 0.02)))
    must("6out", 50, Seq(25, 50, 100, 125, 200, 250).map(f => Out(f, 0.01)))
    // Same six clocks, tolerance 0, every output order. VCO 1000 MHz divides all of them.
    Seq(25.0, 50, 100, 125, 200, 250).permutations.foreach(p => must("6exact", 50, outsOf(p, 0)))
    // 50/75/90/100/150 share VCO 900 MHz. Every order, then every tolerance shape.
    val fiveExact = Seq(50.0, 75, 90, 100, 150)
    fiveExact.permutations.foreach(p => must("5exact", 60, outsOf(p, 0)))
    Seq(24.0, 64, 96, 192).permutations.foreach(p => must("4exact-48", 48, outsOf(p, 0)))
    val kinds = Seq(Centered, AtLeast, AtMost)
    for (k1 <- kinds; k2 <- kinds; k3 <- kinds; tol <- Seq(0.0, 0.005, 0.05))
      must("kind-triple", 60, Seq(Out(75, tol, k1), Out(90, tol, k2), Out(100, tol, k3)))
    must("mix-kinds-tols", 60, Seq(
      Out(75, 0, Centered), Out(90, 0.05, AtLeast), Out(100, 0.02, AtMost), Out(150, 0.01, Centered)))
    val six = Seq(25.0, 50, 100, 125, 200, 250)
    val sixKinds = Seq(Centered, AtLeast, AtMost, Centered, AtLeast, AtMost)
    must("kind-6", 50, six.zip(sixKinds).map { case (f, k) => Out(f, 0.02, k) })
    must("kind-6-rev", 50, six.zip(sixKinds).reverse.map { case (f, k) => Out(f, 0.02, k) })
    must("fout-min", 60, Seq(Out(6.25, 0)))
    must("fout-max", 60, Seq(Out(800, 0)))
    must("fout-max-atleast", 60, Seq(Out(800, 0.05, AtLeast)))
    must("fout-min-atmost", 60, Seq(Out(6.25, 0.05, AtMost)))
    must("fout-cap-overlap", 60, Seq(Out(820, 0.05)))
    must("ref-18", 18, Seq(Out(90, 0.01), Out(120, 0.01)))
    must("ref-20", 20, Seq(Out(40, 0), Out(100, 0.01), Out(200, 0)))
    must("ref-500", 500, Seq(Out(250, 0.01), Out(100, 0.02)))

    // Below 6.25 MHz, above 800 MHz, or a 7th output. The PLL cannot do these.
    mustNot("below-fout", 60, Seq(Out(5, 0.10)))
    mustNot("above-fout", 60, Seq(Out(900, 0.10)))
    mustNot("fout-just-under", 60, Seq(Out(6.24, 0)))
    mustNot("fout-just-over", 60, Seq(Out(801, 0)))
    mustNot("fout-zero", 60, Seq(Out(0, 0)))
    mustNot("ref-below-pfd", 10, Seq(Out(100, 0.05)))
    mustNot("ref-17", 17, Seq(Out(100, 0.05)))
    mustNot("ref-501", 501, Seq(Out(250, 0.05)))
    mustNot("ref-600", 600, Seq(Out(100, 0.05)))
    mustNot("ref-zero", 0, Seq(Out(100, 0.05)))
    mustNot("ref-neg", -10, Seq(Out(100, 0.05)))
    mustNot("no-outs", 60, Seq())
    mustNot("seven-outs", 60, Seq(20, 25, 40, 50, 80, 100, 120).map(f => Out(f, 0.05)))
    mustNot("eight-outs", 60, (1 to 8).map(i => Out(20.0 * i, 0.05)))
    mustReject("tol-100pct") {
      ClockSpecification(HertzNumber(100e6), tolerance = 1.0)
    }

    val seconds = (System.nanoTime() - t0) / 1e9
    val report = Report(solved, unsolved, refused, failures.toSeq)
    val line = f"PLL sweep ${seconds}%.1f s  $report"
    println(line)
    report
  }

  def main(args: Array[String]): Unit = {
    val log = new PrintStream(new java.io.FileOutputStream("/tmp/pll-sweep-detail.log"))
    log.println("start")
    log.flush()
    try {
    val report = Console.withOut(log)(run())
    log.flush()
    println(report)
    if (!report.ok) sys.exit(1)
    } catch {
      case t: Throwable =>
        t.printStackTrace(log)
        log.flush()
        sys.exit(2)
    } finally {
      log.flush()
    }
  }
}
