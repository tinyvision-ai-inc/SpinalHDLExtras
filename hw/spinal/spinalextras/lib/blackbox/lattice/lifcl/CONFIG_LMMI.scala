package spinalextras.lib.blackbox.lattice.lifcl

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.AddressMapping
import spinalextras.lib.bus.{LMMI, WishboneGlobalBus}
import spinalextras.lib.misc.ClockSpecification
import spinalextras.lib.soc.{DeviceTree, DeviceTreeProvider, Rcc}

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
 * LIFCL-33 / LIFCL-33U map to CONFIG_LMMIA (TN-02099 App D); same pin list.
 * SMCLK / RSTSMCLK come from CONFIG_CLKRST_CORE.
 */
class CONFIG_LMMI(lmmiEn: String = "EN") extends BlackBox {
  addGeneric("LMMI_EN", lmmiEn)
  setBlackBoxName("CONFIG_LMMIA")

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

/**
 * CONFIG LMMI host on LMMICLK. Sole master of ADDR_CFG: ROM refresh + FW
 * Class A. CPU never LMMI-masters the primitive (mailbox is RCC Wishbone).
 */
class ConfigLmmiFabricHost(commands: Seq[Seq[Int]] = NexusSysConfig.refreshFromFlash0()) extends Component {
  val io = new Bundle {
    val start = in Bool()
    val startFw = in Bool()
    val fwNrd = in UInt (8 bits)
    val fwWr = slave Stream (Bits(8 bits))
    val rd = master Stream (Bits(8 bits))
    val lmmi = master(LMMI(8, 8))
    val busy = out Bool()
    val wrFill = out UInt (5 bits)
    val rdOcc = out UInt (5 bits)
    val reset = in Bool() default False
    val mailboxReset = out Bool()
  }

  val writer = new ConfigLmmiCommandWriter(commands)
  val startHoldoff = Reg(UInt(4 bits)) init 15
  when(startHoldoff =/= 0) {
    startHoldoff := startHoldoff - 1
  }
  writer.io.start := io.start && (startHoldoff === 0)
  writer.io.startFw := io.startFw && (startHoldoff === 0)
  writer.io.fwNrd := io.fwNrd.resized
  writer.io.reset := io.reset
  io.mailboxReset := io.reset
  writer.io.fwWr << io.fwWr
  io.rd << writer.io.rd
  io.lmmi.cmd << writer.io.lmmi.cmd
  writer.io.lmmi.rsp << io.lmmi.rsp
  io.busy := writer.io.busy
  io.wrFill := writer.io.wrFill.resized
  io.rdOcc := writer.io.rdOcc.resized
}

object CONFIG_LMMI {
  /** OSCD HF_SED into CONFIG_CLKRST_CORE.SEDC_CLK; not used as a fabric clock. */
  val lmmiClkSpec = ClockSpecification(45 MHz)
  val lmmiTimeoutRdata = 0xA5 // LMMI timeout rsp; 0 is valid idle/POR
  val cpuStallRdata = 0x5A    // CPU cmd-stall rsp; distinct from timeout for devmem
  val cpuCmdFifoDepth = 4     // power-of-two; ≥ CPU/LMMI ratio

  def driveFromRcc(rcc: Rcc, writerGo: Bool, cpuCd: ClockDomain, lmmiCd: ClockDomain): ConfigLmmiFabricHost = {
    val host = lmmiCd on new ConfigLmmiFabricHost(NexusSysConfig.refreshFromFlash0())
    val resetL = if (ClockDomain.areSynchronous(cpuCd, lmmiCd)) {
      rcc.io.configResetPulse
    } else {
      lmmiCd on {
        BufferCC(rcc.io.configResetPulse, init = False)
      }
    }
    host.io.reset := resetL
    if (!ClockDomain.areSynchronous(cpuCd, lmmiCd)) {
      host.io.start := PulseCCByToggle(writerGo, cpuCd, lmmiCd)
      val goV = RegInit(False)
      val goP = Reg(UInt(8 bits)) init 0
      when(rcc.io.configResetPulse) {
        goV := False
        goP := 0
      }
      when(rcc.io.configGo) {
        goV := True
        goP := rcc.io.configNrd
      }
      val goSt = Stream(UInt(8 bits))
      goSt.valid := goV
      goSt.payload := goP
      val goCc = goSt.queue(cpuCmdFifoDepth, cpuCd, lmmiCd)
      when(goSt.fire) {
        goV := False
      }
      goCc.ready := True
      val nrdL = lmmiCd on { RegNextWhen(goCc.payload, goCc.valid) init U(0, 8 bits) }
      val goL = lmmiCd on { RegNext(goCc.valid) init False }
      host.io.startFw := goL
      host.io.fwNrd := nrdL
      rcc.io.configPush.queue(cpuCmdFifoDepth, cpuCd, lmmiCd) >> host.io.fwWr
      val rdCc = new StreamFifoCC(Bits(8 bits), cpuCmdFifoDepth, lmmiCd, cpuCd)
      rdCc.io.push << host.io.rd
      rcc.io.configPop << rdCc.io.pop
      rcc.io.configBusy := BufferCC(host.io.busy)
      rcc.io.configWrFill := BufferCC(host.io.wrFill)
      rcc.io.configRdOcc := rdCc.io.popOccupancy.resized
    } else {
      host.io.start := writerGo
      host.io.startFw := rcc.io.configGo
      host.io.fwNrd := rcc.io.configNrd
      host.io.fwWr << rcc.io.configPush
      rcc.io.configPop << host.io.rd
      rcc.io.configBusy := host.io.busy
      rcc.io.configWrFill := host.io.wrFill
      rcc.io.configRdOcc := host.io.rdOcc
    }
    host
  }

  def apply(lmmiClk: Bool, lmmiResetn: Bool, smClk: Bool, rstSmClk: Bool, lmmi: LMMI): CONFIG_LMMI = {
    val u = new CONFIG_LMMI()
    u.io.LMMICLK := lmmiClk
    u.io.LMMIRESETN := lmmiResetn
    u.io.SMCLK := smClk
    u.io.RSTSMCLK := rstSmClk
    u.attachLmmi(lmmi)
    u
  }

  /**
   * CONFIG_CLKRST_CORE + CONFIG_LMMIA. CPU mailbox is RCC (scratch/push/GO/pop).
   * Writer is the only LMMI master (ADDR_CFG). No CPU slave at mapping.
   *
   * CPU/2 fabric clock drives CLKRST.LMMI_CLK. LMMI_CLK_O only to
   * CONFIG_LMMIA.LMMICLK (map DRC 71003036). CONFIG_REFRESH pulses LMMI
   * reset then ISC_ENABLE_X / ISC_DISABLE / LSC_REFRESH.
   */
  def attach_bus(bus: WishboneGlobalBus, mapping: AddressMapping, rcc: Rcc): Unit = {
    val osc = OSCD(OSCDConfig.create(hf_sed = lmmiClkSpec))
    val lmmiClk = RegInit(False)
    lmmiClk := !lmmiClk
    KeepAttribute(lmmiClk)
    spinalextras.lib.Constraints.create_clock(lmmiClk, ClockDomain.current.frequency.getValue / 2)

    /* TN-02099: READY stays low until LMMIRESETN toggles. POR hold must not
     * start the refresh writer (that would loop-reconfigure). */
    val lmmiRstHold = Reg(UInt(10 bits)) init 1023
    val writerDelay = Reg(UInt(6 bits)) init 0
    val runWriter = RegInit(False)
    val refreshStart = rcc.io.configRefreshPulse
    when(refreshStart) {
      lmmiRstHold := 1023
      writerDelay := 0
      runWriter := True
    } elsewhen (lmmiRstHold =/= 0) {
      lmmiRstHold := lmmiRstHold - 1
    } elsewhen (runWriter) {
      writerDelay := 63
      runWriter := False
    } elsewhen (writerDelay =/= 0) {
      writerDelay := writerDelay - 1
    }
    val lmmiResetn = !ClockDomain.current.isResetActive && (lmmiRstHold === 0)
    val writerGo = writerDelay === 1
    val clkrst = osc.attachLmmi(lmmiClk, lmmiResetn)

    val cfg = new CONFIG_LMMI
    cfg.io.LMMICLK := clkrst.io.LMMI_CLK_O
    cfg.io.LMMIRESETN := clkrst.io.LMMI_RST
    cfg.io.SMCLK := clkrst.io.CFG_CLK
    cfg.io.RSTSMCLK := clkrst.io.SMCLK_RST

    val cpuCd = ClockDomain.current
    /* Fabric host clock = CLKRST.LMMI_CLK in, not LMMI_CLK_O (map DRC). */
    val lmmiCd = ClockDomain(
      clock = lmmiClk,
      reset = lmmiResetn,
      config = ClockDomainConfig(resetKind = SYNC, resetActiveLevel = LOW),
      frequency = FixedFrequency(cpuCd.frequency.getValue / 2)
    )

    val host = driveFromRcc(rcc, writerGo, cpuCd, lmmiCd)
    cfg.attachLmmi(host.io.lmmi)

    new DeviceTreeProvider(mapping.lowerBound, (mapping.highestBound - mapping.lowerBound + 1).toLong) {
      override def entryName: String = "sysconfig"
      override def compatible: Seq[String] = Seq("lattice,config-lmmi")
      override def appendDeviceTree(dt: DeviceTree): Unit = {
        super.appendDeviceTree(dt)
        dt.addEntry("lmmi-offset = <0x01>;", baseEntryPath: _*)
        dt.addEntry("mailbox = \"rcc\";", baseEntryPath: _*)
      }
    }
  }
}
