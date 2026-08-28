package spinalextras.lib.soc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig, WishboneSlaveFactory}
import spinalextras.lib.bus.WishboneGlobalBus

import scala.language.postfixOps

/** Wishbone RCC: PLL_STATUS, BLOCK_RESET, CPU_RESET pulse, FABRIC_ALIVE. Not Lattice LMMI. */
object Rcc {
  val windowBase = 0xb4080000L
  val slotBytes = 0x400
  val windowBytes = 16 KiB

  val pllLmmi = SizeMapping(windowBase, slotBytes)
  val configLmmi = SizeMapping(windowBase + 0x400, slotBytes)
  val mapping = SizeMapping(windowBase + 0x800, slotBytes)

  def cameraLmmi(n: Int): SizeMapping =
    SizeMapping(windowBase + 0xC00 + n.toLong * slotBytes, slotBytes)

  val OffPllStatus = 0x00
  val OffBlockReset = 0x04
  val OffResetCause = 0x08 // sticky BOOT; W1C; POR latched at bitstream load
  val OffCpuReset = 0x10
  val OffFabricAlive = 0x14
  val OffRaz = 0x18
  val OffConfigRefresh = 0x1C
  val OffConfigScratch = 0x20 // CPU-domain probe; POR 0xC3
  val OffConfigPush = 0x24    // write operand byte into writer FIFO
  val OffConfigGo = 0x28      // write nRead, start one Class A command
  val OffConfigPop = 0x2C     // pop read byte; empty = 0xA5
  val OffConfigStat = 0x30    // bit0 writer busy
  val OffConfigReset = 0x34   // pulse: flush writer/mailbox FSM (not CONFIG_REFRESH)
  val configScratchReset = 0xC3
  /* Match zephyr/drivers/hwinfo.h RESET_* bit positions. */
  val resetCausePor = 3
  val resetCauseSoftware = 1
  val resetCausePll = 10
  val resetCausePorMask = 1 << resetCausePor
  val configPopEmpty = 0xA5   // empty pop; 0 is valid IDCODE/status

  val cpuResetCycles = 16

  def wishboneConfig: WishboneConfig = WishboneConfig(
    addressWidth = 32,
    dataWidth = 32,
    selWidth = 4,
    addressGranularity = AddressGranularity.BYTE,
    useERR = true
  )
}

class Rcc(val mapping: SizeMapping = Rcc.mapping,
          val busConfig: WishboneConfig = Rcc.wishboneConfig) extends Component {
  val io = new Bundle {
    val bus = slave(Wishbone(busConfig))
    val pllLock = in Bool() default True
    val cpuResetPulse = out Bool()
    val configRefreshPulse = out Bool()
    val configResetPulse = out Bool()
    val blockReset = out Bits(1 bits)
    val configPush = master Stream(Bits(8 bits))
    val configGo = out Bool()
    val configNrd = out UInt(8 bits)
    val configPop = slave Stream(Bits(8 bits))
    val configBusy = in Bool() default False
    val configWrFill = in UInt(5 bits) default 0
    val configRdOcc = in UInt(5 bits) default 0
  }
  noIoPrefix()

  if (io.bus.config.useERR) {
    io.bus.ERR := False
    io.bus.ERR.allowOverride()
  }

  val factory = new WishboneSlaveFactory(io.bus) {
    override def writeByteEnable() = null
  }
  val ctrl = factory.withOffset(mapping.base)

  ctrl.read(BufferCC(io.pllLock, init = True), Rcc.OffPllStatus, 0, documentation = "PLL_STATUS")

  io.blockReset.setAsReg() init 0
  ctrl.readAndWrite(io.blockReset, Rcc.OffBlockReset, 0, documentation = "BLOCK_RESET")

  ctrl.read(U(0, 1 bits), Rcc.OffRaz, 0, documentation = "RAZ")

  /* BOOT so a hart reset does not clear the sticky bits. */
  val fabricAliveCd = ClockDomain(
    clock = ClockDomain.current.readClockWire,
    reset = null,
    config = ClockDomainConfig(
      resetKind = BOOT,
      clockEdge = ClockDomain.current.config.clockEdge
    ),
    frequency = ClockDomain.current.frequency
  )
  fabricAliveCd.setSynchronousWith(ClockDomain.current)
  val fabricAlive = fabricAliveCd on {
    RegInit(False)
  }
  val resetCause = fabricAliveCd on {
    RegInit(B(Rcc.resetCausePorMask, 32 bits))
  }
  val pllLockSync = BufferCC(io.pllLock, init = True)
  when(!pllLockSync) {
    resetCause(Rcc.resetCausePll) := True
  }

  val cpuResetRemaining = Reg(UInt(log2Up(Rcc.cpuResetCycles + 1) bits)) init 0
  io.cpuResetPulse := cpuResetRemaining =/= 0
  when(cpuResetRemaining =/= 0) {
    cpuResetRemaining := cpuResetRemaining - 1
  }
  ctrl.onWrite(Rcc.OffCpuReset, documentation = "CPU_RESET") {
    when(io.bus.DAT_MOSI(0)) {
      cpuResetRemaining := Rcc.cpuResetCycles
      when(fabricAlive) {
        resetCause(Rcc.resetCauseSoftware) := True
      }
    }
  }
  ctrl.read(io.cpuResetPulse, Rcc.OffCpuReset, 0, documentation = "CPU_RESET")

  val configRefreshRemaining = Reg(UInt(log2Up(Rcc.cpuResetCycles + 1) bits)) init 0
  io.configRefreshPulse := configRefreshRemaining =/= 0
  when(configRefreshRemaining =/= 0) {
    configRefreshRemaining := configRefreshRemaining - 1
  }
  ctrl.onWrite(Rcc.OffConfigRefresh, documentation = "CONFIG_REFRESH") {
    when(io.bus.DAT_MOSI(0)) {
      configRefreshRemaining := Rcc.cpuResetCycles
    }
  }
  ctrl.read(io.configRefreshPulse, Rcc.OffConfigRefresh, 0, documentation = "CONFIG_REFRESH")

  val configResetRemaining = Reg(UInt(log2Up(Rcc.cpuResetCycles + 1) bits)) init 0
  io.configResetPulse := configResetRemaining =/= 0
  when(configResetRemaining =/= 0) {
    configResetRemaining := configResetRemaining - 1
  }
  ctrl.onWrite(Rcc.OffConfigReset, documentation = "CONFIG_RESET") {
    when(io.bus.DAT_MOSI(0)) {
      configResetRemaining := Rcc.cpuResetCycles
    }
  }
  ctrl.read(io.configResetPulse, Rcc.OffConfigReset, 0, documentation = "CONFIG_RESET")

  val configScratch = RegInit(B(Rcc.configScratchReset, 8 bits))
  ctrl.readAndWrite(configScratch, Rcc.OffConfigScratch, 0, documentation = "CONFIG_SCRATCH")

  io.configPush.valid := False
  io.configPush.payload := io.bus.DAT_MOSI(7 downto 0)
  val configMailboxIdle = configResetRemaining === 0
  ctrl.onWrite(Rcc.OffConfigPush, documentation = "CONFIG_PUSH") {
    when(configMailboxIdle) {
      io.configPush.valid := True
    }
  }

  val configNrd = Reg(UInt(8 bits)) init 0
  val configGoComb = False
  ctrl.onWrite(Rcc.OffConfigGo, documentation = "CONFIG_GO") {
    when(configMailboxIdle) {
      configNrd := io.bus.DAT_MOSI(7 downto 0).asUInt
      configGoComb := True
    }
  }
  /* Pulse one cycle after nRead is registered so CDC/same-clock both see 4. */
  io.configNrd := configNrd
  io.configGo := RegNext(configGoComb) init False

  io.configPop.ready := False
  ctrl.onRead(Rcc.OffConfigPop, documentation = "CONFIG_POP") {
    io.configPop.ready := True
  }
  ctrl.read(
    Mux(io.configPop.valid, io.configPop.payload, B(Rcc.configPopEmpty, 8 bits)),
    Rcc.OffConfigPop,
    0,
    documentation = "CONFIG_POP"
  )

  ctrl.read(
    Cat(io.configRdOcc.resize(4 bits), io.configWrFill.resize(3 bits), io.configBusy.asBits),
    Rcc.OffConfigStat,
    0,
    documentation = "CONFIG_STAT"
  )

  ctrl.read(resetCause, Rcc.OffResetCause, 0, documentation = "RESET_CAUSE")
  ctrl.onWrite(Rcc.OffResetCause, documentation = "RESET_CAUSE") {
    resetCause := resetCause & ~io.bus.DAT_MOSI
  }

  ctrl.read(fabricAlive, Rcc.OffFabricAlive, 0, documentation = "FABRIC_ALIVE")
  ctrl.onWrite(Rcc.OffFabricAlive, documentation = "FABRIC_ALIVE") {
    when(io.bus.DAT_MOSI(0)) {
      fabricAlive := True
    }
  }

  new DeviceTreeProvider(mapping.base, Rcc.slotBytes) {
    override def entryName: String = "rcc"
    override def compatible: Seq[String] = Seq("tinyvision,rcc")
  }

  def attach_bus(bus: WishboneGlobalBus): Unit = {
    io.bus <> bus.add_slave("rcc", mapping, "cpu")
  }

  def tieConfigMailbox(): Unit = {
    io.configPush.ready := True
    io.configPop.valid := False
    io.configPop.payload := 0
  }
}
