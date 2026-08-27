package spinalextras.lib.soc

import spinal.core._
import spinal.lib._
import spinal.lib.bus.misc.SizeMapping
import spinal.lib.bus.wishbone.{AddressGranularity, Wishbone, WishboneConfig, WishboneSlaveFactory}
import spinalextras.lib.bus.WishboneGlobalBus

import scala.language.postfixOps

/** Wishbone RCC: PLL_STATUS, CPU_RESET pulse, FABRIC_ALIVE. Not Lattice LMMI. */
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
  val OffCpuReset = 0x10
  val OffFabricAlive = 0x14

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

  val cpuResetRemaining = Reg(UInt(log2Up(Rcc.cpuResetCycles + 1) bits)) init 0
  io.cpuResetPulse := cpuResetRemaining =/= 0
  when(cpuResetRemaining =/= 0) {
    cpuResetRemaining := cpuResetRemaining - 1
  }
  ctrl.onWrite(Rcc.OffCpuReset, documentation = "CPU_RESET") {
    when(io.bus.DAT_MOSI(0)) {
      cpuResetRemaining := Rcc.cpuResetCycles
    }
  }
  ctrl.read(io.cpuResetPulse, Rcc.OffCpuReset, 0, documentation = "CPU_RESET")

  /* BOOT so a hart reset does not clear the sticky bit. */
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
}
