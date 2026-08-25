package spinalextras.lib.soc.peripherals

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.Apb3
import spinal.lib.com.spi.ddr.Apb3SpiXdrMasterCtrl

import scala.collection.mutable
import scala.language.postfixOps

/**
 * XIP NOR remap: `nor = cpu + offset - linkBase`.
 *
 * Reset offset is the NOR base of the image (slot0 = 0x20_0000). Combined with
 * linkBase = 0x2020_0000 this is identity with today's 24-bit XIP truncate.
 * Offset[15:0] are tied off (64 KiB alignment). Applies to XIP fetches only.
 */
object XipOffset {
  val csrAddr = 0x4C
  val linkBase = BigInt("20200000", 16)
  val resetOffset = BigInt("200000", 16)
  val flashSize = BigInt("1000000", 16) // 16 MiB while XIP stays 3-byte
  val alignBits = 16

  def hw(cpuAddr: UInt, byteLast: UInt, offset: UInt,
         linkBase: BigInt = XipOffset.linkBase,
         flashSize: BigInt = XipOffset.flashSize): (UInt, Bool) = {
    val w = (cpuAddr.getWidth max offset.getWidth) + 2
    val sum = cpuAddr.resize(w) +^ offset.resize(w)
    val nor = sum -^ U(linkBase, w bits)
    val last = nor +^ byteLast.resize(w)
    val underflow = sum < U(linkBase, w bits)
    val overflow = underflow || (nor >= U(flashSize, w bits)) || (last >= U(flashSize, w bits))
    (nor.resize(24), overflow)
  }
}

object XipOffsetContext {
  private val bound = mutable.WeakHashMap[AnyRef, (UInt, BigInt, BigInt)]()

  def bind(offset: UInt,
           linkBase: BigInt = XipOffset.linkBase,
           flashSize: BigInt = XipOffset.flashSize): Unit = {
    bound(GlobalData.get) = (offset, linkBase, flashSize)
  }

  def get: Option[(UInt, BigInt, BigInt)] = bound.get(GlobalData.get)

  def fromCpu(cpuAddr: UInt, byteLast: UInt): (UInt, Bool) = {
    get match {
      case Some((offset, linkBase, flashSize)) =>
        XipOffset.hw(cpuAddr, byteLast, offset, linkBase, flashSize)
      case None =>
        (cpuAddr.resized, False)
    }
  }
}

class XipOffsetRemapDut extends Component {
  val io = new Bundle {
    val cpuAddr = in UInt (32 bits)
    val byteLast = in UInt (8 bits)
    val offset = in UInt (32 bits)
    val nor = out UInt (24 bits)
    val overflow = out Bool()
  }
  val (nor, overflow) = XipOffset.hw(io.cpuAddr, io.byteLast, io.offset)
  io.nor := nor
  io.overflow := overflow
}

/** Intercepts APB 0x4C; all other addresses pass through to the SPI controller. */
class XipOffsetApb(resetOffset: BigInt = XipOffset.resetOffset) extends Component {
  val io = new Bundle {
    val up = slave(Apb3(Apb3SpiXdrMasterCtrl.getApb3Config))
    val down = master(Apb3(Apb3SpiXdrMasterCtrl.getApb3Config))
    val idle = in Bool()
    val offset = out UInt (32 bits)
  }

  val offsetHi = Reg(UInt(16 bits)) init (resetOffset >> XipOffset.alignBits)
  io.offset := offsetHi @@ U(0, XipOffset.alignBits bits)

  val hit = io.up.PSEL.orR && (io.up.PADDR === XipOffset.csrAddr)
  io.down.PADDR := io.up.PADDR
  io.down.PWRITE := io.up.PWRITE
  io.down.PWDATA := io.up.PWDATA
  io.down.PENABLE := io.up.PENABLE
  io.down.PSEL := Mux(hit, B(0, io.up.PSEL.getWidth bits), io.up.PSEL)
  io.up.PREADY := Mux(hit, True, io.down.PREADY)
  io.up.PRDATA := Mux(hit, io.offset.asBits, io.down.PRDATA)

  val doWrite = hit && io.up.PENABLE && io.up.PWRITE && io.idle
  when(doWrite) {
    offsetHi := io.up.PWDATA(31 downto XipOffset.alignBits).asUInt
  }
}
