package spinalextras.lib.mipi

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib.sim._
import spinal.lib._
import spinalextras.lib.Config
import spinalextras.lib.logging.{GlobalLogger, SignalLogger}

class PixelFlow2Fragment[T <: Data](val dataType: HardType[T]) extends Component {
  val io = new Bundle {
    val pixelFlow = slave(PixelFlow(dataType))
    val pixelFragment = master (Flow(Fragment(dataType)))
  }

  val overflow = Bool()
  // Hold the newest pixel until the next valid *or* until frame_valid falls so
  // we can mark EOF on that last pixel. continueWhen must be FALSE while
  // FV is high and valid is low — otherwise the stage drains between pixels
  // and the EOF beat is lost (no USB frames).
  val lastValidPixel = io.pixelFlow.toStream(overflow).stage()
  assert(!overflow)

  // last is a pulse on FV falling edge, not level ~FV. Level-last tagged every
  // early new-frame beat that still saw frame_valid=0 (byte2pixel Delay(fv)
  // lag after SOF) as EOF → extra empty UVC frames (~2× sof).
  val fvFall = !io.pixelFlow.frame_valid && RegNext(io.pixelFlow.frame_valid, False)

  io.pixelFragment <>
    lastValidPixel.
      continueWhen(~io.pixelFlow.frame_valid || io.pixelFlow.valid).
      addFragmentLast(fvFall).toFlow

  GlobalLogger(
    Set("mipi"),
    SignalLogger.concat("p2f", io.pixelFragment.lastFire.setName("lastFire"), io.pixelFlow.frame_valid, overflow)
  )
}

object PixelFlow2Fragment {
  def apply[T <: Data](pixelFlow : PixelFlow[T]) : Flow[Fragment[T]] = {
    val dut = new PixelFlow2Fragment(pixelFlow.dataType)
    dut.io.pixelFlow <> pixelFlow
    dut.io.pixelFragment
  }
}

class PixelFlow2FragmentTest extends AnyFunSuite {
  test("Basic") {
    Config.sim.doSim(
      new Component {
        val dut = new PixelFlow2Fragment(Bits(12 bit))

        val io = new Bundle {
          val pixelFlow = slave(PixelFlow(dut.dataType))
          val pixelFragment = master (Flow(Fragment(dut.dataType)))
          val meta = master (Flow(PixelFlowMeta()))
        }
        dut.io.pixelFlow <> io.pixelFlow
        dut.io.pixelFragment <> io.pixelFragment

        io.meta <> PixelFlowMetaProvider(dut.io.pixelFragment)
      }.setDefinitionName("PixelFlow2FragmentTest")
    ) { dut =>
      dut.clockDomain.forkStimulus(100 MHz)
      dut.io.pixelFlow.valid #= false
      dut.io.pixelFlow.frame_valid #= false
      dut.clockDomain.waitSampling(5)

      val sco = new ScoreboardInOrder[(Int, Boolean)]

      FlowMonitor(dut.io.pixelFragment, dut.clockDomain) {
        px => sco.pushDut((px.fragment.toInt, px.last.toBoolean))
      }

      for(n <- 0 until  5) {
        for(i <- 0 until  10) {
          dut.io.pixelFlow.frame_valid #= true
          dut.clockDomain.waitSampling(10)
          for (j <- 0 until  20) {
            dut.io.pixelFlow.valid #= true
            val pix = simRandom.nextInt(1 << 10)
            sco.pushRef((pix, i == 9 && j == 19))
            dut.io.pixelFlow.payload #= pix
            dut.clockDomain.waitSampling()
            dut.io.pixelFlow.valid #= false
            dut.io.pixelFlow.payload #= simRandom.nextInt(1 << 10)
          }
        }
        dut.io.pixelFlow.frame_valid #= false
        dut.clockDomain.waitSampling(1)
      }


      //
      //        dut.clockDomain.waitSampling(1)
      ////        for(j <- 0 to 10) {
      ////          dut.io.pixelFlow.frame_valid #= true
      ////          dut.io.pixelFlow.payload #= simRandom.nextInt(1 << 10)
      ////          dut.clockDomain.waitSampling(10)
      ////          dut.io.pixelFlow.frame_valid #= false
      ////          dut.io.pixelFlow.payload #= simRandom.nextInt(1 << 10)
      ////          dut.clockDomain.waitSampling(10)
      ////        }
      //      }
      //
      sco.checkEmptyness()
    }
  }
}
