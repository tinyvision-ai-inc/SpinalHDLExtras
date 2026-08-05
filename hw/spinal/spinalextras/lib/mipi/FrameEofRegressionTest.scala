package spinalextras.lib.mipi

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.sim.{FlowMonitor, ScoreboardInOrder}
import spinalextras.lib.Config
import spinalextras.lib.mipi.MIPIDataTypes.RAW8

import scala.collection.mutable.ArrayBuffer

/**
 * Regressions for flir_uab image/black blink and the "no frames" continueWhen bug:
 *
 * 1. PixelFlow2Fragment must hold the last pixel until FV falls (else no EOF).
 * 2. last must be FV falling-edge, not level ~FV (else early SOF pixels → extra EOFs).
 * 3. byte2pixel must tag in-frame as fv||Delay(fv) so post-SOF pixels are not FV=0.
 */
class FrameEofRegressionTest extends AnyFunSuite {

  test("PixelFlow2Fragment: one last per frame with inter-pixel gaps") {
    Config.sim.doSim(new PixelFlow2Fragment(Bits(8 bits))) { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.pixelFlow.valid #= false
      dut.io.pixelFlow.frame_valid #= false
      dut.io.pixelFlow.payload #= 0
      dut.clockDomain.waitSampling(5)

      val lasts = ArrayBuffer[Boolean]()
      val pixels = ArrayBuffer[Int]()
      FlowMonitor(dut.io.pixelFragment, dut.clockDomain) { px =>
        pixels += px.fragment.toInt
        lasts += px.last.toBoolean
      }

      def sendPixel(v: Int): Unit = {
        dut.io.pixelFlow.frame_valid #= true
        dut.io.pixelFlow.valid #= true
        dut.io.pixelFlow.payload #= v
        dut.clockDomain.waitSampling()
        dut.io.pixelFlow.valid #= false
        // Gap while FV high — stage must HOLD (continueWhen false)
        dut.clockDomain.waitSampling(3)
      }

      for (frame <- 0 until 3) {
        for (i <- 0 until 8) sendPixel(frame * 16 + i)
        dut.io.pixelFlow.frame_valid #= false
        dut.clockDomain.waitSampling(5)
      }

      dut.clockDomain.waitSampling(10)

      assert(pixels.size == 24, s"expected 24 pixels, got ${pixels.size}: $pixels")
      assert(lasts.count(identity) == 3, s"expected 3 lasts, got ${lasts.count(identity)} in $lasts")
      // Exactly one last, on the last pixel of each frame
      assert(lasts(7) && lasts(15) && lasts(23), s"last not on frame ends: $lasts")
      assert(lasts.count(identity) == 3)
    }
  }

  test("PixelFlow2Fragment: early FV=0 pixels must not each become EOF") {
    // Old bug: addFragmentLast(~FV) → every beat while FV low is last.
    Config.sim.doSim(new PixelFlow2Fragment(Bits(8 bits))) { dut =>
      dut.clockDomain.forkStimulus(10)
      dut.io.pixelFlow.valid #= false
      dut.io.pixelFlow.frame_valid #= false
      dut.io.pixelFlow.payload #= 0
      dut.clockDomain.waitSampling(5)

      val lasts = ArrayBuffer[Boolean]()
      FlowMonitor(dut.io.pixelFragment, dut.clockDomain) { px =>
        lasts += px.last.toBoolean
      }

      // Simulate byte2pixel Delay(fv) lag after SOF: first pixels with FV still 0
      for (i <- 0 until 4) {
        dut.io.pixelFlow.frame_valid #= false
        dut.io.pixelFlow.valid #= true
        dut.io.pixelFlow.payload #= i
        dut.clockDomain.waitSampling()
        dut.io.pixelFlow.valid #= false
        dut.clockDomain.waitSampling()
      }
      // Real in-frame pixels
      for (i <- 4 until 12) {
        dut.io.pixelFlow.frame_valid #= true
        dut.io.pixelFlow.valid #= true
        dut.io.pixelFlow.payload #= i
        dut.clockDomain.waitSampling()
        dut.io.pixelFlow.valid #= false
        dut.clockDomain.waitSampling(2)
      }
      dut.io.pixelFlow.frame_valid #= false
      dut.clockDomain.waitSampling(8)

      val nLast = lasts.count(identity)
      assert(nLast == 1,
        s"early FV=0 pixels must not create extra EOFs; lasts=$nLast in $lasts")
    }
  }

  test("byte2pixel+P2F: SOF/FE yields one lastFire per frame (flir-like gear)") {
    // flir_uab-ish: 2 lane × gear8 → 16b geared, 4×RAW8 outputs → DT_WIDTH=32 → byte_clock_fifo
    val cfg = MIPIConfig(numRXLanes = 2, rxGear = 8, outputLanes = 4, refDt = RAW8, dphyByteFreq = 90 MHz)
    val pixelFreq = 90 MHz

    Config.sim
      .withConfig(Config.spinalConfig.copy(defaultClockDomainFrequency = FixedFrequency(pixelFreq)))
      .doSim(
        new Component {
          val byteCd = ClockDomain.external("byte", frequency = FixedFrequency(cfg.dphyByteFreq))
          val b2p = byte2pixel(cfg, byteCd, pixel_cd = ClockDomain.current)
          val frag = PixelFlow2Fragment(b2p.io.pixelFlow)
          val io = new Bundle {
            val mipi_header = slave Flow (MIPIPacketHeader())
            val payload = slave Flow (Bits(cfg.GEARED_LANES bits))
            val pixelFragment = master Flow (Fragment(Bits(cfg.DT_WIDTH bits)))
          }
          b2p.io.mipi_header << io.mipi_header
          b2p.io.payload << io.payload
          io.pixelFragment << frag
        }.setDefinitionName("B2pP2fEofTest")
      ) { top =>
        val byteCd = top.byteCd
        top.clockDomain.forkStimulus(pixelFreq)
        byteCd.forkStimulus(cfg.dphyByteFreq)

        top.io.mipi_header.valid #= false
        top.io.payload.valid #= false
        byteCd.waitSampling(5)
        top.clockDomain.waitSampling(5)

        val lasts = ArrayBuffer[Int]()
        FlowMonitor(top.io.pixelFragment, top.clockDomain) { px =>
          if (px.last.toBoolean) lasts += 1
        }

        def hdr(dt: Int, longAv: Boolean, long: Boolean): Unit = {
          top.io.mipi_header.payload.datatype #= dt
          top.io.mipi_header.payload.is_long_packet #= long
          top.io.mipi_header.payload.is_long_av_packet #= longAv
          top.io.mipi_header.payload.word_count #= 2560
          top.io.mipi_header.payload.virtual_channel #= 0
          top.io.mipi_header.payload.ecc #= 0
          top.io.mipi_header.payload.checksum #= 0
          top.io.mipi_header.payload.virtual_channel_ext #= 0
          top.io.mipi_header.valid #= true
          byteCd.waitSampling()
          top.io.mipi_header.valid #= false
          byteCd.waitSampling()
        }

        def sendBytes(n: Int): Unit = {
          for (_ <- 0 until n) {
            top.io.payload.valid #= true
            top.io.payload.payload #= simRandom.nextLong() & ((1L << cfg.GEARED_LANES) - 1)
            byteCd.waitSampling()
            top.io.payload.valid #= false
            if (simRandom.nextBoolean()) byteCd.waitSampling()
          }
        }

        val nFrames = 4
        for (_ <- 0 until nFrames) {
          hdr(0, longAv = false, long = false) // SOF
          hdr(cfg.refDt.id, longAv = true, long = true)
          sendBytes(64)
          byteCd.waitSampling(10)
          hdr(1, longAv = false, long = false) // FE
          byteCd.waitSampling(30)
          top.clockDomain.waitSampling(30)
        }

        top.clockDomain.waitSampling(50)

        assert(lasts.sum == nFrames,
          s"expected $nFrames fragment lasts (one per SOF/FE), got ${lasts.sum}")
      }
  }
}
