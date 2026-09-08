package spinalextras.lib.tests.formal

import org.scalatest.funsuite.AnyFunSuite
import spinal.core._
import spinal.lib._
import spinalextras.lib.bus.{BusErrorEvent, PmbMissCompleter, BusErrorMaster}
import spinalextras.lib.formal.{ComponentWithFormalProperties, FormalProperties, FormalProperty}
import spinalextras.lib.logging.{FlowLogger, FlowLoggerConfig}
import spinalextras.lib.testing.{FormalTestSuite, GeneralFormalDut}

import scala.language.postfixOps

class BusErrorLogFormalDut() extends ComponentWithFormalProperties {
  val io = new Bundle {
    val a = slave(Flow(BusErrorEvent()))
    val b = slave(Flow(BusErrorEvent()))
  }
  io.a.setName("evt_a")
  io.b.setName("evt_b")
  val logger = new FlowLogger(Seq(
    (io.a.payload.setName("evt_a"), ClockDomain.current),
    (io.b.payload.setName("evt_b"), ClockDomain.current)
  ), FlowLoggerConfig(localDepth = 2))
  logger.io.flows(0).valid := io.a.valid
  logger.io.flows(0).payload := io.a.payload.asBits
  logger.io.flows(1).valid := io.b.valid
  logger.io.flows(1).payload := io.b.payload.asBits
  logger.io.log.ready := True

  override def covers(): Seq[FormalProperty] = new FormalProperties(this) {
    addFormalProperty(io.a.valid && io.b.valid)
    addFormalProperty(logger.io.captured_events >= U(2))
  }
}

class BusErrorLogFormalTest extends AnyFunSuite with FormalTestSuite {
  override def defaultDepth() = 24
  formalTests().foreach(t => test(t._1) { t._2() })

  override def generateRtlCover() = Seq(
    ("BusErrorLogCover", () => GeneralFormalDut(() => new BusErrorLogFormalDut())),
    ("PmbMissCompleterCover", () => GeneralFormalDut(() => new PmbMissCompleter(
      spinal.lib.bus.simple.PipelinedMemoryBusConfig(32, 32), BusErrorMaster.DBus)))
  )
  override def generateRtlProve() = Seq(
    ("BusErrorLogProve", () => GeneralFormalDut(() => new BusErrorLogFormalDut()))
  )
  override def generateRtl() = generateRtlProve()
}
