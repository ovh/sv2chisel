package sv2chiselHelpersTests.tools

import sv2chisel.helpers.tools._
import sv2chiselHelpersTests.utils._

import chisel3._
import chisel3.stage.ChiselStage

import org.scalatest._
import flatspec._

class DeepWhenSpec extends AnyFlatSpec with VerilogMatchers {
  val setTestRunDir = Array("--target-dir", "test_run_dir")

  def getProcessedVerilog(m: => RawModule): String = {
    val annos = Seq(firrtl.stage.RunFirrtlTransformAnnotation(new DeepWhen()))
    (new ChiselStage()).emitVerilog(m, setTestRunDir, annos)
  }

  class BasicWhenInner() extends Module {
    val en  = IO(Input(Bool()))
    val in  = IO(Input(UInt(3.W)))
    val out = IO(Output(UInt(3.W)))

    val rout = Reg(chiselTypeOf(out))

    when(en) {
      val innerA = Reg(in.cloneType)
      innerA := in
      rout := innerA
    }.otherwise {
      val innerB = Reg(in.cloneType)
      innerB := in
      rout := innerB
    }
    out := rout
  }

  behavior of "DeepWhenTransform"

  it should "be useful" in {
    // check if it has been patched upstream
    val raw       = ChiselStage.emitVerilog(new BasicWhenInner())
    val processed = getProcessedVerilog(new BasicWhenInner())

    raw should contain("innerA <= in;", "innerB <= in;")
    processed shouldNot contain("innerA <= in;", "innerB <= in;")
  }
  it should "work with basic assignations" in {
    val processed = getProcessedVerilog(new BasicWhenInner())

    processed should contain("if (en) begin", "innerA <= in;", "end")
    processed should contain("if (~en) begin", "innerB <= in;", "end")
  }

  class MoreAdvancedInner() extends Module {
    val en  = IO(Input(Bool()))
    val in  = IO(Input(UInt(3.W)))
    val out = IO(Output(UInt(9.W)))

    val rout = Reg(chiselTypeOf(out))

    when(en) {
      val inner1 = Reg(new Bundle {
        val test  = in.cloneType
        val testV = Vec(2, in.cloneType)
      })
      inner1.test := in
      inner1.testV(0) := in
      inner1.testV(1) := in
      rout := inner1.asUInt
    }
    out := rout
  }

  it should "work with advanced assignations" in {
    val processed = getProcessedVerilog(new MoreAdvancedInner())
    processed should contain("if (en) begin", "inner1_test <= in;", "end")
    processed should contain("if (en) begin", "inner1_testV_0 <= in;", "end")
    processed should contain("if (en) begin", "inner1_testV_1 <= in;", "end")
  }

  class InnerInner() extends Module {
    val cond = IO(Input(Vec(2, Bool())))
    val in   = IO(Input(UInt(3.W)))
    val out  = IO(Output(UInt(3.W)))

    val rout = Reg(chiselTypeOf(out))

    when(cond(0)) {
      val inner1 = Reg(in.cloneType)
      inner1 := in
      when(cond(1)) {
        val inner2 = Reg(in.cloneType)
        inner2 := inner1
        rout := inner2
      }
    }
    out := rout
  }

  it should "work with inner inner assignations" in {
    val processed = getProcessedVerilog(new InnerInner())

    processed should contain("if (cond_0 & cond_1) begin", "inner2 <= inner1;", "end")
    processed should contain("if (cond_0) begin", "inner1 <= in;", "end")
  }
}
