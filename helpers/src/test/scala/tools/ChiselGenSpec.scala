package sv2chiselHelpersTests.tools

import sv2chisel.helpers.tools._
import sv2chiselHelpersTests.utils._

import chisel3._

import org.scalatest._
import flatspec._

class ChiselGenSpec extends AnyFlatSpec with VerilogMatchers {
  class SimpleRegInit() extends Module {
    val in  = IO(Input(Bool()))
    val out = IO(Output(Bool()))
    val r   = RegInit(false.B)
    r := in
    out := r
  }

  behavior of "ChiselGen"

  val setTestRunDir = Array(
      "--target-dir",
      "test_run_dir"
  )

  it should "just works with syncronous Reset" in {
    val verilog = ChiselGen.emit(new SimpleRegInit(), setTestRunDir)
    verilog should contain(
        "always @(posedge clock) begin",
        "if (reset) begin",
        "r <= 1'h0;",
        "end else begin",
        "r <= in;",
        "end",
        "end"
    )
  }

  it should "just works with Preset" in {
    val verilog = ChiselGen.emitPreset(new SimpleRegInit(), setTestRunDir)
    verilog should contain("reg  r = 1'h0;")
    verilog should contain(
        "always @(posedge clock) begin",
        "r <= in;",
        "end"
    )
  }
}
