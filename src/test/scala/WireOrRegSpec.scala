// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

import java.io.{PrintStream, ByteArrayOutputStream}

class WireOrRegSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "WireOrReg"
  
  it should "infer simple conditional logic " in {
    val result = emitInModule("""
        |localparam [0:0] A = 1;
        |reg c;
        |generate 
        |  if(A) begin
        |    assign c = '1;
        |  end else begin
        |    always @(posedge clk) begin
        |      c <= '0;
        |    end
        |  end
        |endgenerate
        """.stripMargin
      )

    result should contain ("class Test() extends MultiIOModule {")
    result should containLineSet (
      "  val c = if (A) Wire(Bool()) ",
      "          else Reg(Bool()) ",
      "  if(A) {",
      "    c := true.B",
      "  } else {",
      "    c := false.B",
      "  }"
    )
  }
  
  it should "be aware of partial combinational conditional logic " in {
    val out = new ByteArrayOutputStream()
    Logger.setOutput(new PrintStream(out))
    val result = emitInModule("""
        |localparam [0:0] A = 1;
        |wire c;
        |generate 
        |  if(A) begin
        |    assign c = '1;
        |  end
        |endgenerate
        """.stripMargin
      )
    
    val stdout = out.toString
    Logger.reset()
    stdout should contain ( "[critical] Unable to guarantee complete scope definition for logic c resolved as wire without user-provided default value => adding default 0.U.asTypeOf(Bool) & ignoring A condition at :5" )
    
    result should contain ("class Test() extends RawModule {") // no clock
    result should containLineSet (
      "  val c = WireDefault(Bool(), false.B) ",
      "  if(A) {",
      "    c := true.B",
      "  }"
    )

  }
  
  it should "chill with partial clocked conditional logic " in {
    val result = emitInModule("""
        |localparam [0:0] A = 1;
        |reg c;
        |generate 
        |  if(A) begin
        |    always @(posedge clk) begin
        |      c <= '0;
        |    end
        |  end
        |endgenerate
        """.stripMargin
      )

    result should contain ("class Test() extends MultiIOModule {")
    result should containLineSet (
      "  val c = Reg(Bool()) ",
      "  if(A) {",
      "    c := false.B",
      "  }"
    )
  }
  
  it should "infer simple conditional logic with default" in {
    val result = emitInModule("""
        |localparam [0:0] A = 1;
        |reg c = '0;
        |generate 
        |  if(A) begin
        |    assign c = '1;
        |  end else begin
        |    always @(posedge clk) begin
        |      c <= '0;
        |    end
        |  end
        |endgenerate
        """.stripMargin
      )

    result should contain ("class Test() extends MultiIOModule {")
    result should containLineSet (
      "  val c = if (A) WireDefault(Bool(), false.B) ",
      "          else RegInit(Bool(), false.B) ",
      "  if(A) {",
      "    c := true.B",
      "  } else {",
      "    c := false.B",
      "  }"
    )
  }

  
  it should "infer more complex conditional logic " in {
    val result = emitInModule("""
        |localparam [0:0] A, B = 1;
        |reg c;
        |generate 
        |  if(A) begin
        |    assign c = '1;
        |  end else if(B) begin
        |    always @(posedge clk) begin
        |      if( c == '0) begin
        |        c <= '1;
        |      end else begin
        |        c <= '0;
        |      end
        |    end
        |  end else begin 
        |    assign c = '0;
        |  end
        |endgenerate
        """.stripMargin
      )

    result should contain ("class Test() extends MultiIOModule {")
    result should containLineSet (
      "  val c = if (A || (( !A) && ( !B))) Wire(Bool()) ",
      "          else if (( !A) && B) Reg(Bool()) ",
      "  if(A) {",
      "    c := true.B",
      "  } else if(B) {",
      "    when(c === false.B) {",
      "      c := true.B",
      "    } .otherwise {",
      "      c := false.B",
      "    }",
      "  } else {",
      "    c := false.B",
      "  }"
    )

  }
}