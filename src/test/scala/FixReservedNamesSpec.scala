// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class FixReservedNamesSpecs extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "FixReservedNames"
  it should "rename user-defined reset signal into ureset and omit clock" in {
    val result = emitInModule(s"""
      |localparam W = 1;
      |
      |input        clock;
      |input        reset;
      |reg [W-1:0]  counter;
      |
      |always @(posedge clock) begin
      |    if (reset) begin
      |        counter <= 0;            
      |    end else begin
      |        counter <= counter + 1;
      |    end
      |end
      """.stripMargin
    )
    result should containStr ("class Test() extends Module {")

    result should containStr ("val W = 1")
    result shouldNot containStr ("val clock = IO(Input(Bool()))")
    result shouldNot containStr ("val reset = IO(Input(Bool()))")
    result should containStr ("val ureset = IO(Input(Bool()))")
    result should containStr ("val counter = Reg(UInt(W.W))")
    
    result should containStr (
      "when(ureset) {",
        "counter := 0.U",
      "} .otherwise {",
        "counter := counter+1.U",
      "}",
      )
    
  }
  
  it should "rename user-defined reset signal into ureset and omit clk" in {
    val result = emitInModule(s"""
      |localparam W = 1;
      |
      |input        clk;
      |input        reset;
      |reg [W-1:0]  counter;
      |
      |// TODO: always @(posedge clk or posedge reset) begin
      |always @(posedge clk) begin
      |    if (reset) begin
      |        counter <= 0;            
      |    end else begin
      |        counter <= counter + 1;
      |    end
      |end
      """.stripMargin
    )
    result should containStr ("class Test() extends Module {")

    result should containStr ("val W = 1")
    result shouldNot containStr ("val clk = IO(Input(Bool()))")
    result shouldNot containStr ("val reset = IO(Input(Bool()))")
    result should containStr ("val ureset = IO(Input(Bool()))")
    result should containStr ("val counter = Reg(UInt(W.W))")
    
    result should containStr (
      "when(ureset) {",
        "counter := 0.U",
      "} .otherwise {",
        "counter := counter+1.U",
      "}",
      )
    
  }

}