// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class IfElseSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "IfElseSpec" should "be properly emitted without begin end blocks" in {
    val result = emitInModule("""
      |wire a, b, c; 
      |reg [31:0] res; 
      |
      |always @(posedge clk) begin
      |  if (!a || b)
      |    res <= '0;
      |  else if (c)
      |    res <= 1;
      |  else
      |    res <= 2;
      |end
      """.stripMargin
    )
    result should contains ("class Test() extends MultiIOModule {")
    
    result should contains ( "when( !a || b) {",
                               "res := 0.U",
                             "} .elsewhen (c) {",
                               "res := 1.U",
                             "} .otherwise {",
                               "res := 2.U",
                             "}")
    
  }
  
  it should "be properly emitted with begin end blocks" in {
    val result = emitInModule("""
      |wire a, b, c; 
      |reg [31:0] res; 
      |
      |always @(posedge clk) begin
      |  if (!a || b) begin
      |    res <= '0;
      |  end else if (c) begin
      |    res <= 1;
      |  end else begin
      |    res <= 2;
      |  end
      |end
      """.stripMargin
    )
    result should contains ("class Test() extends MultiIOModule {")
    result should contains ( "when( !a || b) {",
                               "res := 0.U",
                             "} .elsewhen (c) {",
                               "res := 1.U",
                             "} .otherwise {",
                               "res := 2.U",
                             "}")
  }
  
  it should "be properly emitted without elsewhen for blocks" in {
    val result = emitInModule("""
      |wire a, b, c; 
      |reg [31:0] res; 
      |
      |always @(posedge clk) begin
      |  if (!a || b) begin
      |    res <= '0;
      |  end else begin 
      |    if (c) begin
      |      res <= 1;
      |    end else begin
      |      res <= 2;
      |    end
      |  end
      |end
      """.stripMargin
    )
    debug(result)
    result should contains ("class Test() extends MultiIOModule {")
    result should contains ( "when( !a || b) {",
                               "res := 0.U",
                             "} .otherwise {",
                                "when(c) {",
                                  "res := 1.U",
                                "} .otherwise {",
                                  "res := 2.U",
                                "}",
                             "}")
  }
  
  it should "be properly emitted without elsewhen for blocks 2" in {
    val result = emitInModule("""
      |wire a, b, c; 
      |reg [31:0] res; 
      |
      |always @(posedge clk) begin
      |  if (!a || b) begin
      |    res <= '0;
      |  end else begin 
      |    if (c) begin
      |      res <= 1;
      |    end else begin
      |      res <= 2;
      |    end
      |    a <= 1;
      |  end
      |end
      """.stripMargin
    )
    debug(result)
    result should contains ("class Test() extends MultiIOModule {")
    
    result should contains ( "when( !a || b) {",
                               "res := 0.U",
                             "} .otherwise {",
                                "when(c) {",
                                  "res := 1.U",
                                "} .otherwise {",
                                  "res := 2.U",
                                "}",
                                "a := true.B",
                             "}")
  }

}