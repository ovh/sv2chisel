// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class IfElseGenSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "IfElseGenSpec"
  it should "be properly emitted without begin end blocks" in {
    val result = emitInModule("""
      |localparam a, b, c; 
      |wire [31:0] res; 
      |
      |generate
      |  if (!a || b)
      |    assign res = '0;
      |  else if (c)
      |    assign res = 1;
      |  else
      |    assign res = 2;
      |endgenerate
      """.stripMargin
    )
    result should containStr ("class Test() extends RawModule {")
    
    result should containStr ( "if(( !a) || b) {",
                               "res := 0.U",
                             "} else if(c) {",
                               "res := 1.U",
                             "} else {",
                               "res := 2.U",
                             "}")
    
  }
  
  
  
  it should "be properly emitted with begin end blocks" in {
    val result = emitInModule("""
      |localparam a, b, c; 
      |wire [31:0] res; 
      |
      |generate
      |  if (!a || b) begin
      |    assign res = '0;
      |  end else if (c) begin
      |    assign res = 1;
      |  end else begin
      |    assign res = 2;
      |  end
      |endgenerate
      """.stripMargin
    )
    result should containStr ("class Test() extends RawModule {")
    result should containStr ( "if(( !a) || b) {",
                               "res := 0.U",
                             "} else if(c) {",
                               "res := 1.U",
                             "} else {",
                               "res := 2.U",
                             "}")
  }
  
  it should "be properly emitted without elsewhen for blocks" in {
    val result = emitInModule("""
      |localparam a, b, c; 
      |wire [31:0] res; 
      |
      |generate
      |  if (!a || b) begin
      |    assign res = '0;
      |  end else begin 
      |    if (c) begin
      |      assign res = 1;
      |    end else begin
      |      assign res = 2;
      |    end
      |  end
      |endgenerate
      """.stripMargin
    )
    debug(result)
    result should containStr ("class Test() extends RawModule {")
    result should containStr ( "if(( !a) || b) {",
                               "res := 0.U",
                             "} else {",
                                "if(c) {",
                                  "res := 1.U",
                                "} else {",
                                  "res := 2.U",
                                "}",
                             "}")
  }
  
  it should "be properly emitted without elsewhen for blocks 2" in {
    val result = emitInModule("""
      |localparam a, b, c; 
      |wire [31:0] res;
      |wire resB;
      |
      |generate
      |  if (!a || b) begin
      |    assign res = '0;
      |  end else begin 
      |    if (c) begin
      |      assign res = 1;
      |    end else begin
      |      assign res = 2;
      |    end
      |    assign resB = 1;
      |  end
      |endgenerate
      """.stripMargin
    )
    debug(result)
    result should containStr ("class Test() extends RawModule {")
    
    result should containStr ( "if(( !a) || b) {",
                               "res := 0.U",
                             "} else {",
                                "if(c) {",
                                  "res := 1.U",
                                "} else {",
                                  "res := 2.U",
                                "}",
                                "resB := true.B",
                             "}")
  }
  
}