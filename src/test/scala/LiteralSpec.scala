// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class LiteralSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "LiteralSpec" should "be properly emitted" in {
    val result = emitInModule(s"""
      |
      |wire          bool;
      |reg [4:0]     iint = 5'b101;
      |wire [4:0]    dec = 7'd101;
      |wire [4:0]    dec2 = 101;
      |wire [4:0]    hex = 16'hfff;
      |
      |assign bool = 1'b1;
      |assign bool = 1'b0;
      |always @(posedge clk) begin
      |   iint <= iint + 1;
      |end
      """.stripMargin
    )
    
    result should containStr ("import chisel3._")
    
    result should containStr ("class Test() extends Module {")

    result should containStr ("val bool = Wire(Bool())")
    result should containStr ("val iint = RegInit(UInt(5.W), \"b101\".U(5.W))")
    result should containStr ("val dec = WireDefault(UInt(5.W), 101.U(7.W))")
    result should containStr ("val dec2 = WireDefault(UInt(5.W), 101.U)")
    result should containStr ("val hex = WireDefault(UInt(5.W), \"hfff\".U(16.W))")
    
    result should containStr ("bool := true.B")
    result should containStr ("bool := false.B")
    result should containStr ("iint := iint+1.U")
    
  }

}