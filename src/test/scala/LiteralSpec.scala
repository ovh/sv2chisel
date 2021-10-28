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
    
    result should contain ("import chisel3._")
    
    result should contain ("class Test() extends MultiIOModule {")

    result should contain ("val bool = Wire(Bool())")
    result should contain ("val iint = RegInit(UInt(5.W), \"b101\".U(5.W))")
    result should contain ("val dec = WireDefault(UInt(5.W), 101.U(7.W))")
    result should contain ("val dec2 = WireDefault(UInt(5.W), 101.U)")
    result should contain ("val hex = WireDefault(UInt(5.W), \"hfff\".U(16.W))")
    
    result should contain ("bool := true.B")
    result should contain ("bool := false.B")
    result should contain ("iint := iint+1.U")
    
  }

}