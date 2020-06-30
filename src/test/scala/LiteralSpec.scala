// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import sv2chisel._

import logger._

import scala.util.Random
import org.scalatest._

class LiteralSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "LiteralSpec" should "be properly emitted" in {
    val result = emitInModule(s"""
      |
      |wire          bool;
      |reg [4:0]     iint = 5'b101;
      |
      |assign bool = 1'b1;
      |assign bool = 1'b0;
      |always @(posedge clk) begin
      |   iint <= iint + 1;
      |end
      """.stripMargin
    )
    
    result should contains ("import chisel3._")
    result should contains ("import fpga.utils.Literals._")
    
    result should contains ("class Test() extends MultiIOModule {")

    result should contains ("val bool = Wire(Bool())")
    result should contains ("val iint = RegInit(UInt(5.W), \"b101\".U(5.W))")
    
    result should contains ("bool := true.B")
    result should contains ("bool := false.B")
    result should contains ("iint := iint+1.U")
    
  }

}