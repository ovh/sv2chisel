// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import sv2chisel._

import logger._

import scala.util.Random
import org.scalatest._

class DefLogicSpecs extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "RegInit" should "be properly emitted" in {
    val result = emitInModule(s"""
      |localparam WWW = 1;
      |localparam WW = 1;
      |
      |wire clk;
      |wire          change;
      |reg [WW-1:0]  counter = '0;
      |reg [WWW-1:0] current = '1;
      |
      |always @(posedge clk) begin
      |    if (change) begin
      |        if (current < (1<<WWW)-1) begin
      |            current <= current + 1;
      |        end else begin
      |            current <= 0;
      |        end
      |        counter <= 0;            
      |    end else begin
      |        counter <= counter + 1;
      |    end
      |end
      """.stripMargin
    )
    result should contains ("class Test() extends MultiIOModule {")

    result should contains ("val WWW = 1")
    result should contains ("val change = Wire(Bool())")
    result should contains ("val counter = RegInit(UInt(WW.W), 0.U)")
    result should contains ("val current = RegInit(UInt(WWW.W), ((1<<WWW)-1).U)")
    
    // TO DO 
    result should contains ("when(current < ((1<<WWW)-1).U) {")
    result should contains ("current := current+1.U")
    
  }

}