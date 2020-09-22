// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import sv2chisel._

import logger._

import scala.util.Random
import org.scalatest._
import java.io.{PrintStream, ByteArrayOutputStream}

class WireOrRegSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Info)
  
  "WireOrReg" should "raise critical in when a signal is either wire or reg " in {
    val out = new ByteArrayOutputStream()
    Logger.setOutput(new PrintStream(out))
    val result = emitInModule("""
        |wire clk;
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
    val stdout = out.toString
    Logger.reset()
    debug(stdout)
    result should contains ("class Test() extends MultiIOModule {")
    stdout should contains ( "[critical] Signal c is driven both by clock clk and assigned combinationally. This probably means that c is either a wire or a reg depending on a generate parameter. Please review and fix generated code. at :12" )
  }

}