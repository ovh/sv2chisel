// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class MacroSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "MacroSpec"
  
  it should "pass lexing & parsing (TODO: produce valid Chisel)" in {
    val result = emitInModule(s"""
      |`define TRUC 32
      |localparam TRUC = 32;
      |`define CALC(VAL1, VAL2, VAL3) \\
      | RESULT = VAL1 EXPR VAL2; \\
      | $$display("Result is %0d", RESULT);
      |localparam CALC = TRUC;
      |`ifdef TRUC
      |wire [`TRUC-1:0] test = `TRUC'b1;
      |`else
      |wire [32-1:0] test = 32'b1;
      |`endif
      """.stripMargin
    )
    
    result should containStr ("val TRUC = 32")
    result should containStr ("val CALC = TRUC")
    result should containStr ("""val test = WireDefault(UInt(32.W), "b1".U(`.TRUC.W))""")
    result should containStr ("""val test = WireDefault(UInt(32.W), "b1".U(32.W))""")
    
  }

}