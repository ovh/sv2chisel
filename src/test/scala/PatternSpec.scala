// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class PatternSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "Pattern" 
  
  it should "be emitted properly" in {
    val result = emitInModule("""
      |localparam WA = 64;
      |localparam WB = 32;
      |wire [WA-1:0] a;
      |wire [WA-1:0] r;
      |localparam PADDING = WA - WB;
      |wire c;
      |// Manually specified 
      |assign r = c ? a[WA-1:0] : {{PADDING{'0}}, a[WB-1:0]};
      |assign r = c ? a[WA-1:0] : {{PADDING{'1}}, a[WB-1:0]};
      |// Automated
      |assign r = c ? a[WA-1:0] : {'0, a[WB-1:0]};
      |assign r = c ? a[WA-1:0] : {'1, a[WB-1:0]};
      """.stripMargin
    )
    debug(result)
    // def
    result should containStr ( 
      "val a = Wire(Vec(WA, Bool()))",
      "val r = Wire(UInt(WA.W))"
    )
    
    // manual
    result should containStr ( 
      "r := Mux(c, a(WA-1,0).asUInt, Cat((VecInit.tabulate(PADDING)(_ => false.B)).asUInt, a(WB-1,0).asUInt))",
      "r := Mux(c, a(WA-1,0).asUInt, Cat((VecInit.tabulate(PADDING)(_ => true.B)).asUInt, a(WB-1,0).asUInt))" 
    )

    // auto
    result should containStr ( 
      "r := Mux(c, a(WA-1,0).asUInt, Cat(0.U((WA-WB).W), a(WB-1,0).asUInt))",
      "r := Mux(c, a(WA-1,0).asUInt, Cat(((1.U << (WA-WB))-1.U).asUInt, a(WB-1,0).asUInt))",
    ) 

  }

}