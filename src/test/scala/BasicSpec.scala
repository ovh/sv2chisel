// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class BasicSpecs extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "BasicSpecs" should "be properly emitted" in {
    val result = emitInModule(s"""
      |localparam A = 5;
      |localparam B = $$clog2(A + 8);
      |localparam C = A == 64 ? 1 : B;
      |localparam D = {"test", "0"+A};
      |localparam E = 2 ** A;
      |localparam hw = 8'b10000000;
      |
      |genvar i, bank;
      |wire w, z;
      |wire [7:0] state;
      |reg r;
      |wire s;
      |assign r = A == 64 ? 1 : 0;
      |assign w = A == 5 ? 1 : A;
      |assign z = state == hw ? r : w;
      |
      |wire [5:0] a;
      |wire b;
      |wire c;
      |assign c = a == '1 && b;
      |// avoid simplification => harder ...
      |wire [5:0] aa;
      |wire d;
      |assign aa[1] = '0;
      |assign c = aa == '1 && b;
      |
      |wire [63:0] ascii;
      |assign ascii = "test";
      |
      """.stripMargin
    )
    debug(result)
    result should contain ("class Test() extends MultiIOModule {")
    result should contain ("// genvar i, bank;")
    
    result should contain ("val A = 5")
    result should contain ("val B = util.log2Ceil(A+8)")
    result should contain ("val C = if(A == 64) 1 else B")
    result should contain ("val D = \"test\" + A") // no need for generic handling ... 
    result should contain ("val E = 1 << A")
    
    result should contain ("val w = Wire(Bool())")
    result should contain ("val r = Wire(Bool())")
    result should contain ("val z = Wire(Bool())")
    
    // this conversion is another challenge :
    result should contain ("r := Mux(A.U === 64.U, true.B, false.B)")
    result should contain ("w := Mux(A.U === 5.U, true.B, (A != 0).B)")
    result should contain ("z := Mux(state === hw, r, w)")
    
    result should contain ("c := a === 63.U && b")
    result should contain ("c := aa.asUInt === 63.U && b")
    // TO DO : leverage implicits ???
    // result should contain ("c := a === Ones && b")
    
    result should contain ("val ascii = Wire(UInt(64.W))")
    result should contain ("ascii := \"test\".V.asTypeOf(UInt(64.W))")

    
  }
  
  it should "deal with verilog literal tricks" in {
    val result = emitInModule(s"""
      |wire [31:0] test;
      |assign test = ~0;
      |assign test[5:0] = '0;
      |wire [31:0] test2;
      |assign test2 = ~0;
      |wire [31:0] test3;
      |assign test3 = ~(0+0);
      """.stripMargin
    )
    debug(result)
    result should contain ("class Test() extends MultiIOModule {")
    result should contain ("val test = Wire(Vec(32, Bool()))")
    result should contain ("test := ( ~0.U(32.W)).asBools")
    result should contain ("test(5,0) := (0.U).asTypeOf(Vec(6, Bool()))")
    result should contain ("val test2 = Wire(UInt(32.W))")
    result should contain ("test2 :=  ~0.U(32.W)")
    result should contain ("val test3 = Wire(UInt(32.W))")
    result should contain ("test3 :=  ~((0+0)).U(32.W)")
  }

}