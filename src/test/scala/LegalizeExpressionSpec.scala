// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class LegalizeExpressionSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "LegalizeExpression" should "manage general types properly" in {
    val result = emitInModule("""
      |localparam [0:0] B_TRUE = 1;
      |localparam [0:0] B_FALSE = 0;
      |localparam [0:0] C_UNKNOWN;
      |localparam integer INTP = (B_TRUE ? 5 : 4) + C_UNKNOWN*B_FALSE;
      |
      |wire a, b, c; 
      |assign c = |{B_TRUE && a, (B_TRUE || B_FALSE) && b, C_UNKNOWN && b};
      |
      |wire [31:0] w;
      |wire [31:0] wu;
      |wire [31:0] z;
      |assign w[14:12] = 3'b000;
      |assign w[31:16] = $signed(w[10:0]);
      |assign w[15:0] = $signed(z[10:0]);
      |assign w[14:0] = $signed({z[5:0], z[7:6]});
      |assign wu = $signed(w);
      |assign w = z*z+z+1;
      |
      |wire [31:0] ww;
      |wire b;
      |assign ww[31:25] = w[6:5] == 2'b00 ? 7'b0100000 : 7'b0000000;
      |assign ww[31:25] = b ? 2'b01 : (b ? 2'b10 : 2'b00);
      """.stripMargin
    )
    debug(result)
    result should contain ("class Test() extends MultiIOModule {")
    result should contain ("val B_TRUE: Boolean = true")
    result should contain ("val B_FALSE: Boolean = false")
    result should contain ("val C_UNKNOWN: Boolean")
    result should contain ("val INTP = (if(B_TRUE) 5 else 4)+(if(C_UNKNOWN) 1 else 0)*(if(B_FALSE) 1 else 0)")
    result should contain ("val a = Wire(Bool())")
    result should contain ("val b = Wire(Bool())")
    result should contain ("val c = Wire(Bool())")
    result should contain ("c := Cat(B_TRUE.B && a, (B_TRUE.B || B_FALSE.B) && b, C_UNKNOWN.B && b).orR()")
    
    result should contain ("val w = Wire(Vec(32, Bool()))")
    result should contain ("val z = Wire(UInt(32.W))")
    result should contain ("w(14,12) := \"b000\".U(3.W).asBools")
    /// padding (bit extension is done properly)
    result should contain ("w(31,16) := w(10,0).asTypeOf(SInt(16.W)).asBools") 
    result should contain ("w(15,0) := z(10,0).asTypeOf(SInt(16.W)).asBools") 
    result should contain ("w(14,0) := Cat(z(5,0), z(7,6)).asTypeOf(SInt(15.W)).asBools")
    result should contain ("wu := w.asTypeOf(SInt(32.W)).asUInt") 
    result should contain ("w := (z*z+z+1.U).asTypeOf(Vec(32, Bool()))") 
    
    result should contain ("ww(31,25) := Mux(w(6,5).asUInt === \"b00\".U(2.W), \"b0100000\".U(7.W), \"b0000000\".U(7.W)).asBools")
    // ugly rendering but ... it works ...
    result should contain ("ww(31,25) := Mux(b, \"b01\".U(2.W).asTypeOf(Vec(7, Bool())), (Mux(b, \"b10\".U(2.W), \"b00\".U(2.W)).asTypeOf(Vec(7, Bool()))))")
  }
    
  it should "also deal with mixed signed and concat" in {
    val result = emitInModule("""
      |wire [31:0] lhsc;
      |wire [31:0] wu;
      |assign {lhsc[15:0], lhsc[31:16]} = $signed(wu[10:0]);
      """.stripMargin
    )
    debug(result)
    result should contain ("class Test() extends MultiIOModule {")
    result should contain ("val lhsc = Wire(Vec(32, Bool()))")
    result should contain ("val wu = Wire(Vec(32, Bool()))")
    result should contain ("val auto_concat = Wire(new Bundle {",
                              "val lhsc_15_0 = Vec(16, Bool())",
                              "val lhsc_31_16 = Vec(16, Bool())",
                            "})")
    result should contain ("auto_concat := wu(10,0).asTypeOf(SInt(32.W)).asTypeOf(auto_concat)")
    result should contain ("lhsc(15,0) := auto_concat.lhsc_15_0")
    result should contain ("lhsc(31,16) := auto_concat.lhsc_31_16")

  }
  
  it should "also deal with arithmetic shift right" in {
    val result = emitInModule("""
      |wire [16:0] a;
      |wire [31:0] res;
      |assign res = $signed(a) >>> 1;
      |assign res = $signed(a) >> 1;
      """.stripMargin
    )
    debug(result)
    result should contain ("class Test() extends MultiIOModule {")
    result should contain ("val a = Wire(UInt(17.W))")
    result should contain ("val res = Wire(UInt(32.W))")
    result should contain ("res := (a.asTypeOf(SInt(32.W)) >> 1.U).asUInt")
    result should contain ("res := a.asTypeOf(SInt(32.W)).asUInt >> 1")
  }
  
  it should "properly manage reduction operators" in {
    val result = emitInModule("""
      |wire [31:0] a;
      |wire [31:0] b;
      |wire res;
      |assign b[15:0] = '1;
      |assign res = |(a & ~b);
      """.stripMargin
    )
    debug(result)
    result should contain ("class Test() extends MultiIOModule {")
    result should contain ("val a = Wire(UInt(32.W))")
    result should contain ("val b = Wire(Vec(32, Bool()))")
    result should contain ("val res = Wire(Bool())")
    result should contain ("b(15,0) := (65535.U).asTypeOf(Vec(16, Bool()))")
    result should contain ("res := (a& ~b.asUInt).orR()")

  }
  
  it should "properly manage bitwise operators on Bool" in {
    val result = emitInModule("""
      |wire a;
      |wire b;
      |wire c;
      |wire w, r, o;
      |assign w = a ^ b;
      |assign r = w ^ c;
      |assign o = (w & c) | (a & b);
      |assign o = (w && c) || (a && b);
      """.stripMargin
    )
    debug(result)
    result should contain ("class Test() extends MultiIOModule {")
    result should contain ( "w := a^b" )
    result should contain ( "r := w^c" )
    result should contain ( "o := (w&c)|(a&b)" )
    result should contain ( "o := (w && c) || (a && b)" )
  }

}