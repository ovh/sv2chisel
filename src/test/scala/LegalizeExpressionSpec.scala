// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class LegalizeExpressionSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "LegalizeExpression"
  
  it should "manage general types properly" in {
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
    result should containStr ("class Test() extends RawModule {")
    result should containStr ("val B_TRUE: Boolean = true")
    result should containStr ("val B_FALSE: Boolean = false")
    result should containStr ("val C_UNKNOWN: Boolean")
    result should containStr ("val INTP = (if(B_TRUE) 5 else 4)+((if(C_UNKNOWN) 1 else 0)*(if(B_FALSE) 1 else 0))")
    result should containStr ("val a = Wire(Bool())")
    result should containStr ("val b = Wire(Bool())")
    result should containStr ("val c = Wire(Bool())")
    result should containStr ("c := Cat(B_TRUE.B && a, (B_TRUE.B || B_FALSE.B) && b, C_UNKNOWN.B && b).orR()")
    
    result should containStr ("val w = Wire(Vec(32, Bool()))")
    result should containStr ("val z = Wire(UInt(32.W))")
    result should containStr ("w(14,12) := \"b000\".U(3.W).asBools")
    /// padding (bit extension is done properly)
    result should containStr ("w(31,16) := w(10,0).asTypeOf(SInt(16.W)).asBools") 
    result should containStr ("w(15,0) := z(10,0).asTypeOf(SInt(16.W)).asBools") 
    result should containStr ("w(14,0) := Cat(z(5,0), z(7,6)).asTypeOf(SInt(15.W)).asBools")
    result should containStr ("wu := w.asTypeOf(SInt(32.W)).asUInt") 
    result should containStr ("w := (((z*z)+z)+1.U).asTypeOf(Vec(32, Bool()))")
    
    result should containStr ("ww(31,25) := Mux(w(6,5).asUInt === \"b00\".U(2.W), \"b0100000\".U(7.W), \"b0000000\".U(7.W)).asBools")
    // ugly rendering but ... it works ...
    result should containStr ("ww(31,25) := Mux(b, \"b01\".U(2.W).asTypeOf(Vec(7, Bool())), (Mux(b, \"b10\".U(2.W), \"b00\".U(2.W)).asTypeOf(Vec(7, Bool()))))")
  }
    
  it should "also deal with mixed signed and concat" in {
    val result = emitInModule("""
      |wire [31:0] lhsc;
      |wire [31:0] wu;
      |assign {lhsc[15:0], lhsc[31:16]} = $signed(wu[10:0]);
      """.stripMargin
    )
    debug(result)
    result should containStr ("class Test() extends RawModule {")
    result should containStr ("val lhsc = Wire(Vec(32, Bool()))")
    result should containStr ("val wu = Wire(Vec(32, Bool()))")
    result should containStr ("val auto_concat = Wire(new Bundle {",
                              "val lhsc_15_0 = Vec(16, Bool())",
                              "val lhsc_31_16 = Vec(16, Bool())",
                            "})")
    result should containStr ("auto_concat := wu(10,0).asTypeOf(SInt(32.W)).asTypeOf(auto_concat)")
    result should containStr ("lhsc(15,0) := auto_concat.lhsc_15_0")
    result should containStr ("lhsc(31,16) := auto_concat.lhsc_31_16")

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
    result should containStr ("class Test() extends RawModule {")
    result should containStr ("val a = Wire(UInt(17.W))")
    result should containStr ("val res = Wire(UInt(32.W))")
    result should containStr ("res := (a.asTypeOf(SInt(32.W)) >> 1.U).asUInt")
    result should containStr ("res := a.asTypeOf(SInt(32.W)).asUInt >> 1")
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
    result should containStr ("class Test() extends RawModule {")
    result should containStr ("val a = Wire(UInt(32.W))")
    result should containStr ("val b = Wire(Vec(32, Bool()))")
    result should containStr ("val res = Wire(Bool())")
    result should containStr ("b(15,0) := 65535.U.asTypeOf(Vec(16, Bool()))")
    result should containStr ("res := (a&( ~b.asUInt)).orR()")

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
    result should containStr ("class Test() extends RawModule {")
    result should containStr ( "w := a^b" )
    result should containStr ( "r := w^c" )
    result should containStr ( "o := (w&c)|(a&b)" )
    result should containStr ( "o := (w && c) || (a && b)" )
  }
  
  it should "manage edge cases" in {
    val result = emitInModule("""
      |localparam PA = 2;
      |localparam PB = 2;
      |localparam PC = 6;
      |
      |typedef struct packed {
      |    logic [7:0] fieldA;
      |    logic [13:0] fieldB;
      |    logic [13:0] fieldC;
      |} my_struct_t;
      |
      |wire my_struct_t s;
      |// force vec bool fields 
      |assign s.fieldA[0] = '1;
      |assign s.fieldB[0] = '1;
      |assign s.fieldC[0] = '1;
      |
      |wire v;
      |wire [7:0] u;
      |
      |assign u = (s.fieldA == PC ? PB : 0) + $bits(my_struct_t)/8;
      |assign u = s.fieldB + PA + (s.fieldA == PC ? PB : 0) + $bits(my_struct_t)/8;
      |
      |// NB:(Chisel error): can't create Mux with heterogeneous types class chisel3.SInt and class chisel3.UInt
      |assign v = s.fieldC >= (s.fieldB + PA + (s.fieldA == PC ? PB : 0) + $bits(my_struct_t)/8);
      |assign v = s.fieldC == (s.fieldB + PA + (s.fieldA == PC ? PB : 0) + $bits(my_struct_t)/8);
      """.stripMargin
    )
    debug(result)
    
    result should containStr ("val v = Wire(Bool())")
    result should containStr ("val u = Wire(UInt(8.W))")
    result should containStr (
      "u := (Mux(s.fieldA.asUInt === PC.U, PB.U(8.W), 0.U))+((new my_struct_t).getWidth/8).U",
      "u := ((s.fieldB.asUInt+PA.U)+(Mux(s.fieldA.asUInt === PC.U, PB.U(8.W), 0.U)))+((new my_struct_t).getWidth/8).U"
    )

    result should containStr (
      "v := s.fieldC.asUInt >= (((s.fieldB.asUInt+PA.U)+(Mux(s.fieldA.asUInt === PC.U, PB.U, 0.U)))+((new my_struct_t).getWidth/8).U)",
      "v := s.fieldC.asUInt === (((s.fieldB.asUInt+PA.U)+(Mux(s.fieldA.asUInt === PC.U, PB.U, 0.U)))+((new my_struct_t).getWidth/8).U)"
    )
  }

  it should "properly manage arithmetic operators on Vec[Bool]" in {
    val result = emitInModule("""
      |wire [7:0] a;
      |wire [7:0] b;
      |wire [7:0] c;
      |// useless assign to preserve Vec[Bool] type
      |assign a[3:0] = '0;
      |assign b[3:0] = '0;
      |assign c = a + b;
      """.stripMargin
    )
    debug(result)
    result should containStr ("val a = Wire(Vec(8, Bool()))")
    result should containStr ("val b = Wire(Vec(8, Bool()))")
    result should containStr ("val c = Wire(UInt(8.W))")

    result should containStr ("c := a.asUInt+b.asUInt")

  }
}