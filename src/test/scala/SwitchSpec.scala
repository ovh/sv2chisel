// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class SwitchSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  /*
  
  TODO: refacto type management with traits
  
  - main question being : shall we totally decouple hw types from sw types ?
  
  - one trait TypeKind 
    - HwTypeKind 
    - SwTypeKind
    - MixedTypeKind ?
    
  - one trait TypeClass
    - BoolTypeClass
    - NumTypeClass
    - DataTypeClass (hw on which you can do asUInt / asTypeOf)
    - 
  - there can be numeric sw and numeric hw types (or numeric mixed ?)
  
  - how to have a function that can passed a trait as argument to now whether to cast or not
  - for example a conditionally requires a BoolType
  
  
  
  */
  
  "SwitchSpec" should "be properly emitted" in {
    val result = emitInModule("""
      |wire a, b, c, d, e; 
      |wire [31:0] mem, prev; 
      |reg [31:0] res; 
      |
      |always @(posedge clk) begin
      |  case (1'b1)
      |    a:
      |      res <= prev;
      |    |{b, c}:
      |      res <= mem[31:12] << 12;
      |    |{a, d, e}:
      |      res <= $signed(mem[31:20]);
      |    &{a, b}:
      |      res <= $signed({mem[31], mem[7], mem[30:25], mem[11:8], 1'b0});
      |    b:
      |      res <= $signed({mem[31:25], mem[11:7]});
      |    default:
      |      res <= 1'bx;
      |  endcase
      |end
      """.stripMargin
    )
    debug(result)
    result should contain ("class Test() extends MultiIOModule {")
    
    result should contain ("val mem = Wire(Vec(32, Bool()))",
                            "val prev = Wire(UInt(32.W))",
                            "val res = Reg(UInt(32.W))")
    
    result should contain ("when(true.B === a) {",
                              "res := prev",
                            "} .elsewhen (true.B === Cat(b, c).orR()) {",
                              "res := mem(31,12).asUInt<<12",
                            "} .elsewhen (true.B === Cat(a, d, e).orR()) {",
                              "res := mem(31,20).asTypeOf(SInt(32.W)).asUInt",
                            "} .elsewhen (true.B === Cat(a, b).andR()) {",
                              "res := Cat(mem(31), mem(7), mem(30,25).asUInt, mem(11,8).asUInt, \"b0\".U(1.W)).asTypeOf(SInt(32.W)).asUInt",
                            "} .elsewhen (true.B === b) {",
                              "res := Cat(mem(31,25).asUInt, mem(11,7).asUInt).asTypeOf(SInt(32.W)).asUInt",
                            "} .otherwise {",
                              "res := \"b0\".U(1.W)",
                            "}")
    
  }

}