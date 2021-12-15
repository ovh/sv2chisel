// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class RemoveConcatSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "RemoveConcats" 
  
  it should "properly remove concats or use Cat" in {
    val result = emitInModule(s"""
      |// might containStr more than the RAM due to moves
      |localparam DBLW = 2;
      |localparam WWW = 16;
      |
      |wire                     bool;
      |wire [DBLW-1:0][WWW:0]   pack;
      |wire [DBLW-1:0][WWW-1:0] packsmall;
      |
      |generate
      |  for (i = 0; i < DBLW/2; i++) begin: loop
      |    assign {bool, packsmall[i][WWW-1:1], packsmall[i][0]} = pack[2*i];
      |    assign pack[2*i+1] = bool ? {packsmall[i][WWW-1:0], 1'b0} : '0;
      |  end
      |endgenerate
      """.stripMargin
    )
    debug(result)
    result should containStr ("class Test() extends RawModule {")
    result should containStr ("val auto_concat = Wire(new Bundle {",
                              "val bool = Bool()",
                              "val packsmall_i_WWW_1_1 = Vec(WWW-1, Bool())",
                              "val packsmall_i_0 = Bool()",
                            "})",
                            "auto_concat := pack(2*i).asTypeOf(auto_concat)",
                            "bool := auto_concat.bool",
                            "packsmall(i)(WWW-1,1) := auto_concat.packsmall_i_WWW_1_1",
                            "packsmall(i)(0) := auto_concat.packsmall_i_0")
    
    // seccond concat inline
    result should containStr ("pack((2*i)+1) := Mux(bool, Cat(packsmall(i)(WWW-1,0).asUInt, \"b0\".U(1.W)), 0.U)")
    
  }
  
  it should "properly emit Cat with subrange and replicate patterns" in {
    val result = emitInModule(s"""
      |localparam WWW = 16;
      |localparam AAA = 4;
      |
      |wire [WWW-1:0] w;
      |wire [WWW-1:0] y;
      |
      |assign w = {{(WWW-AAA){1'b0}}, y[AAA-1:0]};
      """.stripMargin
    )
    debug(result)
    result should containStr ("import chisel3._")
    result should containStr ("import sv2chisel.helpers.vecconvert._")
    result should containStr ("import chisel3.util.Cat")

    result should containStr ("class Test() extends RawModule {")
    result should containStr (
                            "val w = Wire(UInt(WWW.W))",
                            "val y = Wire(Vec(WWW, Bool()))",
                            "w := Cat((VecInit.tabulate((WWW-AAA))(_ => false.B)).asUInt, y(AAA-1,0).asUInt)",
                          )
    
  }
  
  it should "properly emit Cat with subrange and replicate patterns in functions" in {
    val result = emit(wrapInPackage(s"""
      |localparam WWW = 16;
      |localparam AAA = 4;
      |function  [WWW-1:0] w;
      |  input logic [WWW-1:0] y;
      |  w = {{(WWW-AAA){1'b0}}, y[AAA-1:0]};
      |endfunction;
      """.stripMargin
    ))
    debug(result)
    result should containStr ("import chisel3._")
    result should containStr ("import sv2chisel.helpers.vecconvert._")
    result should containStr ("import chisel3.util.Cat")

    result should containStr (
                            "def w(y:Vec[Bool]): UInt = {",
                            "Cat((VecInit.tabulate((WWW-AAA))(_ => false.B)).asUInt, y(AAA-1,0).asUInt)",
                            "}"
                          )
    
  }

}