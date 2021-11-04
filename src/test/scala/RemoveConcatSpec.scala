// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class RemoveConcatSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "Assign Pattern" should "be properly emitted" in {
    val result = emitInModule(s"""
      |// might contain more than the RAM due to moves
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
    result should contain ("class Test() extends MultiIOModule {")
    result should contain ("val auto_concat = Wire(new Bundle {",
                              "val bool = Bool()",
                              "val packsmall_i_WWW_1_1 = Vec(WWW-1, Bool())",
                              "val packsmall_i_0 = Bool()",
                            "})",
                            "auto_concat := pack(2*i).asTypeOf(auto_concat)",
                            "bool := auto_concat.bool",
                            "packsmall(i)(WWW-1,1) := auto_concat.packsmall_i_WWW_1_1",
                            "packsmall(i)(0) := auto_concat.packsmall_i_0")
    
    // seccond concat inline
    result should contain ("pack((2*i)+1) := Mux(bool, Cat(packsmall(i)(WWW-1,0).asUInt, \"b0\".U(1.W)), 0.U)")
    
  }

}