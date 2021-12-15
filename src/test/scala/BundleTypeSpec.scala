// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class BundleTypeSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "BundleType" should "be properly emitted" in {
    val result = emitInModule(s"""
      |input clk;
      |
      |localparam A = 5;
      |localparam B = $$clog2(A + 8);
      |
      |typedef struct packed {
      |  logic                bool;
      |  logic        [B-1:0] uint;
      |  logic [A-1:0][B-1:0] matrix;
      |} t1_t;
      |localparam W = $$bits(t1_t);
      |
      |typedef struct packed {
      |  logic         bool;
      |  logic [W-1:0] uint;
      |  t1_t  [B-1:0] vec;
      |  t1_t          tpe;
      |} t2_t;
      |
      |t2_t [A-1:0] test_wire;
      |t2_t [A-1:0] test_reg;
      |
      |always @(posedge clk) begin
      |   test_reg[0].bool <= '1;
      |   test_reg[0].uint <= '0;
      |   test_reg[0].vec <= '{default: '0};
      |   test_reg[0].vec[0] <= '0;
      |   test_reg[0].vec[0].uint <= '0;
      |   test_reg[0].tpe.uint <= '0;
      |end
      |assign test_wire = '{default: '0};
      |assign test_wire = '{default: '1};
      """.stripMargin
    )
    debug(result)
    result should containStr ("class Test() extends MultiIOModule {")
      
    result should containStr ("class t1_t extends Bundle {",
                              "val bool = Bool()",
                              "val uint = Vec(B, Bool())",
                              "val matrix = Vec(A, Vec(B, Bool()))",
                            "}")
                            
    result should containStr ("val W = (new t1_t).getWidth")
    result should containStr ("class t2_t extends Bundle {",
                              "val bool = Bool()",
                              "val uint = Vec(W, Bool())",
                              "val vec = Vec(B, new t1_t)",
                              "val tpe = new t1_t",
                            "}")
    result should containStr ("val test_wire = Wire(Vec(A, new t2_t))")
    result should containStr ("val test_reg = Reg(Vec(A, new t2_t))")
    
    result should containStr ("test_reg(0).bool := true.B")
    result should containStr ("test_reg(0).uint := 0.U.asTypeOf(Vec(W, Bool()))")
    result should containStr ("test_reg(0).vec := 0.U.asTypeOf(Vec(B, new t1_t))")
    result should containStr ("test_reg(0).vec(0) := 0.U.asTypeOf(new t1_t)")
    result should containStr ("test_reg(0).vec(0).uint := 0.U.asTypeOf(Vec(B, Bool()))")
    result should containStr ("test_reg(0).tpe.uint := 0.U.asTypeOf(Vec(B, Bool()))")
    
    result should containStr ("test_wire := 0.U.asTypeOf(Vec(A, new t2_t))")
    result should containStr ("test_wire := ((1.U << (A*(new t2_t).getWidth))-1.U).asTypeOf(Vec(A, new t2_t))")
    
  }

}