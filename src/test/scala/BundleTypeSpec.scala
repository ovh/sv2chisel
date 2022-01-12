// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class BundleTypeSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "BundleType"
  
  def check(result: String) = { 
    result should containStr ("class t1_t extends Bundle {",
                              "val bool = Bool()",
                              "val uint = UInt(B.W)",
                              "val vec = Vec(B, Bool())",
                              "val vecU = Vec(A, UInt(B.W))",
                              "val matrix = Vec(A, Vec(B, Bool()))",
                            "}")
                            
    result should containStr ("val W = (new t1_t).getWidth")
    result should containStr ("class t2_t extends Bundle {",
                              "val bool = Bool()",
                              "val uint = UInt(W.W)",
                              "val vec = Vec(B, new t1_t)",
                              "val tpe = new t1_t",
                            "}")
    result should containStr ("val test_wire = Wire(Vec(L, new t2_t))")
    result should containStr ("val test_reg = Reg(Vec(L, new t2_t))")
    result should containStr ("val test_raw_bundle = Wire(new t2_t)")
    
    result should containStr ("test_reg(0).bool := true.B",
                              "test_reg(0).uint := 0.U",
                              "test_reg(0).vec := 0.U.asTypeOf(Vec(B, new t1_t))",
                              "test_reg(0).vec(0) := 0.U.asTypeOf(new t1_t)",
                              "test_reg(0).vec(0).uint := 0.U",
                              "test_reg(0).vec(0).vec(1) := false.B",
                              "test_reg(0).vec(0).vecU(1) := 0.U",
                              "test_reg(0).vec(0).matrix(1)(1) := false.B",
                              "test_reg(0).tpe.uint := 0.U"
                            )
    
    result should containStr ("test_wire := 0.U.asTypeOf(Vec(L, new t2_t))")
    result should containStr ("test_wire := ((1.U << (L*(new t2_t).getWidth))-1.U).asTypeOf(Vec(L, new t2_t))")
    result should containStr ("test_raw_bundle := 0.U.asTypeOf(new t2_t)")
  }
  
  it should "be properly emitted with def within the same module" in {
    val result = emitInModule(s"""
      |input clk;
      |
      |localparam A = 5;
      |localparam B = $$clog2(A + 8);
      |
      |typedef struct packed {
      |  logic                bool;
      |  logic        [B-1:0] uint;
      |  logic        [B-1:0] vec;
      |  logic [A-1:0][B-1:0] vecU;
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
      |localparam L = 5;
      |
      |t2_t [L-1:0] test_wire;
      |t2_t [L-1:0] test_reg;
      |t2_t test_raw_bundle;
      |
      |always @(posedge clk) begin
      |   test_reg[0].bool <= '1;
      |   test_reg[0].uint <= '0;
      |   test_reg[0].vec <= '{default: '0};
      |   test_reg[0].vec[0] <= '0;
      |   test_reg[0].vec[0].uint <= '0;
      |   test_reg[0].vec[0].vec[1] <= '0;
      |   test_reg[0].vec[0].vecU[1] <= '0;
      |   test_reg[0].vec[0].matrix[1][1] <= '0;
      |   test_reg[0].tpe.uint <= '0;
      |end
      |assign test_wire = '{default: '0};
      |assign test_wire = '{default: '1};
      |assign test_raw_bundle = '{default: '0};
      """.stripMargin
    )
    debug(result)
    result should containStr ("class Test() extends Module {")
    check(result)
  }
  
  it should "be properly emitted with def in a package" in {
    val pkg = wrapInPackage(s"""
      |localparam A = 5;
      |localparam B = $$clog2(A + 8);
      |
      |typedef struct packed {
      |  logic                bool;
      |  logic        [B-1:0] uint;
      |  logic        [B-1:0] vec;
      |  logic [A-1:0][B-1:0] vecU;
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
        """.stripMargin, 
        "my_package"
      )
    
    val mod = wrapInModule(s"""
      |input clk;
      |
      |import my_package::*;
      |
      |localparam L = 5;
      |
      |t2_t [L-1:0] test_wire;
      |t2_t [L-1:0] test_reg;
      |t2_t test_raw_bundle;
      |
      |always @(posedge clk) begin
      |   test_reg[0].bool <= '1;
      |   test_reg[0].uint <= '0;
      |   test_reg[0].vec <= '{default: '0};
      |   test_reg[0].vec[0] <= '0;
      |   test_reg[0].vec[0].uint <= '0;
      |   test_reg[0].vec[0].vec[1] <= '0;
      |   test_reg[0].vec[0].vecU[1] <= '0;
      |   test_reg[0].vec[0].matrix[1][1] <= '0;
      |   test_reg[0].tpe.uint <= '0;
      |end
      |assign test_wire = '{default: '0};
      |assign test_wire = '{default: '1};
      |assign test_raw_bundle = '{default: '0};
      """.stripMargin
    )
    val result = emit(pkg + mod)
    debug(result)
    result should containStr ("package object my_package {")
    result should containStr ("class Test() extends Module {")
    check(result)
  }

}