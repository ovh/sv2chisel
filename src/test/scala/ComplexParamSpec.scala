// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class ComplexParamSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  // Logger.setLevel("sv2chisel.transforms.InferParamTypes", LogLevel.Debug)
  
  behavior of "ComplexParam"
  
  it should "support assign patterns" in {
    val result = emitInModule(s"""
      |// might containStr more than the RAM due to moves
      |input clk;
      |localparam DBLW = 2;
      |localparam WWW = 16;
      |localparam IT_LEFT = (64 * 2) + (1<<(WWW-2));
      |localparam IT_RIGHT = (64 * 2) + (1<<(WWW-1));
      |localparam int IT_DOWN[1:0] = '{IT_LEFT, IT_RIGHT}; 
      |localparam int IT_UP[0:1] = '{IT_LEFT, IT_RIGHT}; 
      |
      |wire                          clk;
      |reg  signed [DBLW-1:0][64:0]  count = '0;
      |reg  [DBLW-1:0][63:0]         stats_count = '0;
      |wire signed [DBLW-1:0][64:0]  count_live;
      |wire [DBLW-1:0][63:0]         stats_total = '0;
      |wire [DBLW-1:0][DBLW+WWW-1:0] test;
      |// no reason to be setup like that but it is legal and must be supported...
      |reg  [DBLW-1:0]               en = '{default:'0}; 
      |reg  [DBLW-1:0][64:0]         cnt = '{default:'0}; 
      |reg  [DBLW-1:0][64:0]         cnter = '{default:'1};
      |
      |for (i = 0; i < DBLW; i++) begin: total_items_loop
      |    always @(posedge clk) begin
      |        if (count_live[i] > IT_DOWN[i]) begin
      |            count[i] <= IT_DOWN[i];
      |            stats_count[i] <= stats_count[i] + 1;
      |        end
      |      en[i] <= '1;
      |      cnt[i] <= count[i];
      |      cnter[i] <= cnt[i];
      |    end
      |    assign stats_total[i] = count[i];
      |end
      |
      |assign test[DBLW] = '0;
      |
      |wire [DBLW-1:0][WWW-1:0]    a_addr;
      |reg  [DBLW+1-1:0][WWW-1:0]  b_addr;
      |reg  [DBLW+1-1:0]           b_weird;
      |
      |always @(posedge clk) begin
      |   b_addr[DBLW-1:0] <= a_addr;
      |   b_weird[DBLW-1:0] <= {DBLW{a_addr[0][WWW-1]}};
      |end
      """.stripMargin
    )
    debug(result)
    result should containStr ("import chisel3._")
    result should containStr ("import sv2chisel.helpers.vecconvert._")
    
    result should containStr ("class Test() extends Module {")

    result should containStr ("val IT_DOWN: Seq[Int] = Seq(IT_RIGHT, IT_LEFT)")
    result should containStr ("val IT_UP: Seq[Int] = Seq(IT_LEFT, IT_RIGHT)")
    
    result should containStr ("val count = RegInit(Vec(DBLW, SInt(65.W)), 0.U.asTypeOf(Vec(DBLW, SInt(65.W))))")
    // IDEAL: 
    // result should containStr ("val count = RegInit(VecInit.tabulate(DBLW)(_ => 0.S(65.W))")
    
    result should containStr ("val stats_count = RegInit(Vec(DBLW, UInt(64.W)), 0.U.asTypeOf(Vec(DBLW, UInt(64.W))))")
    result should containStr ("val count_live = Wire(Vec(DBLW, SInt(65.W)))")
    result should containStr ("val stats_total = WireDefault(Vec(DBLW, UInt(64.W)), 0.U.asTypeOf(Vec(DBLW, UInt(64.W))))")
    
    result should containStr ("val en = RegInit(Vec(DBLW, Bool()), 0.U.asTypeOf(Vec(DBLW, Bool())))")
    result should containStr ("val cnt = RegInit(Vec(DBLW, UInt(65.W)), 0.U.asTypeOf(Vec(DBLW, UInt(65.W))))")
    
    // TO DO:
    // result should containStr ("val en = RegInit(VecInit(Seq.fill(DBLW)(false.B)))")
    // OR (a few more characters but less functions and more extendable)
    // a bit more obscur however
    // result should containStr ("val en = RegInit(VecInit.tabulate(DBLW)(_ => false.B))")
    
    result should containStr ("val cnter = RegInit(Vec(DBLW, UInt(65.W)), ((1.U << (DBLW*65))-1.U).asTypeOf(Vec(DBLW, UInt(65.W))))")
    // to do 
    // result should containStr ("val cnter = RegInit(VecInit.tabulate(DBLW)(_ => ((1.U<<65)-1.U))")
    
    
    result should containStr ("for(i <- 0 until DBLW){")
    result should containStr ("when(count_live(i).asUInt > IT_DOWN(i).U) {")
    // warning : SInt should have priority over UInt - See gitlab issue #11
    // result should containStr ("when(count_live(i) > IT_DOWN(i).S) {")
    result should containStr ("count(i) := IT_DOWN(i).S(65.W)")
    result should containStr ("stats_count(i) := stats_count(i)+1.U")
    result should containStr ("stats_total(i) := count(i).asTypeOf(UInt(64.W))")
    
    result should containStr ("b_addr(DBLW-1,0) := a_addr")
    result should containStr ("b_weird(DBLW-1,0) := VecInit.tabulate(DBLW)(_ => a_addr(0)(WWW-1))")

    
  }
  
  it should "support complex tables" in {
    val result = emitInModule(s"""
      |// comment
      |localparam string STR = "string";
      |localparam string TABLE_STR [2:0] = '{"test", "truc", "bidule"};
      |localparam logic [1:0] TABLE_CONCAT [2:0] = '{2'd3, 2'd2, 2'd1};
      |localparam DBLW = 5;
      |localparam logic [DBLW-1:0]       en  = '{default:'0}; 
      |localparam logic [DBLW-1:0][64:0] cnt = '{default:'0};
      |
      |// avoid UInt inference for en
      |localparam enH = en[0];
      |localparam enT = en[DBLW-1];
      """.stripMargin
    )
    debug(result)
    result should containStr ("import chisel3._")
    
    result should containStr ("val STR = \"string\"")
    // NB: reverse is expected to preserve the indexes TABLE_STR[0] = "bidule"
    result should containStr ("val TABLE_STR: Seq[String] = Seq(\"bidule\", \"truc\", \"test\")")
    
    result should containStr ("val TABLE_CONCAT: Seq[UInt] = Seq(1.U(2.W), 2.U(2.W), 3.U(2.W))")
    result should containStr ("val en: UInt = 0.U")
    result should containStr ("val cnt: Vec[UInt] = 0.U.asTypeOf(Vec(DBLW, UInt(65.W)))")
  }
  
  it should "support complex tables in package" in {
    val result = emit(wrapInPackage(s"""
      |// comment
      |localparam string STR = "string";
      |localparam string TABLE_STR [2:0] = '{"test", "truc", "bidule"};
      |localparam logic [1:0] TABLE_CONCAT [2:0] = '{2'd3, 2'd2, 2'd1};
      |localparam DBLW = 5;
      |localparam logic [DBLW-1:0]       en  = '{default:'0}; 
      |localparam logic [DBLW-1:0][64:0] cnt = '{default:'0};
      |
      """.stripMargin
    ))
    debug(result)
    result should containStr ("import chisel3._")
    
    result should containStr ("val STR = \"string\"")
    // NB: reverse is expected to preserve the indexes TABLE_STR[0] = "bidule"
    result should containStr ("val TABLE_STR: Seq[String] = Seq(\"bidule\", \"truc\", \"test\")")
    
    result should containStr ("val TABLE_CONCAT: Seq[UInt] = Seq(1.U(2.W), 2.U(2.W), 3.U(2.W))")
    result should containStr ("val en: UInt = 0.U")
    result should containStr ("val cnt: Vec[UInt] = 0.U.asTypeOf(Vec(DBLW, UInt(65.W)))")
  }
  
  it should "support explicit hardware parameters in instances" in {
    val p = wrapInPackage(s"""
          |localparam WIDTH = 5;
          |localparam LEN = 2;
          |localparam logic [1:0] SEQ_VALUE [LEN:0] = '{2'd3, 2'd2, 2'd1};
          |
          |localparam logic [WIDTH-1:0] INIT_VALUE = '0;
          |localparam logic [1:0] SEQ_UINT [2:0];
      """.stripMargin, "test_p"
    )
    val inner = """
          |
          |import test_p::WIDTH;
          |
          |module my_module #(
          |  parameter logic [WIDTH-1:0] INIT_VALUE = '0,
          |  parameter logic [1:0] SEQ_UINT [2:0],
          |  parameter LABELS,
          |  parameter logic [WIDTH-1:0] LABEL_SEQ [LABELS-1:0],
          |  parameter logic [WIDTH-1:0] LABEL_SEQ2 [WIDTH-1:0],
          |  parameter TEST
          |)(
          |  input a,
          |  output b
          |);
          |wire w;
          |assign w = a == SEQ_UINT[LABELS] ? a : '0;
          |assign b = TEST ? w : '0;
          |endmodule
        """.stripMargin
        
    val main = wrapInModule("""
          |input  a;
          |input  b;
          |
          |my_module #(.INIT_VALUE('0), .SEQ_UINT(test_p::SEQ_VALUE), .TEST(0)) inst(
          |  .a(a),
          |  .b(b)
          |);
        """.stripMargin)
    
    val result = emit(p + inner + main)
    debug(result)
    result should containStr ("import chisel3._")
    
    result should containStr (
      "package object test_p {",
        "",
        "val WIDTH = 5",
        "val LEN = 2",
        "val SEQ_VALUE: Seq[UInt] = Seq(1.U(2.W), 2.U(2.W), 3.U(2.W))",
        "",
        "val INIT_VALUE: UInt = 0.U",
        "val SEQ_UINT: Seq[UInt]",
        "",
      "}"
    )
    
    result should containStr ("import test_p.WIDTH")

    result should containStr (
      "class my_module(",
          "val INIT_VALUE: UInt = 0.U,",
          "val SEQ_UINT: Seq[UInt],",
          "val LABELS: Int,",
          "val LABEL_SEQ: Seq[UInt],",
          "val LABEL_SEQ2: Seq[UInt],",
          "val TEST: Boolean",
        ") extends RawModule {",
        "val a = IO(Input(Bool()))",
        "val b = IO(Output(Bool()))",
        "val w = Wire(Bool())",
        "w := Mux(a === (SEQ_UINT(LABELS) =/= 0.U), a, false.B)",
        "b := Mux(TEST.B, w, false.B)"
    )
    
    result should containStr (
      "val inst = Module(new my_module(",
        "INIT_VALUE = 0.U,",
        "SEQ_UINT = test_p.SEQ_VALUE,",
        "TEST = false",
      "))",
      "inst.a := a",
      "b := inst.b"
    )
  }
}