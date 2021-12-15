// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._
import sv2chisel.{TranslationOptions, ChiselizerOptions}

class BlackBoxSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "BlackBox" 
  
  
  it should "work with inline ports with resource" in {

    val bb = wrapInModule("""
        |input  [7:0] i_a;
        |input  [7:0] i_b;
        |input        i_c;
        |output [7:0] o;
        |output [7:0] o_v;
        |
        |assign o = sum(i_a, i_b);
        |assign o_v = sum(i_a, i_c);
        """.stripMargin,
        "my_black_box"
      )
        
    val main = wrapInModule("""
        |input  [7:0] i_a;
        |input  [7:0] i_b;
        |input        i_c;
        |output [7:0] o;
        |output [7:0] o_v;
        |
        |my_black_box inst(
        |  .i_a(i_a),
        |  .i_b(i_b),
        |  .i_c(i_c),
        |  .o(o),
        |  .o_v(o_v)
        |);
        """.stripMargin
      )
    val options = TranslationOptions().copy(
      chiselizer = ChiselizerOptions().copy(baseBlackboxRessourcePath = Some("./test_run_dir/resources/"))
    )
    val result = emit(bb, main, Some("src/main/resources/project/hdl/my_module.sv"), options)
      
    result should containStr ( "import chisel3.util.HasBlackBoxResource")
    result should containStr ( "class my_black_box() extends BlackBox with HasBlackBoxResource {")
    result should containStr ( 
      "val io = IO(new Bundle {",
        "val i_a = Input(UInt(8.W))",
        "val i_b = Input(UInt(8.W))",
        "val i_c = Input(Bool())",
        "val o = Output(UInt(8.W))",
        "val o_v = Output(UInt(8.W))",
      "})"
    )
    
    result should containStr ( 
      "val inst = Module(new my_black_box)",
      "inst.io.i_a := i_a",
      "inst.io.i_b := i_b",
      "inst.io.i_c := i_c",
      "o := inst.io.o",
      "o_v := inst.io.o_v"
    )
  }
  
  it should "work with inline ports without ressource" in {

    val bb = wrapInModule("""
        |input  [7:0] i_a;
        |input  [7:0] i_b;
        |input        i_c;
        |output [7:0] o;
        |output [7:0] o_v;
        |
        |assign o = sum(i_a, i_b);
        |assign o_v = sum(i_a, i_c);
        """.stripMargin,
        "my_black_box"
      )
        
    val main = wrapInModule("""
        |input  [7:0] i_a;
        |input  [7:0] i_b;
        |input        i_c;
        |output [7:0] o;
        |output [7:0] o_v;
        |
        |my_black_box inst(
        |  .i_a(i_a),
        |  .i_b(i_b),
        |  .i_c(i_c),
        |  .o(o),
        |  .o_v(o_v)
        |);
        """.stripMargin
      )

    val result = emit(bb, main, None, TranslationOptions())
      
    result shouldNot containStr ( "import chisel3.util.HasBlackBoxResource")
    result should containStr ( "class my_black_box() extends BlackBox {")
    result should containStr ( 
      "val io = IO(new Bundle {",
        "val i_a = Input(UInt(8.W))",
        "val i_b = Input(UInt(8.W))",
        "val i_c = Input(Bool())",
        "val o = Output(UInt(8.W))",
        "val o_v = Output(UInt(8.W))",
      "})"
    )
    
    result should containStr ( 
      "val inst = Module(new my_black_box)",
      "inst.io.i_a := i_a",
      "inst.io.i_b := i_b",
      "inst.io.i_c := i_c",
      "o := inst.io.o",
      "o_v := inst.io.o_v"
    )
  }
  
  it should "work with vec ports" in {

    val bb = wrapInModule("""
        |input  [3:0][7:0] i;
        |output [3:0][7:0] o;
        |
        |assign o = i;
        """.stripMargin,
        "my_black_box"
      )
        
    val main = wrapInModule("""
        |input  [3:0][7:0] i;
        |output [3:0][7:0] o;
        |
        |my_black_box inst(
        |  .i(i),
        |  .o(o)
        |);
        """.stripMargin
      )

    val result = emit(bb, main, None, TranslationOptions())
      
    result shouldNot containStr ( "import chisel3.util.HasBlackBoxResource")
    result should containStr ( "class my_black_box() extends RawModule {",
      "",
      "val io = IO(new Bundle {",
        "val i = Input(Vec(4, UInt(8.W)))",
        "val o = Output(Vec(4, UInt(8.W)))",
      "})"
    )
    
    result should containStr ( 
      "val inst = Module(new my_black_box)",
      "inst.io.i := i",
      "o := inst.io.o"
    )
    result should containStr ( "class my_black_boxBB() extends BlackBox {",
      "val io = IO(new Bundle {",
        "val i = Input(UInt((4*8).W))",
        "val o = Output(UInt((4*8).W))",
      "})"
    )
  }
  
  it should "work with parameters and ports functional-style" in {

    val bb = """
          |module my_black_box#(
          |  parameter TESTI = 1,
          |  parameter TESTS = "TRUE",
          |  parameter TEST
          |)(
          |  input a,
          |  output b
          |);
          |assign b = TEST ? a : '0;
          |endmodule
        """.stripMargin
        
    val main = wrapInModule("""
        |input  a;
        |output b;
        |
        |my_black_box #(.TEST(1)) inst (
        |  .a(a),
        |  .b(b)
        |);
        """.stripMargin
      )

    val result = emit(bb, main, None, TranslationOptions())
    val q = "\""
    result should containStr ( 
       "class my_black_box(",
           "val TESTI: Int = 1,",
          s"val TESTS: String = ${q}TRUE$q,",
           "val TEST: Int",
         ") extends BlackBox(Map(",
                     s"${q}TESTI$q -> TESTI,",
                     s"${q}TESTS$q -> TESTS,",
                     s"${q}TEST$q -> TEST",
         ")) {",
         "val io = IO(new Bundle {",
           "val a = Input(Bool())",
           "val b = Output(Bool())",
         "})",
       "}"
    )
    
    result should containStr ( 
      "val inst = Module(new my_black_box(",
          "TEST = 1",
      "))",
      "inst.io.a := a",
      "b := inst.io.b"
    )
  }
  
  it should "support explicit hardware parameters" in {
    val p = wrapInPackage(s"""
          |localparam WIDTH = 5;
          |localparam logic [WIDTH-1:0] INIT_VALUE = '0;
      """.stripMargin, "test_p"
    )
    val inner = """
          |
          |import test_p::WIDTH;
          |
          |module my_module #(
          |  parameter logic [WIDTH-1:0] INIT_VALUE = '0,
          |  parameter TEST
          |)(
          |  input a,
          |  output b
          |);
          |assign b = TEST ? a : '0;
          |endmodule
        """.stripMargin
        
    val main = wrapInModule("""
          |input  a;
          |input  b;
          |
          |my_module #(.INIT_VALUE('0), .TEST(0)) inst(
          |  .a(a),
          |  .b(b)
          |);
        """.stripMargin)
    
    val result = emit(inner, p + main, None, TranslationOptions())
    debug(result)
    result should containStr ("import chisel3._")
    
    result should containStr (
      "package object test_p {",
        "",
        "val WIDTH = 5",
        "val INIT_VALUE: UInt = 0.U",
        "",
      "}"
    )
    
    result should containStr ("import test_p.WIDTH")

    result should containStr (
      "class my_module(",
          "val INIT_VALUE: UInt = 0.U,", // probably not the best style but compilable
          "val TEST: Int",
        ") extends BlackBox(Map(",
                  "\"INIT_VALUE\" -> INIT_VALUE.litValue,", // thanks to litValue
                  "\"TEST\" -> TEST",
        ")) {",
        "val io = IO(new Bundle {",
          "val a = Input(Bool())",
          "val b = Output(Bool())",
        "})"
    )
    
    result should containStr (
      "val inst = Module(new my_module(",
          "INIT_VALUE = 0.U,",
          "TEST = 0",
      "))",
      "inst.io.a := a",
      "b := inst.io.b"
    )

  }
  
}