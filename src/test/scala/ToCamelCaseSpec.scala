// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import sv2chisel.{TranslationOptions, ChiselizerOptions, TopLevelChiselGenerator}
import logger._

class ToCamelCaseSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  // Basic Tests of Module wraper for tests
  behavior of "ToCamelCase"
  
  it should "preserve emission names (with params => ParamWrapperGen)" in {
    val chiselizerOpts = ChiselizerOptions().copy(
      toCamelCase = true, 
      topLevelChiselGenerators = Seq(TopLevelChiselGenerator("main_module"))
    )
    val result = emit(s"""
      |module camel_case_inner #(
      |  parameter TEST_PARAM = 1
      |)(
      |  input a_in,
      |  output [31:0] b_out
      |);
      |  assign b_out = TEST_PARAM ? {32{a_in}} : '0;
      |endmodule
      |
      |module main_module #(
      |  parameter TEST_PARAM = 0
      |)(
      |  input clk,
      |  input my_input,
      |  output [31:0] my_output
      |);
      |  reg in_reg = '0;
      |  always @(posedge clk) begin
      |    in_reg <= my_input;
      |  end
      |  camel_case_inner #(.TEST_PARAM(TEST_PARAM)) inst_camel (in_reg, my_output);
      |endmodule
      """.stripMargin,
      options = TranslationOptions(chiselizer = chiselizerOpts)
    )
    debug(result)
    result should containStr (
      "class CamelCaseInner(",
          "val testParam: Boolean = true",
        ") extends RawModule {",
        "val aIn = IO(Input(Bool()))",
        "val bOut = IO(Output(UInt(32.W)))",
        "bOut := Mux(testParam.B, (VecInit.tabulate(32)(_ => aIn)).asUInt, 0.U)",
      "}"
    )
    result should containStr (
      "class MainModule(",
          "val testParam: Int = 0",
        ") extends Module {",
        "override def desiredName = \"main_module\"",
        "val myInput = IO(Input(Bool())).suggestName(\"my_input\")",
        "val myOutput = IO(Output(UInt(32.W))).suggestName(\"my_output\")",
        "val inReg = RegInit(Bool(), false.B)",
        "inReg := myInput",
        "val instCamel = Module(new CamelCaseInner(",
            "testParam = (testParam != 0)",
        "))",
        "instCamel.aIn := inReg",
        "myOutput := instCamel.bOut",
      "}"
    )
    result should containStr (
      "object MainModuleGen extends App {",
        "val gen = () => new MainModule(",
          "testParam = 0",
        ")",
        "val params = ParamSet(Seq(",
          "\"testParam\" -> IntParam(0)",
        "))",
        "ParamWrapperGenerator.emit(",
          "Map(params -> gen),",
          "renameWrapperPorts = Map(\"clock\" -> \"clk\"),",
          "forcePreset = true,",
          "unflatPorts = true,",
          "args = args",
        ")",
      "}"
    )
  }
  
  it should "preserve emission names (without params => VerilogPortWrapper)" in {
    val chiselizerOpts = ChiselizerOptions().copy(
      toCamelCase = true, 
      topLevelChiselGenerators = Seq(TopLevelChiselGenerator("main_module"))
    )
    val result = emit(s"""
      |module camel_case_inner #(
      |  parameter TEST_PARAM = 1
      |)(
      |  input a_in,
      |  output [31:0] b_out
      |);
      |  assign b_out = TEST_PARAM ? {32{a_in}} : '0;
      |endmodule
      |
      |module main_module (
      |  input clk,
      |  input my_input,
      |  output [31:0] my_output
      |);
      |  reg in_reg = '0;
      |  always @(posedge clk) begin
      |    in_reg <= my_input;
      |  end
      |  camel_case_inner #(.TEST_PARAM(0)) inst_camel (in_reg, my_output);
      |endmodule
      """.stripMargin,
      options = TranslationOptions(chiselizer = chiselizerOpts)
    )
    debug(result)
    result should containStr (
      "class CamelCaseInner(",
          "val testParam: Boolean = true",
        ") extends RawModule {",
        "val aIn = IO(Input(Bool()))",
        "val bOut = IO(Output(UInt(32.W)))",
        "bOut := Mux(testParam.B, (VecInit.tabulate(32)(_ => aIn)).asUInt, 0.U)",
      "}"
    )
    result should containStr (
      "class MainModule() extends Module {",
        "override def desiredName = \"main_module\"",
        "val myInput = IO(Input(Bool())).suggestName(\"my_input\")",
        "val myOutput = IO(Output(UInt(32.W))).suggestName(\"my_output\")",
        "val inReg = RegInit(Bool(), false.B)",
        "inReg := myInput",
        "val instCamel = Module(new CamelCaseInner(",
            "testParam = false",
        "))",
        "instCamel.aIn := inReg",
        "myOutput := instCamel.bOut",
      "}"
    )
    result should containStr (
      "object MainModuleGen extends App {",
        "VerilogPortWrapper.emit(",
          "() => new MainModule(),",
          "renameWrapperPorts = Map(\"clock\" -> \"clk\"),",
          "forcePreset = true,",
          "args = args",
        ")",
      "}"
    )
  }
  
  it should "not mess with unparametered blackboxes" in {

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
    val options = TranslationOptions(
      chiselizer = ChiselizerOptions(toCamelCase = true)
    )
    val result = emit(bb, main, None, options)
      
    result should containStr ( 
      "class MyBlackBox() extends BlackBox {",
        "override def desiredName = \"my_black_box\"",
        "",
        "val io = IO(new Bundle {",
          "val i_a = Input(UInt(8.W))",
          "val i_b = Input(UInt(8.W))",
          "val i_c = Input(Bool())",
          "val o = Output(UInt(8.W))",
          "val o_v = Output(UInt(8.W))",
        "})",
      "}"
    )
    
    result should containStr ( 
      "class Test() extends RawModule {",
        "",
        "val iA = IO(Input(UInt(8.W)))",
        "val iB = IO(Input(UInt(8.W)))",
        "val iC = IO(Input(Bool()))",
        "val o = IO(Output(UInt(8.W)))",
        "val oV = IO(Output(UInt(8.W)))",
        "",
        "val inst = Module(new MyBlackBox)",
        "inst.io.i_a := iA",
        "inst.io.i_b := iB",
        "inst.io.i_c := iC",
        "o := inst.io.o",
        "oV := inst.io.o_v",
        "",
      "}",
    )
  }
  
  it should "not mess with parametered blackboxes" in {

    val bb = """
          |module my_black_box#(
          |  parameter PARAM_TESTI = 1,
          |  parameter PARAM_TESTS = "TRUE",
          |  parameter PARAM_TEST
          |)(
          |  input in_a,
          |  output out_b
          |);
          |assign out_b = PARAM_TEST ? in_a : '0;
          |endmodule
        """.stripMargin
        
    val main = wrapInModule("""
        |input  in_a;
        |output out_b;
        |
        |my_black_box #(.PARAM_TEST(1)) inst (
        |  .in_a(in_a),
        |  .out_b(out_b)
        |);
        """.stripMargin
      )
        
    val options = TranslationOptions(
      chiselizer = ChiselizerOptions(toCamelCase = true)
    )
    val result = emit(bb, main, None, options)
      
    result should containStr ( 
      "class MyBlackBox(",
          "val paramTesti: Int = 1,",
          "val paramTests: String = \"TRUE\",",
          "val paramTest: Int",
        ") extends BlackBox(Map(",
              "\"PARAM_TESTI\" -> paramTesti,",
              "\"PARAM_TESTS\" -> paramTests,",
              "\"PARAM_TEST\" -> paramTest",
        ")) {",
        "override def desiredName = \"my_black_box\"",
        "val io = IO(new Bundle {",
          "val in_a = Input(Bool())",
          "val out_b = Output(Bool())",
        "})",
      "}",
    )
    
    result should containStr ( 
      "class Test() extends RawModule {",
        "",
        "val inA = IO(Input(Bool()))",
        "val outB = IO(Output(Bool()))",
        "",
        "val inst = Module(new MyBlackBox(",
            "paramTest = 1",
        "))",
        "inst.io.in_a := inA",
        "outB := inst.io.out_b",
        "",
      "}",
    )
  }
  
  it should "not mess with wrapped blackboxes" in {

    val bb = """
          |module my_black_box#(
          |  parameter PARAM_TESTI = 1,
          |  parameter PARAM_TESTS = "TRUE",
          |  parameter PARAM_TEST
          |)(
          |  input in_a,
          |  input clk,
          |  output out_b
          |);
          |assign out_b = PARAM_TEST ? in_a : '0;
          |endmodule
        """.stripMargin
        
    val main = wrapInModule("""
        |input  clk;
        |input  in_a;
        |output out_b;
        |
        |reg in_reg = '0;
        |always @(posedge clk) begin
        |  in_reg <= in_a;
        |end
        |
        |my_black_box #(.PARAM_TEST(1)) inst (
        |  .clk(clk),
        |  .in_a(in_reg),
        |  .out_b(out_b)
        |);
        """.stripMargin
      )
        
    val options = TranslationOptions(
      chiselizer = ChiselizerOptions(toCamelCase = true)
    )
    val result = emit(bb, main, None, options)
      
    result should containStr ( 
      "class MyBlackBox(",
          "val paramTesti: Int = 1,",
          "val paramTests: String = \"TRUE\",",
          "val paramTest: Int",
        ") extends RawModule {",
        "override def desiredName = \"MyBlackBoxWrapper\"",
        "val io = IO(new Bundle {",
          "val inA = Input(Bool())",
          "val clk = Input(Clock())",
          "val outB = Output(Bool())",
        "})",
        "val inst = Module(new MyBlackBoxBB(paramTesti, paramTests, paramTest))",
        "inst.io.in_a := io.inA",
        "inst.io.clk := io.clk.asTypeOf(inst.io.clk)",
        "io.outB := inst.io.out_b",
      "}",
    )
    
    result should containStr ( 
      "class MyBlackBoxBB(",
          "val paramTesti: Int = 1,",
          "val paramTests: String = \"TRUE\",",
          "val paramTest: Int",
        ") extends BlackBox(Map(",
              "\"PARAM_TESTI\" -> paramTesti,",
              "\"PARAM_TESTS\" -> paramTests,",
              "\"PARAM_TEST\" -> paramTest",
        ")) {",
        "override def desiredName = \"my_black_box\"",
        "val io = IO(new Bundle {",
          "val in_a = Input(Bool())",
          "val clk = Input(UInt(1.W))",
          "val out_b = Output(Bool())",
        "})",
      "}",
    )
    result should containStr ( 
      "class Test() extends Module {",
        "val inA = IO(Input(Bool()))",
        "val outB = IO(Output(Bool()))",
        "",
        "val inReg = RegInit(Bool(), false.B)",
        "inReg := inA",
        "",
        "val inst = Module(new MyBlackBox(",
        "paramTest = 1",
        "))",
        "inst.io.clk := clock",
        "inst.io.inA := inReg",
        "outB := inst.io.outB",
        "",
      "}",
    )
  }

}