// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class ModuleSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  // Basic Tests of Module wraper for tests
  
  "Module" should "be properly emitted with wrapper" in {
    val result = emitInModule(s"""
      |wire a, b;
      |assign a = b;
      """.stripMargin
    )
    debug(result)
    result should contains ("class Test() extends MultiIOModule {")
    result should contains ("val a = Wire(Bool())")
    result should contains ("val b = Wire(Bool())")
    result should contains ("a := b")
    
  }
  it should "be properly emitted with named wrapper" in {
    val result = emitInModule("MyTest", s"""
      |wire a, b;
      |assign a = b;
      """.stripMargin
    )
    debug(result)
    result should contains ("class MyTest() extends MultiIOModule {")
    result should contains ("val a = Wire(Bool())")
    result should contains ("val b = Wire(Bool())")
    result should contains ("a := b")
    
  }
  
  // Test Inputs
  it should "be properly emitted with arg-style IOs" in {
    val result = emit(s"""
      |module CustomModule(
      |  input a,
      |  output b
      |);
      |assign b = a;
      |endmodule
      """.stripMargin
    )
    debug(result)
    result should contains ("class CustomModule() extends MultiIOModule {")
    result should contains ("val a = IO(Input(Bool()))")
    result should contains ("val b = IO(Output(Bool()))")
    result should contains ("b := a")
  }
  
  it should "be properly emitted with inline style IOs" in {
    val result = emit(s"""
      |module CustomModule(a,b);
      |  input a;
      |  output b;
      |
      |assign b = a;
      |endmodule
      """.stripMargin
    )
    debug(result)
    result should contains ("class CustomModule() extends MultiIOModule {")
    result should contains ("val a = IO(Input(Bool()))")
    result should contains ("val b = IO(Output(Bool()))")
    result should contains ("b := a")
  }
  
  // Test Params
  it should "be properly emitted with Params" in {
    val result = emit(s"""
      |module CustomModule#(
      |  parameter TEST
      |)(
      |  input a,
      |  output b
      |);
      |assign b = TEST ? a : '0;
      |endmodule
      """.stripMargin
    )
    debug(result)
    result should contains ("class CustomModule(" )
    result should contains ("val TEST: Int" )
    result should contains (") extends MultiIOModule {")
    result should contains ("val a = IO(Input(Bool()))")
    result should contains ("val b = IO(Output(Bool()))")
    result should contains ("b := Mux((TEST != 0).B, a, false.B)")
  }
  
  // Instances
  "Instances" should "be properly emitted with options" in {
    val result = emit(s"""
      |module mod#(
      |  parameter TEST = 1
      |)(
      |  input a,
      |  output b
      |);
      |assign b = TEST ? a : '0;
      |endmodule
      |
      |module Main#(
      |  parameter OPT
      |)(
      |  input i,
      |  output o
      |);
      |mod #(.TEST(OPT)) instA (.a(i),.b(o));
      |endmodule
      """.stripMargin
    )
    debug(result)
    result should contains ("class mod(" )
    result should contains ("val TEST: Int = 1" )
    
    result should contains ("class Main(" )
    result should contains ("val OPT: Int" )
    result should contains (") extends MultiIOModule {")
    result should contains ("val i = IO(Input(Bool()))")
    result should contains ("val o = IO(Output(Bool()))")

    result should contains ("val instA = Module(new mod(")
    result should contains ("TEST = OPT")
    result should contains ("))")
    result should contains ("instA.a := i")
    result should contains ("o := instA.b")
    
  }
  
  it should "be properly emitted without options" in {
    val result = emit(s"""
      |module mod#(
      |  parameter TEST = 1
      |)(
      |  input a,
      |  output b
      |);
      |assign b = TEST ? a : '0;
      |endmodule
      |
      |module Main#(
      |  parameter OPT
      |)(
      |  input i,
      |  output o
      |);
      |mod instB (.a(i),.b(o));
      |endmodule
      """.stripMargin
    )
    debug(result)
    result should contains ("class mod(" )
    result should contains ("class Main(" )

    result should contains ("val instB = Module(new mod)")
    result should contains ("instB.a := i")
    result should contains ("o := instB.b")
  }
  
  it should "be properly emitted without named port map" in {
    val result = emit(s"""
      |module mod#(
      |  parameter TEST = 1
      |)(
      |  input a,
      |  output [31:0] b
      |);
      |assign b[15:0] = TEST ? {16{a}} : '0;
      |assign b[31:16] = TEST ? {16{a}} : '1;
      |endmodule
      |
      |module Main#(
      |  parameter OPT
      |)(
      |  input i,
      |  output [31:0] o
      |);
      |mod instB (i, o);
      |endmodule
      """.stripMargin
    )
    debug(result)
    result should contains ("class mod(" )
    result should contains ("class Main(" )

    result should contains ("val instB = Module(new mod)")
    result should contains ("instB.a := i")
    result should contains ("o := instB.b.asUInt")
  }

}