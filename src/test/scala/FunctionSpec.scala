// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class FunctionSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "Function" 
  
  
  it should "just work with basic functions -- in body args" in {

    val result = emit(wrapInPackage("""
        |function logic simple_function;
        |    input in;
        |    simple_function = in;
        |endfunction

        """.stripMargin
      ))
      
    result should contain (  "def simple_function(in:Bool): Bool = {",
                                "in",
                              "}")

  }
  it should "just work with basic functions -- arg list" in {

    val result = emit(wrapInPackage("""
        |function logic simple_function(input logic in);
        |    simple_function = in;
        |endfunction

        """.stripMargin
      ))

    result should contain (  "def simple_function(in:Bool): Bool = {",
                                "in",
                              "}")

  }
  it should "just work with basic functions -- minimal (potentially illegal) arg list style" in {

    val result = emit(wrapInPackage("""
        |function logic simple_function(input in);
        |    simple_function = in;
        |endfunction

        """.stripMargin
      ))

    result should contain (  "def simple_function(in:Bool): Bool = {",
                                "in",
                              "}")

  }
  
  it should "just work with simple functions " in {

    val result = emit(wrapInPackage("""
        |function [7:0] sum;
        |    input [7:0] a;
        |    input [7:0] b;
        |    sum = a + b;
        |endfunction

        """.stripMargin
      ))
      
    result should contain (  "def sum(a:UInt, b:UInt): UInt = {",
                                "a+b",
                              "}")

  }
  
  it should "just work with simple function in usage " in {

    val pkt = wrapInPackage("""
        |function [7:0] sum;
        |    input [7:0] a;
        |    input [7:0] b;
        |    sum = a + b;
        |endfunction
        """.stripMargin, "test_p"
      )
         
    val import_p = """
        |import test_p::*;
        |""".stripMargin
        
    val module = wrapInModule("""
        |input  [7:0] i_a;
        |input  [7:0] i_b;
        |input        i_c;
        |output [7:0] o;
        |output [7:0] o_v;
        |
        |// meaningless assigns whose sole goal is to prevent UInt conversion 
        |assign i_a[2:0] = '0; 
        |assign o_v[2:0] = '0;
        |
        |assign o = sum(i_a, i_b);
        |assign o_v = sum(i_a, i_c);
        """.stripMargin
      )

    val verilog = pkt + import_p + module
    trace(verilog)
    val result = emit(verilog)
      
    result should contain ( "def sum(a:UInt, b:UInt): UInt = {",
                              "a+b",
                            "}")
    
    result should contain ( "val i_a = IO(Input(Vec(8, Bool())))")
    result should contain ( "val i_b = IO(Input(UInt(8.W)))")
    result should contain ( "val i_c = IO(Input(Bool()))")
    result should contain ( "val o = IO(Output(UInt(8.W)))")
    result should contain ( "val o_v = IO(Output(Vec(8, Bool())))")

    result should contain ( "i_a(2,0) := (0.U).asTypeOf(Vec(3, Bool()))")
    result should contain ( "o_v(2,0) := (0.U).asTypeOf(Vec(3, Bool()))")
    
    result should contain ( "o := sum(i_a.asUInt, i_b)")
    result should contain ( "o_v := sum(i_a.asUInt, i_c).asBools") // Bool is a subtype of UInt => no need for a cast

  }
}