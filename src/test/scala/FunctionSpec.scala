// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import sv2chisel._

import logger._

import scala.util.Random
import org.scalatest._
import java.io.{PrintStream, ByteArrayOutputStream}

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
      
    result should contains (  "function simple_function(in:Bool): Bool = {",
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

    result should contains (  "function simple_function(in:Bool): Bool = {",
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

    result should contains (  "function simple_function(in:Bool): Bool = {",
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
      
    result should contains (  "function sum(a:UInt, b:UInt): UInt = {",
                                "a+b",
                              "}")

  }
}