// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

import sv2chisel.TranslationOptions

class AddDontCareSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "AddDontCare" 
  
  
  it should "apply on incomplete named port map for modules" in {

    val sub = wrapInModule("""
        |input  [7:0] i;
        |output [7:0] o;
        |input [7:0] extra;
        |
        |assign o = i;
        """.stripMargin,
        "my_sub"
      )
        
    val main = wrapInModule("""
        |input  [7:0] i;
        |output [7:0] o;
        |
        |my_sub inst(
        |  .i(i),
        |  .o(o)
        |);
        """.stripMargin
      )

    val result = emit(sub + main, Some("raw"))
      
    result should contain ( "class my_sub() extends RawModule {")
    
    result should contain ( 
      "val inst = Module(new my_sub)",
      "inst.i := i",
      "o := inst.o",
      "inst.extra := DontCare"
    )
  }
  it should "apply on incomplete named port map for blackboxes" in {

    val bb = wrapInModule("""
        |input  [7:0] i;
        |output [7:0] o;
        |input [7:0] extra;
        |
        |assign o = i;
        """.stripMargin,
        "my_black_box"
      )
        
    val main = wrapInModule("""
        |input  [7:0] i;
        |output [7:0] o;
        |
        |my_black_box inst(
        |  .i(i),
        |  .o(o)
        |);
        """.stripMargin
      )

    val result = emit(bb, main, Some("raw"), TranslationOptions())
      
    result should contain ( "class my_black_box() extends BlackBox {")
    
    result should contain ( 
      "val inst = Module(new my_black_box)",
      "inst.io.i := i",
      "o := inst.io.o",
      "inst.io.extra := DontCare"
    )
  }
  
}