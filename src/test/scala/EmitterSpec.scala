// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._
import java.io.{PrintStream, ByteArrayOutputStream}

class EmitterSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "Emitter"
  
  def checkOneLine(result: String) = {
    result should containStr ("import chisel3._")
    result should containLineSet (
      "// first intro comment",
      "",
      "// intro comment",
      "package object test_p { // this is package test_p"
    )
    result should containLineSet (
      "package object test_p { // this is package test_p",
      "  // comment1 prior A",
      "  // comment2 prior A",
      "  // comment3 prior A",
      "  val A = 2 // comment after A"
    )
    result should containLineSet (
      "  val A = 2 // comment after A",
      "",
      "",
      "  // comment just before B",
      "  val B = 2"
    )
    result should containLineSet (
      "  val B = 2",
      "",
      "  // comment one line before C",
      "",
      "  val C = 2"
    )
    result should containLineSet (
      "  val C = 2",
      "",
      "",
      "",
      "  val WWW = 16 // comment after WWW"
    )
    result should containLineSet (
      "  val WWW = 16 // comment after WWW",
      "// end of package is coming",
      "} // test_p",
      "// end of file"
    )
  }
  
  it should " support one-line comments (with proper original indent)" in {
    val result =emit(s"""
      |
      |// first intro comment
      |
      |// intro comment
      |package test_p; // this is package test_p
      |    // comment1 prior A
      |    // comment2 prior A
      |    // comment3 prior A
      |    localparam A = 2; // comment after A
      |
      |
      |    // comment just before B
      |    localparam B = 2;
      |
      |    // comment one line before C
      |
      |    localparam C = 2;
      |    
      |    
      |    
      |    localparam WWW = 16; // comment after WWW
      |    // end of package is coming
      |endpackage  // test_p
      |// end of file
      """.stripMargin
    )
    checkOneLine(result)
  }
  
  it should " support one-line comments (with unexpected original indent)" in {
    val result =emit(s"""
      |
      |// first intro comment
      |
      |// intro comment
      |package test_p; // this is package test_p
      |// comment1 prior A
      |// comment2 prior A
      |// comment3 prior A
      |localparam A = 2; // comment after A
      |
      |
      |// comment just before B
      |localparam B = 2;
      |
      |// comment one line before C
      |
      |localparam C = 2;
      |
      |
      |
      |localparam WWW = 16; // comment after WWW
      |// end of package is coming
      |endpackage  // test_p
      |// end of file
      """.stripMargin
    )
    checkOneLine(result)
  }
  
  def checkMultiLine(result: String) = {
    result should containStr ("import chisel3._")
    result should containLineSet (
      "/* first intro comment",
      " *",
      " * intro comment",
      " */",
      "package object test_p { /* this is package test_p */"
    )
    result should containLineSet (
      "package object test_p { /* this is package test_p */",
      "  /* comment1 prior A",
      "   * comment2 prior A",
      "   * comment3 prior A",
      "   */",
      "  val A = 2 /* comment after A */"
    )
    result should containLineSet (
      "  val A = 2 /* comment after A */",
      "",
      "",
      "  /* comment just before B */",
      "  val B = 2"
    )
    result should containLineSet (
      "  val B = 2",
      "",
      "  /* comment one line before C */",
      "",
      "  val C = 2"
    )
    result should containLineSet (
      "  val C = 2",
      "",
      "",
      "",
      "  val WWW = 16 /* comment after WWW"
    )
    result should containLineSet (
      "  val WWW = 16 /* comment after WWW",
      "     end of package is coming",
      " */",
      "} // test_p",
      "/* end of file comment",
      " * on multiple lines",
      " */"
    )
  }
  
  it should " support multi-line comments (with proper original indent)" in {
    val result =emit(s"""
      |
      |/* first intro comment
      | *
      | * intro comment
      | */
      |package test_p; /* this is package test_p */
      |    /* comment1 prior A
      |     * comment2 prior A
      |     * comment3 prior A
      |     */
      |    localparam A = 2; /* comment after A */
      |
      |
      |    /* comment just before B */
      |    localparam B = 2;
      |
      |    /* comment one line before C */
      |
      |    localparam C = 2;
      |    
      |    
      |    
      |    localparam WWW = 16; /* comment after WWW
      |     end of package is coming
      | */
      |endpackage  // test_p
      |/* end of file comment
      | * on multiple lines
      | */
      """.stripMargin
    )
    
    checkMultiLine(result)
  }
  
  it should " support multi-line comments (with unexpected original indent)" in {
    val result =emit(s"""
      |
      |/* first intro comment
      | *
      | * intro comment
      | */
      |package test_p; /* this is package test_p */
      |/* comment1 prior A
      | * comment2 prior A
      | * comment3 prior A
      | */
      |localparam A = 2; /* comment after A */
      |
      |
      |/* comment just before B */
      |localparam B = 2;
      |
      |/* comment one line before C */
      |
      |localparam C = 2;
      |
      |
      |
      |localparam WWW = 16; /* comment after WWW
      |     end of package is coming
      | */
      |endpackage  // test_p
      |/* end of file comment
      | * on multiple lines
      | */
      """.stripMargin
    )
    
    checkMultiLine(result)
  }
  
  it should " not mess with reversed values" in {
    val result =emit(s"""
      |
      |/* first intro comment
      | *
      | * intro comment
      | */
      |package test_p; /* this is package test_p */
      |/* comment1 prior A
      | * comment2 prior A
      | * comment3 prior A
      | */
      |localparam A = 2; /* comment after A */
      |
      |// random comment
      |localparam integer MANY_VALS [47:0] = '{4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 64, 32, 16, 8, 4, 2, 1};
      |// SINGLE COMMENT
      |localparam SIMPLE_VAL = 4096;
      |
      |endpackage  // test_p
      |/* end of file comment
      | * on multiple lines
      | */
      """.stripMargin
    )
    
    result should containStr ("import chisel3._")
    result should containLineSet (
      "/* first intro comment",
      " *",
      " * intro comment",
      " */",
      "package object test_p { /* this is package test_p */"
    )
    result should containLineSet (
      "package object test_p { /* this is package test_p */",
      "  /* comment1 prior A",
      "   * comment2 prior A",
      "   * comment3 prior A",
      "   */",
      "  val A = 2 /* comment after A */"
    )
    result should containLineSet (
      "  val A = 2 /* comment after A */",
      "",
      "  // random comment",
      "  val MANY_VALS: Seq[Int] = Seq(1, 2, 4, 8, 16, 32, 64, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096, 4096)",
      "  // SINGLE COMMENT",
      "  val SIMPLE_VAL = 4096"
    )

    result should containLineSet (
      "} // test_p",
      "/* end of file comment",
      " * on multiple lines",
      " */"
    )
  }
  it should " not mess without comments" in {
    val result =emit(s"""package test_p;
      |localparam A = 2;
      |endpackage""".stripMargin
    )
    
    result should containStr ("import chisel3._")
    result should containLineSet (
      "package object test_p {",
      "  val A = 2",
      "}"
    )
  }
  
  it should " work in more complex situations" in {
    val result = emitInModule("""
      |localparam a, b, c; // my param
      |wire [31:0] res; // my wire
      |
      |// starting big block
      |generate // generate
      |  if (!a || b) // "if"
      |    assign res = '0; // zero
      |  else if (c) // "else if"
      |    assign res = 1; // one
      |  else // "else"
      |    assign res = 2; // two
      |endgenerate // end
      """.stripMargin
    )
    
    result should containStr ("import chisel3._")
    result should containLineSet (
      "  val a: Boolean",
      "  val b: Boolean",
      "  val c: Boolean // my param",
      "  val res = Wire(UInt(32.W))  // my wire",
      "",
      "  // starting big block  // generate",
      "  if(( !a) || b) { // \"if\"",
      "    res := 0.U // zero",
      "  } else if(c) { // \"else if\"",
      "    res := 1.U // one",
      "  } else { // \"else\"",
      "    res := 2.U", // BUG: the comment two should be here... 
      "  } // two",
      "// end"
    )
  }
  
  
  it should "warn about preprocessor directives" in {

    
    val out = new ByteArrayOutputStream()
    Logger.setOutput(new PrintStream(out))
    val result = emitInModule("""
      |`ifndef USE_REG
      |wire [31:0] res;
      |`else
      |reg [31:0] res;
      |`endif
      """.stripMargin
    )
    
    val stdout = out.toString
    Logger.reset()
    stdout should containStr ( "[critical] Ignoring preprocessor directive: '`ifndef USE_REG' at sv2chiselTests.EmitterSpec:4:4" )
    stdout should containStr ( "[critical] Ignoring preprocessor directive: '`else' at sv2chiselTests.EmitterSpec:6:4" )
    stdout should containStr ( "[critical] Ignoring preprocessor directive: '`endif' at sv2chiselTests.EmitterSpec:8:4" )
    
    result should containStr ("class Test() extends RawModule {") // no clock
    result should containLineSet (
      "  val res = Wire(UInt(32.W)) ",
      "  val res = Wire(UInt(32.W)) ",
    )

  }
}