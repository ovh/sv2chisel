// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import sv2chisel.{TranslationOptions, ChiselizerOptions}
import logger._

class UnpackedEmissionStyleSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  // Basic Tests of Module wraper for tests
  behavior of "UnpackedEmissionStyle"
  
  val baseContent = wrapInModule(
    s"""
      |//Verilog wire
      |wire [1:0] w[3:0];
      |assign w[3] = '0;
      |assign w[2] = '1;
      |assign w[1] = '1;
      |assign w[0] = '0;
      |// verilog register
      |input clk;
      |reg [1:0] r[3:0];
      |always @(posedge clk) begin
      |  r[3] <= '0;
      |  r[2] <= '1;
      |  r[1] <= '1;
      |  r[0] <= '0;
      |end
    """.stripMargin
  ) 

  it should "support unpacked wire & reg with UnpackedEmissionStyle.Reg" in {
    val chiselizerOpts = ChiselizerOptions().copy(
      unpackedEmissionStyle = ChiselizerOptions.UnpackedEmissionStyle.Reg
    )
    val result = emit(baseContent,
      options = TranslationOptions(chiselizer = chiselizerOpts)
    )
    debug(result)
    result should containStr (
      "val w = Wire(Vec(4, UInt(2.W)))",
      "w(3) := 0.U",
      "w(2) := 3.U",
      "w(1) := 3.U",
      "w(0) := 0.U"
      )
    result should containStr (
      "val r = Reg(Vec(4, UInt(2.W)))",
      "r(3) := 0.U",
      "r(2) := 3.U",
      "r(1) := 3.U",
      "r(0) := 0.U",
    )
  }
  
  it should "support unpacked wire & reg with UnpackedEmissionStyle.Mem" in {
    val chiselizerOpts = ChiselizerOptions().copy(
      unpackedEmissionStyle = ChiselizerOptions.UnpackedEmissionStyle.Mem
    )
    val result = emit(baseContent,
      options = TranslationOptions(chiselizer = chiselizerOpts)
    )
    debug(result)
    result should containStr (
      "val w = Wire(Vec(4, UInt(2.W)))",
      "w(3) := 0.U",
      "w(2) := 3.U",
      "w(1) := 3.U",
      "w(0) := 0.U"
      )
    result should containStr (
      "val r = Mem(4,UInt(2.W))",
      "r(3) := 0.U",
      "r(2) := 3.U",
      "r(1) := 3.U",
      "r(0) := 0.U",
    )
  }
}