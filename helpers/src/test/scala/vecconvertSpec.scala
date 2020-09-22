// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselHelpersTests

import sv2chiselHelpersTests.utils._
import sv2chisel.helpers._
import sv2chisel.helpers.vecconvert._

import chisel3._
import chisel3.stage.ChiselStage

import org.scalatest._

class vecconvertSpec extends FlatSpec with VerilogMatchers {
  val setTestRunDir = Array("--target-dir", "test_run_dir")

  "Ascii literals" should "just works" in {
    class VecInitTest() extends RawModule {
      val outA = IO(Output(Vec(8, UInt(8.W))))
      val outB = IO(Output(Vec(8, UInt(8.W))))
      val outC = IO(Output(UInt(64.W)))

      outA := "testtest".V
      outB := "test".V(8.W)
      outC := "test".V.asUInt
    }
    val verilog = (new ChiselStage()).emitVerilog(new VecInitTest(), setTestRunDir)
    verilog should contains("assign outA_0 = 8'h74;")
    verilog should contains("assign outA_1 = 8'h65;")
    verilog should contains("assign outA_2 = 8'h73;")
    verilog should contains("assign outA_3 = 8'h74;")
    verilog should contains("assign outA_4 = 8'h74;")
    verilog should contains("assign outA_5 = 8'h65;")
    verilog should contains("assign outA_6 = 8'h73;")
    verilog should contains("assign outA_7 = 8'h74;")

    verilog should contains("assign outB_0 = 8'h0;")
    verilog should contains("assign outB_1 = 8'h0;")
    verilog should contains("assign outB_2 = 8'h0;")
    verilog should contains("assign outB_3 = 8'h0;")
    verilog should contains("assign outB_4 = 8'h74;")
    verilog should contains("assign outB_5 = 8'h65;")
    verilog should contains("assign outB_6 = 8'h73;")
    verilog should contains("assign outB_7 = 8'h74;")

    verilog should contains("assign outC = 64'h74736574;") // NB: reversed as expected
  }

  "Implicit subwords" should "enable direct subrange assignments" in {
    class TestVecSubWords() extends RawModule {
      val outB = IO(Output(Vec(5, Bool())))
      // enabled by default
      outB(0) := false.B
      // possible thanks to new implicits
      outB(2, 1) := VecInit(true.B, true.B)
      outB(4, 3) := Seq(false.B, false.B)
    }
    val verilog = (new ChiselStage()).emitVerilog(new TestVecSubWords(), setTestRunDir)
    verilog should contains("assign outB_0 = 1'h0;")
    verilog should contains("assign outB_1 = 1'h1;")
    verilog should contains("assign outB_2 = 1'h1;")
    verilog should contains("assign outB_3 = 1'h0;")
    verilog should contains("assign outB_4 = 1'h0;")
  }
  it should "enable direct subrange slicing and use in arithmetic expression" in {
    class TestVecSubWords() extends RawModule {
      val in   = IO(Input(Vec(5, Bool())))
      val outA = IO(Output(UInt(2.W)))
      val outB = IO(Output(UInt(3.W)))
      val outC = IO(Output(Bool()))

      outA := in(2, 1).asUInt
      outB := 3.U + in(4, 3).asUInt
      outC := 1.U === in(4, 3).asUInt

      // let's do some checks about MSB // Vec2Int conversions
      val checkA = IO(Output(Bool()))
      val checkB = IO(Output(Bool()))
      val checkC = IO(Output(Bool()))
      checkA := VecInit(Seq(false.B, false.B, true.B)).asUInt === 4.U
      checkB := (4.U)(1, 0) === 0.U
      val bools = (4.U).asBools
      checkC := bools(0) === false.B && bools(1) === false.B && bools(2) === true.B

    }
    val verilog = (new ChiselStage()).emitVerilog(new TestVecSubWords(), setTestRunDir)
    verilog should contains("assign outA = {in_2,in_1};")
    verilog should contains("assign checkA = 1'h1;")
    verilog should contains("assign checkB = 1'h1;")
    verilog should contains("assign checkC = 1'h1;")
    // NB : outB cannot be checked without involving intermediate wires whose names could change
  }

  "TestBitPattern" should "properly assign with zeroes and ones" in {
    class TestBitPattern() extends RawModule {
      val outA = IO(Output(UInt(8.W)))
      val outB = IO(Output(UInt(8.W)))
  
      outA := Zeroes
      outB := Ones
    }
    val verilog = (new ChiselStage()).emitVerilog(new TestBitPattern(), setTestRunDir)
    verilog should contains("assign outA = 8'h0;")
    verilog should contains("assign outB = 8'hff;")
  }
}
