// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselHelpersTests

import sv2chiselHelpersTests.utils._
import sv2chisel.helpers.bundleconvert._

import chisel3._
import chisel3.stage.ChiselStage

import org.scalatest._
import flatspec._

class bundleconvertSpec extends AnyFlatSpec with VerilogMatchers {
  val setTestRunDir = Array("--target-dir", "test_run_dir")

  behavior of "BundleSubRangeAccess" 
  
  it should "allow field extraction by range" in {
    class SubRangeBundle() extends RawModule {
      class MyBundle extends Bundle {
        val a = Bool() // index (7, 7)
        val b = Bool() // index (6, 6)
        val c = UInt(5.W) // index (5, 1)
        val d = Bool() // index (0,0)
      }
      val out = IO(Output(new MyBundle))
      out(0,0) := true.B
      out(5,1) := 4.U
      out(6,6) := true.B
      out(7,7) := false.B
    }
    val verilog = (new ChiselStage()).emitVerilog(new SubRangeBundle(), setTestRunDir)

    verilog should contains ("assign out_a = 1'h0;")
    verilog should contains ("assign out_b = 1'h1;")
    verilog should contains ("assign out_c = 5'h4;")
    verilog should contains ("assign out_d = 1'h1;")
  }

}
