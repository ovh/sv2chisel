// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselHelpersTests

import sv2chiselHelpersTests.utils._
import sv2chisel.helpers.MemInit

import chisel3._
import chisel3.stage.ChiselStage

import org.scalatest._
import flatspec._

class MemInitSpec extends AnyFlatSpec with VerilogMatchers {
  val setTestRunDir = Array("--target-dir", "test_run_dir")

  behavior of "MemInit"
  
  def checkNoReg(verilog: String): Unit = {
    verilog should containStr("reg [4:0] rom [0:3];")
    verilog should containStr("assign rom_out_MPORT_data = rom[rom_out_MPORT_addr];")
    verilog should containStr("assign out = rom_out_MPORT_data;")
    verilog should containStr (
        "rom[0] = 0;",
        "rom[1] = 1;",
        "rom[2] = 3;",
        "rom[3] = 7;",
      "end"
    )
  }
    

  it should "work with VecInit style apply" in {
    
    class Example() extends Module {
      val sel = IO(Input(UInt(2.W)))
      val out = IO(Output(UInt(5.W)))
      val rom = MemInit(0.U(5.W), 1.U(5.W), 3.U(5.W), 7.U(5.W))
      out := rom(sel) 
    }

    val verilog = (new ChiselStage()).emitVerilog(new Example(), setTestRunDir)
    checkNoReg(verilog)
  }
  
  it should "work with VecInit style apply (reg)" in {
    
    class Example() extends Module {
      val sel = IO(Input(UInt(2.W)))
      val en = IO(Input(Bool()))
      val out = IO(Output(UInt(5.W)))
      val outR = Reg(UInt(5.W))
      val rom = MemInit(0.U(5.W), 1.U(5.W), 3.U(5.W), 7.U(5.W))
      outR := rom(sel)
      out := outR
    }

    val verilog = (new ChiselStage()).emitVerilog(new Example(), setTestRunDir)
    verilog should containStr("reg [4:0] rom [0:3];")
    verilog should containStr("assign out = outR;")
    verilog should containStr("outR <= rom_outR_MPORT_data;")
    verilog should containStr (
        "rom[0] = 0;",
        "rom[1] = 1;",
        "rom[2] = 3;",
        "rom[3] = 7;",
      "end"
    )
  }
  
  it should "work with VecInit style apply (seq)" in {
    
    class Example() extends Module {
      val sel = IO(Input(UInt(2.W)))
      val out = IO(Output(UInt(5.W)))
      val rom = MemInit(Seq(0.U(5.W), 1.U(5.W), 3.U(5.W), 7.U(5.W)))
      out := rom(sel) 
    }

    val verilog = (new ChiselStage()).emitVerilog(new Example(), setTestRunDir)
    checkNoReg(verilog)
  }
  
  it should "work with VecInit style apply (Vec.Lit)" in {
    // hang in scala 2.13... ?
    class Example() extends Module {
      val sel = IO(Input(UInt(2.W)))
      val out = IO(Output(UInt(5.W)))
      import chisel3.experimental.VecLiterals._
      val rom = MemInit(Vec.Lit(0.U(5.W), 1.U(5.W), 3.U(5.W), 7.U(5.W)))
      out := rom(sel) 
    }

    val verilog = (new ChiselStage()).emitVerilog(new Example(), setTestRunDir)
    checkNoReg(verilog)
  }
  
  it should "work with basic apply" in {
    
    class Example() extends Module {
      val sel = IO(Input(UInt(2.W)))
      val out = IO(Output(UInt(5.W)))

      val rom = MemInit(4, UInt(5.W), Seq(0,1,3,7).map(BigInt(_)))
      out := rom(sel)
    }

    val verilog = (new ChiselStage()).emitVerilog(new Example(), setTestRunDir)
    checkNoReg(verilog)
  }
  
  it should "work with fill" in {
    
    class Example() extends Module {
      val sel = IO(Input(UInt(2.W)))
      val out = IO(Output(UInt(5.W)))

      val rom = MemInit.fill(4, UInt(5.W))(BigInt(0))
      out := rom(sel)
    }

    val verilog = (new ChiselStage()).emitVerilog(new Example(), setTestRunDir)
    verilog should containStr("reg [4:0] rom [0:3];")
    verilog should containStr("assign rom_out_MPORT_data = rom[rom_out_MPORT_addr];")
    verilog should containStr("assign out = rom_out_MPORT_data;")
    verilog should containStr (
      "for (initvar = 0; initvar < 4; initvar = initvar+1)",
        "rom[initvar] = 0;"
    )
  }
  
  it should "work with tabulate" in {
    
    class Example() extends Module {
      val sel = IO(Input(UInt(2.W)))
      val out = IO(Output(UInt(5.W)))

      val rom = MemInit.tabulate(4, UInt(5.W))(i => (BigInt(1) << i) - 1)
      out := rom(sel)
    }

    val verilog = (new ChiselStage()).emitVerilog(new Example(), setTestRunDir)
    checkNoReg(verilog)
  }
  
}
