// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselHelpersTests

import sv2chiselHelpersTests.utils._
import sv2chisel.helpers.enum._

import chisel3._
import chisel3.stage.ChiselStage

import org.scalatest._
import flatspec._

class HwEnumSpec extends AnyFlatSpec with VerilogMatchers {
  val setTestRunDir = Array("--target-dir", "test_run_dir")

  "GenericHwEnum" should "just works" in {
    
    // warning: this type aliasing might hide bugs: confusing literal value (MyEnum.Value) with hardware type (UInt)
    // type MyEnum = UInt // GenericHwEnum are simple UInt but provide syntax convenience
    object MyEnum extends GenericHwEnum {
      val stateA = Value
      val stateB = Value
      val stateC = Value
    }
    
    class Example extends Module {
      val in       = IO(Input(Bool()))
      val out      = IO(Output(MyEnum()))

      val test = Wire(MyEnum())
      when(in){
        test := MyEnum.stateA
      } .otherwise {
        test := (MyEnum.maxId - 1).U // illustrate scala enumeration features  
      }
      
      // typical reason why providing MyEnum aliasing is not a good idea
      // def fun(en: MyEnum): MyEnum = en match {
      //   case MyEnum.stateA => MyEnum.stateB // fruitless test : UInt against UInt literal => but compiler cannot warn
      //   case MyEnum.stateB => MyEnum.stateA
      //   case _ => println("always true"); en 
      // }
      // software: dealing with scala literal values
      def funSw(en: MyEnum.Value): MyEnum.Value = en match {
        case MyEnum.stateA => MyEnum.stateB // match scala literal values for equivalence
        case MyEnum.stateB => MyEnum.stateA
        case _ => en 
      }
      // hardware: dealing with signal and hardware literal
      def funHw(en: UInt): UInt =  {
        val res = Wire(en.cloneType)
        when(en === MyEnum.stateA){ // implicit conversion to UInt => enabling to apply the equality in hardware
          res := MyEnum.stateB // implicit conversion to UInt
        } .elsewhen(en === MyEnum.stateB) {
          res := MyEnum.stateA
        } .otherwise {
          res := en
        }
        res
      }
      
      // when(fun(test) === fun(MyEnum.stateB)) { //quite risky for fun(test), basically doing nothing
      // NB: using suggestName to get predictible name for intermediate verilog wires
      when((funHw(test).suggestName("funHw") === funSw(MyEnum.stateB)).suggestName("bool")) {
        out := test
      } .otherwise {
        out := 0.U
      }
    }
    val verilog = (new ChiselStage()).emitVerilog(new Example(), setTestRunDir)
    verilog should containStr ("wire [1:0] test = in ? 2'h0 : 2'h2;")
    verilog should containStr ("wire  bool = funHw == 2'h0;")
    verilog should containStr ("assign out = bool ? test : 2'h0;")

  }
  
  "CustomHwEnum" should "just works" in {
    
    // warning: this type aliasing might hide bugs: confusing literal value (MyEnum.Value) with hardware type (UInt)
    // type MyEnum = UInt // GenericHwEnum are simple UInt but provide syntax convenience
    object MyEnum extends CustomHwEnum {
      val stateA = V(0.U)
      val stateB = V(12.U)
      val stateC = V(5.U)
    }
    
    class Example extends Module {
      val in       = IO(Input(Bool()))
      val out      = IO(Output(MyEnum()))

      val test = Wire(MyEnum())
      when(in){
        test := MyEnum.stateA
      } .otherwise {
        test := MyEnum.stateB
      }
      
      // typical reason why providing MyEnum aliasing is not a good idea
      // def fun(en: MyEnum): MyEnum = en match {
      //   case MyEnum.stateA => MyEnum.stateB // fruitless test : UInt against UInt literal => but compiler cannot warn
      //   case MyEnum.stateB => MyEnum.stateA
      //   case _ => println("always true"); en 
      // }
      // software: dealing with scala literal values
      def funSw(en: MyEnum.Value): MyEnum.Value = en match {
        case MyEnum.stateA => MyEnum.stateB // match scala literal values for equivalence
        case MyEnum.stateB => MyEnum.stateA
        case _ => en 
      }
      // hardware: dealing with signal and hardware literal
      def funHw(en: UInt): UInt =  {
        val res = Wire(en.cloneType)
        when(en === MyEnum.stateA){ // implicit conversion to UInt => enabling to apply the equality in hardware
          res := MyEnum.stateB // implicit conversion to UInt
        } .elsewhen(en === MyEnum.stateB) {
          res := MyEnum.stateA
        } .otherwise {
          res := en
        }
        res
      }
      
      // when(fun(test) === fun(MyEnum.stateB)) { //quite risky for fun(test), basically doing nothing
      // NB: using suggestName to get predictible name for intermediate verilog wires
      when((funHw(test).suggestName("funHw") === funSw(MyEnum.stateA)).suggestName("bool")) {
        out := test
      } .otherwise {
        out := 0.U
      }
    }
    val verilog = (new ChiselStage()).emitVerilog(new Example(), setTestRunDir)
    verilog should containStr ("wire [3:0] test = in ? 4'h0 : 4'hc;")
    verilog should containStr ("wire  bool = funHw == 4'hc;")
    verilog should containStr ("assign out = bool ? test : 4'h0;")

  }
  
}
