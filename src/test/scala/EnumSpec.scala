// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class EnumSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "Enum"
  
  it should " support generic enumeration" in {
    val result = emit(wrapInPackage(s"""
      |// comment
      |typedef enum logic [1:0] {
      |    STATE_A = 2'd0,
      |    STATE_B = 2'd1,
      |    STATE_C = 2'd2
      |} state_t;
      """.stripMargin
    ))
    debug(result)
    result should containStr ("import chisel3._")
    result should containStr ("import sv2chisel.helpers.enum._")
    
    result should containStr ("object state_t extends GenericHwEnum {")
    result should containStr ("val STATE_A = Value")
    result should containStr ("val STATE_B = Value")
    result should containStr ("val STATE_C = Value")
    
    // compression option to discuss below a given number of characters 
    // object state_t extends GenericHwEnum {
    //   val STATE_A, STATE_B, STATE_C = Value
    // }
    
  }
  
  it should " support verilog generic enumeration" in {
    val result = emit(wrapInPackage(s"""
      |// comment
      |typedef enum logic [1:0] {
      |    STATE_A,
      |    STATE_B,
      |    STATE_C
      |} state_t;
      """.stripMargin
    ))
    debug(result)
    result should containStr ("import chisel3._")
    result should containStr ("import sv2chisel.helpers.enum._")
    
    result should containStr ("object state_t extends GenericHwEnum {")
    result should containStr ("val STATE_A = Value")
    result should containStr ("val STATE_B = Value")
    result should containStr ("val STATE_C = Value")
    
    // compression option to discuss below a given number of characters 
    // object state_t extends GenericHwEnum {
    //   val STATE_A, STATE_B, STATE_C = Value
    // }
    
  }
  
  it should " support custom enumeration" in {
    val result = emit(wrapInPackage(s"""
      |// comment
      |typedef enum logic [1:0] {
      |    STATE_A = 2'd0,
      |    STATE_B = 2'd3,
      |    STATE_C = 2'd2
      |} state_t;
      """.stripMargin
    ))
    debug(result)
    result should containStr ("import chisel3._")
    result should containStr ("import sv2chisel.helpers.enum._")
    
    result should containStr ("object state_t extends CustomHwEnum {")
    result should containStr ("val STATE_A = V(0.U(2.W))")
    result should containStr ("val STATE_B = V(3.U(2.W))")
    result should containStr ("val STATE_C = V(2.U(2.W))")
    
  }
  
}