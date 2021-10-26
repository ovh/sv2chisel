// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import sv2chisel._

import logger._

import scala.util.Random
import org.scalatest._

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
    result should contains ("import chisel3._")
    result should contains ("import sv2chisel.helpers.enum._")
    
    result should contains ("object state_t extends GenericHwEnum {")
    result should contains ("val STATE_A = Value")
    result should contains ("val STATE_B = Value")
    result should contains ("val STATE_C = Value")
    
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
    result should contains ("import chisel3._")
    result should contains ("import sv2chisel.helpers.enum._")
    
    result should contains ("object state_t extends CustomHwEnum {")
    result should contains ("val STATE_A = Val(0.U(2.W))")
    result should contains ("val STATE_B = Val(3.U(2.W))")
    result should contains ("val STATE_C = Val(2.U(2.W))")
    
  }
  
}