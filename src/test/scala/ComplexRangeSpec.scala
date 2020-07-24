// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import sv2chisel._

import logger._

import scala.util.Random
import org.scalatest._

class ComplexRangeSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "ComplexRangeSpec" should "be properly emitted" in {
    val result = emitInModule(s"""
      |logic [31: 0] a_vect;
      |logic [0 :31] b_vect;
      |logic [7 :0] a_res;
      |logic [0 :7] b_res;
      |
      |logic [63: 0] dword;
      |logic [7: 0] d_res;
      |integer sel;
      |
      |assign a_res = a_vect[ 0 +: 8]; // == a_vect[ 7 : 0]
      |assign a_res = a_vect[15 -: 8]; // == a_vect[15 : 8]
      |assign b_res = b_vect[ 0 +: 8]; // == b_vect[0 : 7]
      |assign b_res = b_vect[15 -: 8]; // == b_vect[8 :15]
      |
      |assign d_res = dword[8*sel +: 8]; // variable part-select with fixed width
      |
      """.stripMargin
    )
    debug(result)
    result should contains ("class Test() extends MultiIOModule {")
    result should contains ("val a_vect = Wire(Vec(32, Bool()))")
    result should contains ("val b_vect = Wire(Vec(32, Bool()))")
    result should contains ("val a_res = Wire(UInt(8.W))")
    result should contains ("val b_res = Wire(UInt(8.W))")
    result should contains ("val dword = Wire(Vec(64, Bool()))")
    result should contains ("val d_res = Wire(UInt(8.W))")
    result should contains ("val sel = Wire(UInt(64.W))")
    result should contains ("a_res := a_vect(7,0).asTypeOf(UInt(8.W)) // == a_vect[ 7 : 0]")
    result should contains ("a_res := a_vect(15,8).asTypeOf(UInt(8.W)) // == a_vect[15 : 8]")
    result should contains ("b_res := b_vect(7,0).asTypeOf(UInt(8.W)) // == b_vect[0 : 7]")
    result should contains ("b_res := b_vect(15,8).asTypeOf(UInt(8.W)) // == b_vect[8 :15]")
    result should contains ("d_res := dword(8.U*sel+7.U,8.U*sel).asTypeOf(UInt(8.W))")
    
  }

}