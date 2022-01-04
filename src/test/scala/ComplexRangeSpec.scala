// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

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
      |logic [7: 0] e_res;
      |logic [7: 0] f_res;
      |integer sel;
      |
      |assign a_res = a_vect[ 0 +: 8]; // == a_vect[ 7 : 0]
      |assign a_res = a_vect[15 -: 8]; // == a_vect[15 : 8]
      |assign b_res = b_vect[ 0 +: 8]; // == b_vect[0 : 7]
      |assign b_res = b_vect[15 -: 8]; // == b_vect[8 :15]
      |
      |// variable part-select with fixed width
      |assign d_res = dword[8*sel +: 8]; 
      |localparam W = 8;
      |assign e_res = dword[W*sel -: W]; 
      |localparam P = 7;
      |assign f_res = dword[W*P -: W]; 
      |
      """.stripMargin
    )
    debug(result)
    result should containStr ("class Test() extends RawModule {")
    result should containStr ("val a_vect = Wire(Vec(32, Bool()))")
    result should containStr ("val b_vect = Wire(Vec(32, Bool()))")
    result should containStr ("val a_res = Wire(UInt(8.W))")
    result should containStr ("val b_res = Wire(UInt(8.W))")
    result should containStr ("val dword = Wire(Vec(64, Bool()))")
    result should containStr ("val d_res = Wire(UInt(8.W))")
    result should containStr ("val sel = Wire(UInt(64.W))")
    result should containStr ("a_res := a_vect(7,0).asUInt // == a_vect[ 7 : 0]")
    result should containStr ("a_res := a_vect(15,8).asTypeOf(UInt(8.W)) // == a_vect[15 : 8]")
    result should containStr ("b_res := b_vect(7,0).asUInt // == b_vect[0 : 7]")
    result should containStr ("b_res := b_vect(15,8).asTypeOf(UInt(8.W)) // == b_vect[8 :15]")
    result should containStr ("d_res := dword((8.U*sel)+7.U,8.U*sel).asTypeOf(UInt(8.W))")
    result should containStr ("e_res := dword(W.U*sel,(W.U*sel)-(W-1).U).asTypeOf(UInt(8.W))")
    result should containStr ("f_res := dword(W*P,(W*P)-(W-1)).asTypeOf(UInt(8.W))")
    
  }

}