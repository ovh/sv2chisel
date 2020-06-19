// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import sv2chisel._

import logger._

import scala.util.Random
import org.scalatest._

class LegalizeExpressionSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "LegalizeExpressionSpec" should "should be properly emitted" in {
    val result = emitInModule("""
      |localparam [0:0] B_TRUE = 1;
      |localparam [0:0] B_FALSE = 0;
      |localparam [0:0] C_UNKNOWN;
      |localparam integer INTP = (B_TRUE ? 5 : 4) + C_UNKNOWN*B_FALSE;
      |
      |wire a, b, c; 
      |assign c = |{B_TRUE && a, (B_TRUE || B_FALSE) && b, C_UNKNOWN && b};
      |
      |wire [31:0] w;
      |wire [31:0] z;
      |assign w[14:12] = 3'b000;
      |assign w[31:16] = $signed(w[10:0]);
      |assign w[15:0] = $signed(z[10:0]);
      |assign w = z*z+1;
      """.stripMargin
    )
    debug(result)
    result should contains ("class Test() extends MultiIOModule {")
    result should contains ("val B_TRUE: Boolean = true")
    result should contains ("val B_FALSE: Boolean = false")
    result should contains ("val C_UNKNOWN: Boolean")
    result should contains ("val INTP = (if(B_TRUE) 5 else 4)+(if(C_UNKNOWN) 1 else 0)*(if(B_FALSE) 1 else 0)")
    result should contains ("val a = Wire(Bool())")
    result should contains ("val b = Wire(Bool())")
    result should contains ("val c = Wire(Bool())")
    result should contains ("c := Concat(B_TRUE.B && a, (B_TRUE.B || B_FALSE.B) && b, C_UNKNOWN.B && b).orR()")
    
    result should contains ("val w = Wire(Vec(32, Bool()))")
    result should contains ("val z = Wire(UInt(32.W))")
    result should contains ("w(14,12) := b\"000\".U(3.W).asBools")
    /// padding (bit extension is done properly)
    result should contains ("w(31,16) := w(10,0).asTypeOf(SInt(16.W)).asBools") 
    result should contains ("w(15,0) := z(10,0).asTypeOf(SInt(16.W)).asBools") 
    result should contains ("w := (z*z+1.U).asTypeOf(Vec(32, Bool()))") 
  }

}