// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class AsUIntSubAccessSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "AsUIntSubAccess" 
  
  it should "add apply for asUInt() in subrange" in {
    val result = emitInModule("""
      |typedef struct packed {
      |  logic       bool;
      |  logic [7:0] uint;
      |} tpe_t;
      |
      |wire tpe_t a;
      |wire [3:0] b;
      |
      |assign b = a[3:0];
      """.stripMargin
    )
    debug(result)

    result shouldNot containStr ( "b := a.asUInt(3,0)" )
    result should containStr ( "b := a.asUInt.apply(3,0)" )
  }
  
  it should "add apply for asUInt() in subindex" in {
    val result = emitInModule("""
      |typedef struct packed {
      |  logic       bool;
      |  logic [7:0] uint;
      |} tpe_t;
      |
      |wire tpe_t a;
      |wire       b;
      |
      |assign b = a[3];
      """.stripMargin
    )
    debug(result)

    result shouldNot containStr ( "b := a.asUInt(3)" )
    result should containStr ( "b := a.asUInt.apply(3)" )
  }
  
  it should "add apply for literals in subindex" in {
    val result = emitInModule("""
      |localparam P = 5[2];
      """.stripMargin
    )
    debug(result)

    result should containStr ( "val P: Bool = 5.U.apply(2)" )
  }

}