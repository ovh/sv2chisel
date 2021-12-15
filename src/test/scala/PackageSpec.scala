// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class PackageSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "Package" 
  
  it should "just work with localparam " in {

    val result = emit(wrapInPackage("""
        |localparam test = 0;
        |localparam [0:0] A = 1;
        """.stripMargin, 
        "my_package"
      ))

    result should containStr ("package object my_package {")
    result should containStr ("val test = 0")
    result should containStr ("val A: Boolean = true")
    result should containStr ("}")

  }
  
  it should "just work with type alias " in {

    val result = emit(wrapInPackage("""
        |localparam TEST = 5;
        |typedef logic[TEST-1:0] mynewtype_t;
        """.stripMargin, 
        "my_package"
      ))

    result should containStr (  "object mynewtype_t {",
                                "def apply() = Vec(TEST, Bool())",
                              "}")

  }

}