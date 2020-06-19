// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests.utils

import sv2chisel._
import org.scalatest._

import logger._

abstract class Sv2ChiselSpec extends FlatSpec with ChiselMatchers with EasyLogging {
  
  def emit(input: String): String = Driver.emitChisel(Project("test", input))
  // to do add optional param & ports 
  def emitInModule(body: String) = {
    val str = """
      |module Test();
      |""".stripMargin ++ 
        body.split("\n").mkString("    ", "\n    ", "") ++ """
      |endmodule
      |""".stripMargin
    debug(str)
    emit(str)
  }
}
