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
  def wrapInModule(body: String, name : String = "Test") : String = {
    s"""
      |module $name();
      |""".stripMargin ++ 
        body.split("\n").mkString("    ", "\n    ", "") ++ """
      |endmodule
      |""".stripMargin
  }
  
  def emitInModule(body: String) : String = {
    val str = wrapInModule(body)
    debug(str)
    emit(str)
  }
  
  def emitInModule(name: String, body : String) : String = {
    val str = wrapInModule(body, name)
    debug(str)
    emit(str)
  }
}
