// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests.utils

import sv2chisel._
import org.scalatest._
import flatspec._

import logger._

abstract class Sv2ChiselSpec extends AnyFlatSpec with ChiselMatchers with EasyLogging {
  def defaultPath(path: Option[String]): Some[String] = path match {
    case Some(p) => Some(p)
    case None => Some(this.getClass.getName)
  }
  
  def emit(input: String, path: Option[String] = None, options: TranslationOptions = TranslationOptions()): String = 
    Driver.emitChisel(Project("test", input, defaultPath(path)), options, noFileIO = true)
    
  def emit(blackboxes: String, main: String, path: Option[String], options: TranslationOptions): String = 
    Driver.emitChisel(Project("test", blackboxes, main, defaultPath(path)), options, noFileIO = true)
  
  // to do add optional param & ports 
  def wrapInModule(body: String, name : String = "Test") : String = {
    s"""
      |module $name();
      |""".stripMargin ++ 
        body.split("\n").mkString("    ", "\n    ", "") ++ """
      |endmodule
      |""".stripMargin
  }
  
  def wrapInPackage(body: String, name : String = "Test") : String = {
    s"""
      |package $name;
      |""".stripMargin ++ 
        body.split("\n").mkString("    ", "\n    ", "") ++ """
      |endpackage
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
