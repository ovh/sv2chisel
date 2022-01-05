// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselHelpersTests.utils

import org.scalatest._
import matchers.should._

import collection.mutable.ArrayBuffer

trait VerilogMatchers extends Matchers {
  import matchers._

  /** Checks that the emitted circuit has the expected lines in order */
  def containExactly(expectedLines: String*) = new VerilogStrictStringsMatcher(expectedLines)
  def containStr(expectedLines: String*)     = new VerilogFlexStringsMatcher(expectedLines)

  def findFaillingLine(data: Seq[String], expected: Seq[String]): String = {
    val msg = ArrayBuffer[String]()
    val starts = data.zipWithIndex.collect { case (s, t) if (s == expected.head) => t }
    if(starts.isEmpty) msg += s"[DEBUG] Unable to find a first matching line (${expected.head})"
    starts.foreach(i => {
      msg += s"DEBUG: starting at index $i"
      data.drop(i).zip(expected).foreach { 
        case (chisel, exp) if (chisel != exp ) => msg += s"[DEBUG]: failing at `$chisel` (expected: `$exp`)"
        case _ => 
      }
    })
    msg.mkString("\n")
  }

  class VerilogStrictStringsMatcher(expectedLines: Seq[String]) extends Matcher[String] {
    override def apply(verilog: String): MatchResult = {
      val data = verilog.split("\n").toSeq
      MatchResult(
          data.containsSlice(expectedLines),
          verilog + "\n did not contain \"" + expectedLines + "\"\n" +"\nDetails:\n" + findFaillingLine(data, expectedLines),
          s"Emitted verilog contained $expectedLines"
      )
    }
  }
  class VerilogFlexStringsMatcher(expectedLines: Seq[String]) extends Matcher[String] {
    override def apply(verilog: String): MatchResult = {
      val data = stripComments(verilog).split("\n").map(_.trim).toSeq
      MatchResult(
          data.containsSlice(expectedLines),
          verilog + "\n did not contain \"" + expectedLines + "\"\n" +"\nDetails:\n" + findFaillingLine(data, expectedLines),
          s"Emitted verilog contained $expectedLines"
      )
    }
  }
  def stripComments(str: String): String = {
    "//.*\n".r.replaceAllIn(str, "\n")
  }

}