// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselHelpersTests.utils

import org.scalatest._
import matchers.should._

trait VerilogMatchers extends Matchers {
  import matchers._

  /** Checks that the emitted circuit has the expected lines in order */
  def containExactly(expectedLines: String*) = new VerilogStrictStringsMatcher(expectedLines)
  def contain(expectedLines: String*)     = new VerilogFlexStringsMatcher(expectedLines)

  class VerilogStrictStringsMatcher(expectedLines: Seq[String]) extends Matcher[String] {
    override def apply(verilog: String): MatchResult = {
      MatchResult(
          verilog.split("\n").containsSlice(expectedLines),
          verilog + "\n did not contain \"" + expectedLines + "\"",
          s"Emitted verilog contained $expectedLines"
      )
    }
  }
  class VerilogFlexStringsMatcher(expectedLines: Seq[String]) extends Matcher[String] {
    override def apply(verilog: String): MatchResult = {
      MatchResult(
          stripComments(verilog).split("\n").map(_.trim).containsSlice(expectedLines),
          verilog + "\n did not contain \"" + expectedLines + "\"",
          s"Emitted verilog contained $expectedLines"
      )
    }
  }
  def stripComments(str: String): String = {
    "//.*\n".r.replaceAllIn(str, "\n")
  }

}