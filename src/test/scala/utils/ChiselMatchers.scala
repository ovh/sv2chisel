// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests.utils

import org.scalatest._
import scala.util.matching.Regex

trait ChiselMatchers extends Matchers {
  import matchers._
  /** Checks that the emitted circuit has the expected lines in order */
  def containLines(expectedLines: String*) = new ChiselStrictStringsMatcher(expectedLines)
  def contains(expectedLines: String*) = new ChiselFlexStringsMatcher(expectedLines)

  class ChiselStrictStringsMatcher(expectedLines: Seq[String]) extends Matcher[String] {
    override def apply(verilog: String): MatchResult = {
      MatchResult(
        verilog.split("\n").containsSlice(expectedLines),
        verilog + "\n did not contain \"" + expectedLines + "\"",
        s"Emitted verilog contained $expectedLines"
      )
    }
  }
  class ChiselFlexStringsMatcher(expectedLines: Seq[String]) extends Matcher[String] {
    override def apply(verilog: String): MatchResult = {
      MatchResult(
        verilog.split("\n").map(_.trim).containsSlice(expectedLines),
        verilog + "\n did not contain \"" + expectedLines + "\"",
        s"Emitted verilog contained $expectedLines"
      )
    }
  }
}