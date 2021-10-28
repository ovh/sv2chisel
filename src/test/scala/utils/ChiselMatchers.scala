// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests.utils

import org.scalatest._
import matchers.should._

trait ChiselMatchers extends Matchers {
  import matchers._
  /** Checks that the emitted circuit has the expected lines in order */
  def containLineSet(expectedLines: String*) = new ChiselStrictStringsMatcher(expectedLines)
  def contain(expectedLines: String*) = new ChiselFlexStringsMatcher(expectedLines)

  class ChiselStrictStringsMatcher(expectedLines: Seq[String]) extends Matcher[String] {
    override def apply(chisel: String): MatchResult = {
      MatchResult(
        chisel.split("\n").containsSlice(expectedLines),
        chisel + "\n did not contain \"" + expectedLines + "\"",
        s"Emitted chisel contained $expectedLines"
      )
    }
  }
  class ChiselFlexStringsMatcher(expectedLines: Seq[String]) extends Matcher[String] {
    override def apply(chisel: String): MatchResult = {
      // remove leading & trailling spaces + console color tags 
      val data = chisel.split("\n").map(_.trim.replaceAll("\\P{Print}\\[[0-9]+m", ""))
      MatchResult(
        data.containsSlice(expectedLines),
        chisel + "\n did not contain \"" + expectedLines + "\"",
        s"Emitted chisel contained $expectedLines"
      )
    }
  }
}