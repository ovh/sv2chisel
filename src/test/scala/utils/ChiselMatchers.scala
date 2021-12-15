// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests.utils

import org.scalatest._
import matchers.should._
import collection.mutable.ArrayBuffer

trait ChiselMatchers extends Matchers {
  import matchers._
  /** Checks that the emitted circuit has the expected lines in order */
  def containLineSet(expectedLines: String*) = new ChiselStrictStringsMatcher(expectedLines)
  def containStr(expectedLines: String*) = new ChiselFlexStringsMatcher(expectedLines)

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

  class ChiselStrictStringsMatcher(expectedLines: Seq[String]) extends Matcher[String] {
    override def apply(chisel: String): MatchResult = {
      val data = chisel.split("\n")
      MatchResult(
        data.containsSlice(expectedLines),
        chisel + "\n did not contain \"" + expectedLines + "\"\n" +"\nDetails:\n" + findFaillingLine(data, expectedLines),
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
        chisel + "\n did not contain \"" + expectedLines + "\"\n" +"\nDetails:\n" + findFaillingLine(data, expectedLines),
        s"Emitted chisel contained $expectedLines"
      )
    }
  }
}