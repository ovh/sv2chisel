// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel.helpers

import chisel3._

/** Package holding all implicit conversion for bundle range assignments
 *
 *  This package must be imported if subrange access to bundle are made.
 *  It is a very hacky way of accessing to bundle fields, inherited from inability of verilog to enumerate fields.
 *  Proper manual translation to chisel
 *
 *  In all bundleconvert package, we use Chisel3 convention: (high, low) slicing, high is included.
 *
 */
package object bundleconvert {
  implicit def bundleToSubRangeAccess[T <: Bundle](b: T): BundleSubRangeAccess[T] = new BundleSubRangeAccess(b)
  implicit def bundleFieldToData[T <: Bundle](f: BundleField[T]): Data = f.data
}

object BundleConvertUsageWarning {
  private var emitted = false
  def emit(s: String): Unit = {
    if (!emitted) {
      emitted = true
      println("[WARNING] Use of bundle subrange access (provided by sv2chisel.helpers.bundleconvert) " +
        "is highly discouraged! It is a very hacky way of accessing bundle fields. This hack is provided " + 
        "to enable flawless 1-1 translation from verilog which does not provide ability to enumerate struct fields. " +
        "This code should be refactored to leverage Chisel generation abilities, in this case Bundle.elements"
      )
      // To do ? add pointer to file:loc ? (using stacktrace) 
      println(s"[INFO] Recording only a single example: $s")
    }
  }
}

/** Buffer class for implicit BundleField usage
 *
 *  Internal use only
 */
class BundleSubRangeAccess[T <: Bundle](b: T) {
  def apply(high: Int, low: Int): BundleField[T] = {
    // here comes the funny part
    // elements are listed starting with last defined
    // the last element defined are also the lowest ranking bits, starting as index 0
    // NB: getWidth must work for this conversion to success
    var startingIndex = 0
    var index = 0
    val indexed = b.elements.toSeq.map { case (s, data) => {
      val low = startingIndex
      val w = data.getWidth
      val high = low + w - 1
      startingIndex = high + 1
      index = index + 1
      low -> BundleField(b, s, data, index-1, high, low)
    }}
    
    indexed.toMap.get(low) match {
      case Some(b) if(b.high) == high => 
        val msg = s"Extract field #${b.index} `${b.name}` with downto range [$high:$low] --> ${b.data}"
        BundleConvertUsageWarning.emit(msg)
        b
      case _ => throw new Exception(s"Cannot extract field with downto range [$high:$low] from bundle. Details of fields: $indexed")
    }
  }
}


case class BundleField[T <: Bundle](b: T, name: String, data: Data, index: Int, high: Int, low: Int) {
  // we need very kind connect for this meta field 
  def :=(d: Data): Unit = {
    d.widthOption match {
      case Some(i) if (i <= high-low + 1) => data := d.asTypeOf(data) // safe 
      case Some(i) => 
        println(s"Warning: losing ${high-low+1-i} bits of data from $d in its connection to field $name ($data) from bundle $b")
        data := d.asTypeOf(data) // Not safe 
      case _ => 
        println(s"Critical: unsafe connection of $d to field $name ($data) from bundle $b")
        data := d.asTypeOf(data)
    }
    
  }
}

