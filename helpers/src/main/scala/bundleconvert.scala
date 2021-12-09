// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel.helpers

import chisel3._
import chisel3.util.Cat

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
  implicit def bundleToSubRangeAccess[T <: Record](b: T): RecordSubRangeAccess[T] = new RecordSubRangeAccess(b)
  implicit def bundleFieldToData[T <: Record](f: RecordField[T]): Data = f.data
  implicit def selectedRecordFieldToUInt[T <: Record](f: SelectedRecordFields[T]): UInt = f.asUInt
}

object RecordConvertUsageWarning {
  private var emitted = false
  def emit(s: String): Unit = {
    if (!emitted) {
      emitted = true
      println("[WARNING] Use of bundle subrange access (provided by sv2chisel.helpers.bundleconvert) " +
        "is highly discouraged! It is a very hacky way of accessing bundle fields. This hack is provided " + 
        "to enable flawless 1-1 translation from verilog which does not provide ability to enumerate struct fields. " +
        "This code should be refactored to leverage Chisel generation abilities, in this case Record.elements"
      )
      // To do ? add pointer to file:loc ? (using stacktrace) 
      println(s"[INFO] Recording only a single example: $s")
    }
  }
}

/** Buffer class for implicit RecordField usage
 *
 *  Internal use only
 */
class RecordSubRangeAccess[T <: Record](b: T) {
  def apply(high: Int, low: Int): SelectedRecordFields[T] = {
    // here comes the funny part
    // elements are listed starting with last defined
    // the last element defined are also the lowest ranking bits, starting as index 0
    // NB: getWidth must work for this conversion to success
    var startingIndex = 0
    var index = 0
    val fields = b.elements.toSeq.map { case (s, data) => {
      val low = startingIndex
      val w = data.getWidth
      val high = low + w - 1
      startingIndex = high + 1
      index = index + 1
      RecordField(b, s, data, index-1, DRange(high, low), DRange(high, low))
    }}
    
    // val msg = s"Extract field #${b.index} `${b.name}` with downto range [$high:$low] --> ${b.data}"
    
    
    val sel = fields.flatMap(f => {
      if(f.rdef.high <= high && f.rdef.low >= low) { // case of inclusion either exact or with order fields on sides
        Some(f)
      } else if (f.rdef.high >= high && f.rdef.low == low) {
        // trunking fields on higher bits 
        Some(f.copy(select = f.select.copy(high = high)))
      } else if (f.rdef.high == high && f.rdef.low <= low) {
        // trunking lower bits
        Some(f.copy(select = f.select.copy(low = low)))
      } else if (f.rdef.high > high && f.rdef.low < low) {
        Some(f.copy(select = DRange(high, low)))
      } else { 
        None // too high or too low to be included
      }
    })
    
    lazy val details = sel.map(f => 
      if(f.rdef == f.select) s"${f.name} (${f.data})" else s"${f.name}[${f.select.high}:${f.select.low}] (${f.data})"
    ).mkString("\n","\n","\n")
    RecordConvertUsageWarning.emit(s"Extracted fields from $b[${high}:${low}]: $details")

    SelectedRecordFields(b, sel, DRange(high, low))
  }
}

case class DRange(high: Int, low: Int){
  def <<(i:Int): DRange = DRange(high+i, low+i)
  def >>(i:Int): DRange = DRange(high-i, low-i)
}

case class SelectedRecordFields[T <: Record](b: T, fields: Seq[RecordField[T]], select: DRange) {
  private def getUInt(d: Data, range: DRange): UInt = {
    d match {
      case bit: Bits => bit.apply(range.high, range.low).asUInt
      case v: Vec[_] => new SubWordable(v).apply(range.high, range.low).asUInt
      case r: Record => new RecordSubRangeAccess(r).apply(range.high, range.low).asUInt
      case t => throw new Exception(s"BundleConvert: Unable to provide subrange read access to $t")
    }
  }
  
  def asUInt: UInt = Cat(fields.map(f => {
    if(f.select == f.rdef){
      f.data.asUInt
    } else {
      getUInt(f.data, f.select)
    }
  }))
  
  def :=(data: Data): Unit = {
    fields match {
      case Seq(f) => f := data 
      case _ => fields.foreach( f => {
        val value = getUInt(data, f.select >> select.low)
        if(f.select == f.rdef){
          f := value
        } else {
          data match {
            // NB: UInt are NOT subrange assignable
            case v: Vec[_] => new SubWordable(v).apply(f.select.high, f.select.low) := value
            case r: Record => new RecordSubRangeAccess(r).apply(f.select.high, f.select.low) := value
            case t => throw new Exception(s"BundleConvert: Unable to provide subrange write access to $t")
          }
        }
      })
    }
  }
}


case class RecordField[T <: Record](b: T, name: String, data: Data, index: Int, rdef: DRange, select: DRange) {
  // we need very kind connect for this meta field 
  def :=(d: Data): Unit = {
    val h = select.high
    val l = select.low
    d.widthOption match {
      case Some(i) if (i <= h-l + 1) => data := d.asTypeOf(data) // safe 
      case Some(i) => 
        println(s"Warning: losing ${i-(h-l+1)} bits of data from $d in its connection to field $name ($data) of bundle $b")
        data := d.asTypeOf(data) // Not safe 
      case _ => 
        println(s"Critical: unsafe connection of $d to field $name ($data) from bundle $b")
        data := d.asTypeOf(data)
    }
    
  }
}

