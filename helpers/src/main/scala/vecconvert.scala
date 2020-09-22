// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel.helpers

import chisel3._

/** Package holding all implicit conversion for subword assignments
 *
 *  This package must be imported if subwords assignments are necessary. It allows a number of operation, through
 *  implicit conversion, in order to ease UInt usage as bit vectors.
 *
 *  In all vecconvert package, we use Chisel3 convention: (high, low) slicing, high is included.
 *
 */
package object vecconvert {
  implicit def vecToSubwords[T <: Data](v: Vec[T]): SubWordable[T] = new SubWordable(v)
  implicit def subwordsToVec[T <: Data](s: SubWords[T]): Vec[T]    = s.asVec
  implicit def dataToBitPatternConnectable[T <: Data](d: T): BitPatternConnectable[T] =
  new BitPatternConnectable(d)
  implicit def stringToHwString(s: String): HwString = new HwString(s)
}

import vecconvert._

/** Vec-based representation of a String in Hardware (using ASCII codes for characters)
 *
 *  This can be used to initialize some Vectors for example:
 *
 *  {{{
 *  val s = Wire(Vec(12, UInt(8.W)))
 *  s := "Hello World".V
 *  s := "World".V(12.W)
 *  }}}
 */
class HwString(s: String) {

  /** Simple conversion (length is inferred based on string length) */
  def V: Vec[UInt] = { //scalastyle:ignore method.name
    VecInit(s.toCharArray.map(_.U(8.W)))
  }

  /** Conversion with length specified
   *
   *  The conversion will fail if the String is too long to be stored in with the specified length. It the String is
   *  shorter, it will be 0-padded (on right-aligned, so left-padded).
   *
   *  @param w
   *    required width for the final Vec
   */
  def V(w: internal.firrtl.Width): Vec[UInt] = { //scalastyle:ignore method.name
    val n     = w.get
    val chars = s.toCharArray
    val l     = chars.length
    require(l <= n, s"String `$s' is too long (${l}) to fit in expected Vec($n, UInt(8.W))")
    /* If the size of a string assigned to a string variable is smaller than the declared size of the variable, then it
     * will be left-padded with zeros. */
    // from renerta http://verilog.renerta.com/source/vrg00048.htm
    VecInit.tabulate(n)(i => if (i < (n - l)) 0.U(8.W) else chars(i - n + l).U(8.W))
  }
}

/** Representation of Vec subwords
 *
 *  Vanilla Chisel3 does not allow slicing a Vec (you can only select a single element of the Vec). This class fills
 *  this gap, by representing a sub-vector which can then be cast to a Vec (and thus to other elements). This can be
 *  used directly, but is managed by the implicit conversion of vecconvert package. This class respects the Chisel3
 *  convention for slices: high (included) downto low (which is not the same as Scala convention)
 *
 *  It can be used directly or implicitly :
 *  {{{
 *  val baseVector = VecInit(Seq(true.B, false.B, true.B))
 *  // Direct usage
 *  val directSlice = SubWords(baseVector, 1, 0).asVec
 *  // Implicit usage
 *  import vecconvert._
 *  val implSlice = baseVector(1, 0)
 *  }}}
 *
 *  @tparam T
 *    contained type of the vector we want to slice
 *  @param v
 *    vector we want to slice
 *  @param high
 *    high index of the slice (included)
 *  @param low
 *    low index of the slice (included)
 */
case class SubWords[T <: Data](v: Vec[T], high: Int, low: Int) {
  require(high >= low)
  val length = high - low + 1

  // Seq where first element is LSB (as everywhere else in chisel)
  def :=(s: Seq[T]): Unit = {
    require(
        s.length <= length,
        s"Too many elements given ($s) for connection with ${v.slice(low, high + 1)}"
    )
    require(
        s.length >= length,
        s"Too few elements given ($s) for connection with ${v.slice(low, high + 1)}"
    )
    s.zip(v.slice(low, high + 1)).foreach(t => t._2 := t._1)
  }

  def :=(v: Vec[T]): Unit = :=(v.toSeq)
  // asUInt
  // Aggregates are recursively packed with the first element appearing in the least-significant bits of the result.
  def asVec: Vec[T]                   = VecInit(v.slice(low, high + 1))
  def asUInt: UInt                    = asVec.asUInt
  def asTypeOf[S <: Data](data: S): S = asVec.asTypeOf(data)
  def U: UInt                         = asUInt //scalastyle:ignore method.name

}

/** Buffer class for implicit Subwords usage
 *
 *  Internal use only
 */
class SubWordable[T <: Data](v: Vec[T]) {
  def apply(high: Int, low: Int): SubWords[T] = SubWords(v, high, low)
}

/** Trait used to define usual bit patterns */
sealed trait BitPattern

/** All zeroes bit pattern */
case object Zeroes extends BitPattern

/** All ones bit pattern */
case object Ones extends BitPattern

/** Simplified assignment of usual bit patterns
 *
 *  This can be used through implicit usage:
 *  {{{
 *  val t = Wire(UInt(8.W))
 *  t := Ones // => Will assign all 8 bits to 1
 *  }}}
 */
class BitPatternConnectable[T <: Data](d: Data) {
  def :=(that: BitPattern): Unit = {
    that match {
      case Zeroes => d := 0.U.asTypeOf(d)
      case Ones   => d := (~0.U(d.getWidth.W)).asTypeOf(d)
    }
  }
}
