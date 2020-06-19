// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import sv2chisel.ir._
import Utils.trim

object Implicits {
  implicit def int2WInt(i: Int): WrappedInt = WrappedInt(UndefinedInterval,BigInt(i))
  implicit def bigint2WInt(i: BigInt): WrappedInt = WrappedInt(UndefinedInterval,i)
}

case class WrappedInt(tokens: Interval, value: BigInt) {
  def U: Expression = UIntLiteral(tokens, value, Width(Utils.getUIntWidth(value)), NumberDecimal)
  def S: Expression = SIntLiteral(tokens, value, Width(Utils.getSIntWidth(value)))
}
