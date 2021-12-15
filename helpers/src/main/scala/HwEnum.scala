// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel.helpers
package enum

import chisel3._

// require import scala.language.implicitConversions if not enabled in scalacOptions
abstract class GenericHwEnum extends Enumeration {
  def apply(): UInt = UInt(util.log2Up(this.values.size).W)
  implicit def valueToUInt(x: Value): UInt = x.id.U
}

// Typical use-case example
private object GenericHwEnumExample extends GenericHwEnum {
  val stateA = Value
  val stateB = Value
  val stateC = Value
}
private object GenericHwEnumInlineExample extends GenericHwEnum {
  val stateA, stateB, stateC = Value
}


// require import scala.language.implicitConversions if not enabled in scalacOptions
abstract class CustomHwEnum extends Enumeration {
  def getWidth: Int = this.values.map(_.getWidth).max
  def apply(): UInt = UInt(this.getWidth.W)
  
  protected case class V(data: Data) extends super.Val {}
  implicit def valueToCustomVal(x: Value): V = x.asInstanceOf[V]
  implicit def valueToUInt(x: Value): UInt = x.data.asUInt
}

private object CustomHwEnumExample extends CustomHwEnum {
  val stateA = V(0.U)
  val stateB = V(12.U)
  val stateC = V(5.U)
}

