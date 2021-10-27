// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package ir

import logger.EasyLogging

package object widthExpressionType {
  implicit def typeToWithExpressionType(t: Type) = new WidthExpressionType(t)
}

import widthExpressionType._

class WidthExpressionType(t: Type) extends EasyLogging {
  val ui = UndefinedInterval
  
  def getWidthExpression(): Expression = {
    t match {
      case v: VecType =>
        v.tpe match {
          case Seq(t) => DoPrim(ui, PrimOps.Mul(ui), Seq(v.getWidth(), t.getWidthExpression))
          case _ => 
            fatal("Unsupported MixedVec Type for width expression calculation")
            UndefinedExpression(ui)
        }
      case b: BundleType =>
        DoPrim(ui, PrimOps.GetWidth(ui), Seq(TypeInst(ui, b, None, HwExpressionKind, UnknownFlow)))
        
      case tpe: GroundType => tpe.width.expr
      case u: UserRefType => 
        DoPrim(ui, PrimOps.GetWidth(ui), Seq(TypeInst(ui, u.tpe, Some(u.name), HwExpressionKind, SourceFlow)))

      case _ => 
        fatal(s"Unsupported Type for width expression calculation: ${t}")
        UndefinedExpression(ui)
    }
  }
}