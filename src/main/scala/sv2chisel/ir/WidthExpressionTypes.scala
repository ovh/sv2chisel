// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package ir

import logger.{LazyLogging}
import sv2chisel.transforms.InfoLogger
import org.antlr.v4.runtime.{CommonTokenStream}


package object widthExpressionType {
  implicit def typeToWithExpressionType(t: Type) = new WidthExpressionType(t)
}

import widthExpressionType._

class WidthExpressionType(t: Type) extends LazyLogging with InfoLogger {
  var currentSourceFile : Option[SourceFile] = None
  var currentStream : Option[CommonTokenStream] = None
  
  val ui = UndefinedInterval
  
  def getWidthExpression(implicit currentSourceFile : Option[SourceFile], currentStream : Option[CommonTokenStream]): Expression = {
    this.currentSourceFile = currentSourceFile
    this.currentStream = currentStream
    t match {
      case v: VecType =>
        v.tpe match {
          case Seq(_: BoolType) => v.getLen // avoid a useless mul by one
          case Seq(t) => DoPrim(ui, PrimOps.Mul(ui), Seq(v.getLen, t.getWidthExpression))
          case _ => 
            fatal(v, "Unsupported MixedVec Type for width expression calculation")
            UndefinedExpression(ui)
        }
      case b: BundleType =>
        DoPrim(ui, PrimOps.GetWidth(ui), Seq(TypeInst(ui, b, None, Seq(), HwExpressionKind, UnknownFlow)), SwExpressionKind, IntType())
        
      case tpe: GroundType => tpe.width.expr
      case u: UserRefType => 
        DoPrim(ui, PrimOps.GetWidth(ui), Seq(TypeInst(ui, u.tpe, Some(u.name), u.path, HwExpressionKind, SourceFlow)), SwExpressionKind, IntType())

      case _ => 
        fatal(t, s"Unsupported Type for width expression calculation: ${t}")
        UndefinedExpression(ui)
    }
  }
}