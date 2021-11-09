// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package ir

import logger.{LazyLogging}
import sv2chisel.transforms.InfoLogger
import org.antlr.v4.runtime.{CommonTokenStream}


package object expressionToLiteral {
  implicit def expToLiteral(e: Expression) = new ExpressionToLiteral(e)
}

class ExpressionToLiteral(e: Expression) extends LazyLogging with InfoLogger {
  var currentSourceFile : Option[SourceFile] = None
  var currentStream : Option[CommonTokenStream] = None
  
  val ui = UndefinedInterval
  
  def toLiteralOption(implicit currentSourceFile : Option[SourceFile], currentStream : Option[CommonTokenStream]): Option[Expression] = {
    this.currentSourceFile = currentSourceFile
    this.currentStream = currentStream
    e match {
      case n: Number => 
        val value = n.getBigInt
        if(value < 0) {
          n.base match {
            case NumberDecimal => Some(SIntLiteral(n.tokens, n.getBigInt, n.width))
            case b =>
              warn(n, s"[fixable] Unable to convert negative numbers with base $b to Literal")
              None
          }
        } else {
          Some(UIntLiteral(n.tokens, n.getBigInt, n.width, n.base))
        }
      case r: Reference => 
        r.tpe match {
          case i:IntType =>
            warn(r, s"Tech Debt: casting IntType to UIntType (TODO: proper signed management)")
            Some(DoCast(r.tokens, e, HwExpressionKind, UIntType(UndefinedInterval, UnknownWidth(), i.base)))
          case _ => 
            trace(r, s"Reference with tpe: ${r.tpe.serialize}")
            None
        }
      
      case _ => 
        trace(e, s"Cannot convert expression ${e.serialize} to Literal")
        None
    }
  }
}