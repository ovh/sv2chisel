// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package ir

import logger.{InfoLogging}
import org.antlr.v4.runtime.{CommonTokenStream}


package object expressionToLiteral {
  implicit def expToLiteral(e: Expression) = new ExpressionToLiteral(e)
}

class ExpressionToLiteral(e: Expression) extends InfoLogging {
  var currentSourceFile : Option[SourceFile] = None
  var currentStream : Option[CommonTokenStream] = None
  implicit def svnode2Interval(n:SVNode): Interval = n.tokens
  
  val ui = UndefinedInterval
  
  def toLiteralOption(implicit currentSourceFile : Option[SourceFile], currentStream : Option[CommonTokenStream]): Option[Expression] = {
    this.currentSourceFile = currentSourceFile
    this.currentStream = currentStream
    e match {
      case n: Number => 
        n.getBigInt match {
          case Some(bg) if (bg < 0) => 
            n.base match {
              case NumberDecimal => Some(SIntLiteral(n.tokens, bg, n.width))
              case b =>
              warn(n, s"[fixable] Unable to convert negative numbers with base $b to Literal")
              None
            }
            
          case Some(bg) => Some(UIntLiteral(n.tokens, bg, n.width, n.base))
          
          case _ =>
            warn(n, s"Unable to convert ${n.serialize} to BigInt")
            None
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