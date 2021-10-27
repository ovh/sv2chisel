// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package ir

import ir.PrimOps._

package object evalExpression {
  implicit def expressionToEvalExpression(e: Expression) = new EvalExpression(e)
}

class EvalExpression(e: Expression) {
  val ui = UndefinedInterval
  
  private def evalBinOp(s: Seq[Expression], f:(BigInt, BigInt)=> BigInt): Option[BigInt] = {
    s match {
      case Seq(a, b) => 
        (recEvalBigInt(a), recEvalBigInt(b)) match {
          case (Some(e1), Some(e2)) => Some(f(e1, e2))
          case _ => None
        }
      case _ => None 
    }
  }
  
  private def evalBoolOp(s: Seq[Expression], f:(BigInt, BigInt)=> Boolean): Option[BigInt] = {
    def tmp(a: BigInt, b: BigInt): BigInt = if(f(a, b)) BigInt(1) else BigInt(0)
    evalBinOp(s, tmp)
  }
  
  private def evalUnaryOp(s: Seq[Expression], f:(BigInt) => BigInt): Option[BigInt] = {
    s match {
      case Seq(e) => 
        (recEvalBigInt(e)) match {
          case (Some(e1)) => Some(f(e1))
          case _ => None
        }
      case _ => None 
    }
  }
  
  private def recEvalBigInt(e: Expression): Option[BigInt] = {
    e match {
      // supported primitives
      case n: Number => Some(n.getBigInt) 
      case l :Literal => Some(l.value)
      
      // DoPrim 
      case DoPrim(_, Par(_), s, _, _) => evalUnaryOp(s, bi => bi)
      
      case DoPrim(_, Mul(_), s, _, _) => evalBinOp(s, _*_)
      case DoPrim(_, Pow(_), s, _, _) => 
        evalBinOp(s, (a, b) => BigInt(math.pow(a.toDouble,b.toDouble).toLong))
      case DoPrim(_, Div(_), s, _, _) => evalBinOp(s, _/_)
      case DoPrim(_, Rem(_), s, _, _) => evalBinOp(s, _%_)
      case DoPrim(_, Add(_), s, _, _) => evalBinOp(s, _+_)
      case DoPrim(_, Sub(_), s, _, _) => evalBinOp(s, _-_)
      
      case DoPrim(_, Lt(_), s, _, _) => evalBoolOp(s, _<_)
      case DoPrim(_, Leq(_), s, _, _) => evalBoolOp(s, _<=_)
      case DoPrim(_, Gt(_), s, _, _) => evalBoolOp(s, _>_)
      case DoPrim(_, Geq(_), s, _, _) => evalBoolOp(s, _>=_)
      case DoPrim(_, Eq(_), s, _, _) => evalBoolOp(s, _==_)
      case DoPrim(_, Neq(_), s, _, _) => evalBoolOp(s, _!=_)
      
      case DoPrim(_, Not(_), Seq(expr), _, _) => 
        recEvalBigInt(expr) match {
          case None => None
          case Some(v) if (v == 0) => Some(BigInt(1))
          case _ => Some(BigInt(0))
        }
      
      case DoPrim(_, And(_), Seq(e1, e2), _, _) => 
        (recEvalBigInt(e1), recEvalBigInt(e2)) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(a), Some(b)) if(a == 1 && b == 1) => Some(BigInt(1))
          case _ => Some(BigInt(0))
        }
      case DoPrim(_, Or(_), Seq(e1, e2), _, _) => 
        (recEvalBigInt(e1), recEvalBigInt(e2)) match {
          case (None, _) => None
          case (_, None) => None
          case (Some(a), Some(b)) if(a == 0 && b == 0) => Some(BigInt(0))
          case _ => Some(BigInt(1))
        }
      
      case DoPrim(_, BitNeg(_), s, _, _) => evalUnaryOp(s, ~_)
      case DoPrim(_, BitAnd(_), s, _, _) => evalBinOp(s, _&_)
      case DoPrim(_, BitOr(_), s, _, _) => evalBinOp(s, _|_)
      case DoPrim(_, BitXor(_), s, _, _) => evalBinOp(s, _^_)
      
      case DoPrim(_, Shl(_), s, _, _) => evalBinOp(s, (a, b) => a << b.toInt)
      case DoPrim(_, Shr(_), s, _, _) => evalBinOp(s, (a, b) => a >> b.toInt)
      case DoPrim(_, LogShr(_), s, _, _) => evalBinOp(s, (a, b) => a.toInt >>> b.toInt)
        
      case DoPrim(_, Plus(_), s, _, _) => evalUnaryOp(s, bi => bi)
      case DoPrim(_, Minus(_), s, _, _) => evalUnaryOp(s, -_)
      
      case DoPrim(_, CeilLog2(_), s, _, _) => evalUnaryOp(s, in => (in-1).bitLength)
      case DoPrim(_, InlineIf(_), s, _, _) => 
        s match {
          case Seq(a, b, c) => 
            recEvalBigInt(a) match {
              case None => None 
              case Some(v) if (v == 0) => recEvalBigInt(c)
              case _ => recEvalBigInt(b)
            }
          case _ => None 
        }
      // Note : concat is not supported 
      case _ => None 
    }
  }
  
  def evalBigIntOption(): Option[BigInt] = recEvalBigInt(e)
  def evalBigInt(): BigInt = this.evalBigIntOption().get
}