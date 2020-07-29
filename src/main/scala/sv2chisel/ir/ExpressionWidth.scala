// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package ir

import logger.EasyLogging

package object expressionWidth {
  implicit def expressionToExpressionWidth(e: Expression) = new ExpressionWidth(e)
}

import expressionWidth._

class ExpressionWidth(e: Expression) extends EasyLogging {
  val ui = UndefinedInterval
  
  def getWidthOption(): Option[Expression] = {
    e.tpe.widthOption match {
      case Some(w) => Some(w.expr)
      case None => 
        e match {
          case c: Concat => 
            val args = c.args.map(_.getWidthOption)
            args.collect {case a@None => a} match {
              case Seq() => 
                // OK continue
                Some(args.map(_.get).reduce((a, b) => {
                  DoPrim(ui, PrimOps.Add(ui), Seq(a, b))
                }))
              case _ => None
            }
            
            
          // case x: DoPrim => 
          // case x: Reference => 
          // case x: DoCast => 
          // case x: DoCall => 
          // case x: SubField => 
          // case x: SubIndex => 
          // case x: SubRange => 
          // case x: Number => 
          // case x: Assign => 
          // case x: MappedValues => 
          // case x: SeqValues => 
          // case x: ReplicatePattern => 
          // case x: UIntLiteral => 
          // case x: SIntLiteral => 
          // case x: BoolLiteral => 
          // case x: RawScalaExpression => 
          // case x: RawScalaExprWrapper => 
          // case x: TypeInst => 
          // case x: DontCare => 
          // case x: StringLit => 
          // case x: FillingBitPattern => 
          // case x: AssignPattern => 
          // case x: MaskedNumber => 
          // case x: FixedLiteral => 
          // case x: DefaultAssignPattern => 
          // case x: UndefinedExpression => 
          
          case _ => 
            warn(s"Unsupported Expression for width inference: ${e.serialize}")
            None
        }
    }
  }
}