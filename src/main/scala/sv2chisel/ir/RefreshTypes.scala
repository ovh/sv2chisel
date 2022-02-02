// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package ir

import logger.{InfoLogging}
import org.antlr.v4.runtime.{CommonTokenStream}


package object refreshTypes {
  implicit def subRangeToSubRangeRefreshType(e: SubRange) = new SubRangeRefreshType(e)
  implicit def subIndexToSubIndexRefreshType(e: SubIndex) = new SubIndexRefreshType(e)
}

class SubRangeRefreshType(s: SubRange) extends InfoLogging {
  var currentSourceFile : Option[SourceFile] = None
  var currentStream : Option[CommonTokenStream] = None
  implicit def svnode2Interval(n:SVNode): Interval = n.tokens
  
  def refreshedType(implicit currentSourceFile : Option[SourceFile], currentStream : Option[CommonTokenStream]): SubRange = {
    this.currentSourceFile = currentSourceFile
    this.currentStream = currentStream
    
    trace(s, s"SubRange: ${s.serialize}")
    trace(Utils.getStackHere("sv2chisel.transforms.TypeReferences.process"))
    val ui = UndefinedInterval
    
    s.expr.tpe match {
      case v: VecType =>
        v.tpe match {
          case Seq(_:Type) => 
            val tpe = s.right match {
              case Number(_,"0",_,_,_) => v.mapBound(_ => s.left)
              case _ => v.mapBound(_ => DoPrim(ui, PrimOps.Sub(ui), Seq(s.left, Utils.safeOperand(s.right))))
            }
            s.copy(tpe = tpe, kind = s.expr.kind)
          case _ => 
            critical(s, s"Unsupported mixed vec type: ${s.serialize}") ; s
        }
      case _: SIntType => critical(s, s"Unsupported conversion of range to SInt: ${s.serialize}") ; s
      case _: UIntType =>
        debug(s, s"Converting range to UInt: ${s.serialize}")
        // update UIntType width according to range if known
        // Note : conversion of Vec downto to UInt (up to) : SubRanges are downto
        // this might require an additional pass ... to be confirmed ???
        // in InferUInt ; subranges should be permuted when converting a downto vec into UInt
        // Create new width expression width = s.left - s.right +1
        val width = (s.left, s.right) match {
          case (DoPrim(_, PrimOps.Sub(_), Seq(e, Number(_,"1",_,_,_)), _, _), _) => 
            DoPrim(ui, PrimOps.Sub(ui), Seq(e, Utils.safeOperand(s.right)))
          
          case (_, Number(_,"1",_,_,_)) => s.left
          case (_, Number(_,"0",_,_,_)) => 
            DoPrim(ui, PrimOps.Add(ui), Seq(s.left, Number(ui, "1")))
          
          case (_, DoPrim(_, PrimOps.Add(_), Seq(e, Number(_,"1",_,_,_)), _, _)) => 
            DoPrim(ui, PrimOps.Sub(ui), Seq(s.left, Utils.safeOperand(e)))
          
          case _ =>
            val diff = DoPrim(ui, PrimOps.Sub(ui), Seq(s.left, Utils.safeOperand(s.right)))
            DoPrim(ui, PrimOps.Add(ui), Seq(diff, Number(ui, "1")))
        }
        val tpe = UIntType(ui, Width(width), NumberDecimal)
        debug(s, s"New type: ${tpe.serialize}")
        s.copy(tpe = tpe, kind = s.expr.kind)
      
      // will be handled by bundleconvert
      case UserRefType(_,_,_,_:BundleType) => s.copy(tpe = DataType(), kind = s.expr.kind)
      case _:BundleType => s.copy(tpe = DataType(), kind = s.expr.kind)
      
      case u: UserRefType => 
        debug(s, s"Unsupported user-defined type '${u.serialize}' for subrange expression '${s.serialize}' (this should be adressed by LegalizeExpression transform)")
        s.copy(tpe = UnknownType(), kind = s.expr.kind)
        
      case t => 
        critical(s, s"Unsupported Type '${t.serialize}' for subrange expression '${s.serialize}'")
        s.copy(tpe = UnknownType(), kind = s.expr.kind)
    }
  }
  
}

class SubIndexRefreshType(s: SubIndex) extends InfoLogging {
  var currentSourceFile : Option[SourceFile] = None
  var currentStream : Option[CommonTokenStream] = None
  implicit def svnode2Interval(n:SVNode): Interval = n.tokens
  
  def refreshedType(implicit currentSourceFile : Option[SourceFile], currentStream : Option[CommonTokenStream]): SubIndex = {
    this.currentSourceFile = currentSourceFile
    this.currentStream = currentStream
    
    trace(s, s"SubIndex: ${s.serialize}")
    trace(Utils.getStackHere("sv2chisel.transforms.TypeReferences.process"))
    
    s.expr.tpe match {
      case v: VecType => // this is the only one expected here 
        v.tpe match {
          case Seq(t) => 
            val updatedKind = t match {
              case _:UIntType => HwExpressionKind
              case _ => s.expr.kind
            }
            s.copy(tpe = t, kind = updatedKind) 
          case _ => s // MixedVec unsupported
        }
      case u: SIntType => s.copy(tpe = BoolType(u.tokens)) // index of SInt is bool
      case u: UIntType => 
        debug(s, s"Converting index to UInt: ${s.serialize}")
        s.copy(tpe = BoolType(u.tokens), kind = s.expr.kind) // index of UInt is bool
        
      case i: IntType => 
        debug(s, s"Converting index to UInt: ${s.serialize}")
        s.expr match {
          // Number(tokens: Interval, value: String, width: Width, base: NumberBase, kind: ExpressionKind)
          // UIntLiteral(tokens: Interval, value: BigInt, width: Width, base: NumberBase)
          case n: Number => 
            n.getBigInt match {
              case Some(bg) => 
                val expr = UIntLiteral(n.tokens, bg, n.width, n.base)
                s.copy(tpe = BoolType(i.tokens), kind = HwExpressionKind, expr = expr)
                
              case _ => 
                warn(n, s"Unable to convert ${n.serialize} to BigInt")
                s
            }
            
            
          case e =>
            critical(s, s"Unsupported IntType for expr '${e.serialize}' for subindex expression '${s.serialize}'")
            s.copy(tpe = UnknownType(), kind = s.expr.kind)
        }
        
        
      case u: UserRefType => 
        debug(s, s"Unsupported user-defined type '${u.serialize}' for subrange expression '${s.serialize}' (this should be adressed by LegalizeExpression transform)")
        s.copy(tpe = UnknownType(), kind = s.expr.kind)
        
      case t => 
        critical(s, s"Unsupported Type '${t.serialize}' for subindex expression '${s.serialize}'")
        s.copy(tpe = UnknownType(), kind = s.expr.kind)
    }
  }
  
}