// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._
// implicits
import sv2chisel.ir.evalExpression._
import sv2chisel.ir.widthExpressionType._

import collection.mutable.{HashMap, ArrayBuffer}

class RemovePatterns(val options: TranslationOptions) extends DescriptionBasedTransform {
  private val ui = UndefinedInterval
  private val ut = UnknownType()
  private val uw = UnknownWidth()
  
  def genOnes(expr: Expression): Expression = {
    val shift = DoPrim(ui, PrimOps.Shl(ui), Seq(UIntLiteral(ui, 1, uw, NumberDecimal), expr), HwExpressionKind)
    val par = DoPrim(ui, PrimOps.Par(ui), Seq(shift), HwExpressionKind)
    DoPrim(ui, PrimOps.Sub(ui), Seq(par, UIntLiteral(ui, 1, uw, NumberDecimal)), HwExpressionKind)
  }

  def processDescription(m: Description): Description = {
    implicit val srcFile = currentSourceFile
    implicit val stream = currentStream
    // SINGLE PASS 
    
    def getFilling(e: Expression, ftpe: FullType, bit: String, forceWidth: Boolean = false): Expression = {
      trace(e, s"get filling for $e $ftpe $bit")
      (ftpe.tpe, bit) match {
        case (_: BoolType, "'0") => BoolLiteral(e.tokens, false, ftpe.kind)
        case (_: BoolType, "'1") => BoolLiteral(e.tokens, true, ftpe.kind)

        case (u: UIntType, "'0") if(forceWidth) => 
          UIntLiteral(e.tokens, 0, u.width, NumberDecimal)
          
        case (_: UIntType, "'0") => // specific width usually not required
          UIntLiteral(e.tokens, 0, uw, NumberDecimal)
          
        case (tpe, "'0") => 
          DoCast(e.tokens, UIntLiteral(e.tokens, 0, uw, NumberDecimal), e.kind, Utils.cleanTokens(tpe))
            
        case (tpe, "'1") => 
          val castTpe = tpe match {
            // avoid useless detailed cast for UInt / SInt
            case UIntType(t,_,b) => UIntType(t, UnknownWidth(), b)
            case SIntType(t,_) => SIntType(t, UnknownWidth())
            case _ => tpe
          }
          
          val expr = tpe.getWidthExpression
          expr.evalBigIntOption match {
            case None => 
              (expr.kind, e.kind) match {
                case (sw@SwExpressionKind, _) =>
                  val shift = DoPrim(ui, PrimOps.Shl(ui), Seq(Number(ui, "1"), expr), sw)
                  val par = DoPrim(ui, PrimOps.Par(ui), Seq(shift), sw)
                  val decr = DoPrim(ui, PrimOps.Sub(ui), Seq(par, Number(ui, "1")), sw)
                  DoCast(e.tokens, decr, e.kind, Utils.cleanTokens(castTpe))
                  
                case (HwExpressionKind, _) => DoCast(e.tokens, genOnes(expr), e.kind, Utils.cleanTokens(castTpe))
                case (_, HwExpressionKind) => DoCast(e.tokens, genOnes(expr), e.kind, Utils.cleanTokens(castTpe))
                  
                case _ => fatal(e, s"Unexpected UnknownExpressionKind for expression ${e.serialize}"); e
              }  
            
            case Some(v) => 
              val lit = UIntLiteral(e.tokens, (1 << v.toInt) - 1, uw, NumberDecimal)
              castTpe match {
                case _:UIntType => lit
                case _:SIntType => SIntLiteral(e.tokens, (1 << v.toInt) - 1, uw)
                case _ => DoCast(e.tokens, lit, e.kind, Utils.cleanTokens(castTpe))
              }
          }

        case _ => fatal(e, s"Unsupported expression ${e.serialize}"); e
      }
    }
    
    def getSeqValues(a: AssignPattern, ftpe: FullType): Expression = {
      // TO DO : make it recursive with underlying type fetching and so on ...
      
      val underlyingTpe = ftpe.tpe match {
        case v: VecType => v.tpe match {
          case Seq(t) => t
          case _ => 
            critical(a, s"unable to retrieve proper underlying type of ${ftpe.tpe.serialize} for assign pattern ${a.serialize}")
            UnknownType()
        }
        case _ => 
          critical(a, s"unable to retrieve proper underlying type of ${ftpe.tpe.serialize} for assign pattern ${a.serialize}: a VecType was expected here")
          UnknownType()
      }
      
      val seqValues = ArrayBuffer[Expression]()
      val mapValues = HashMap[Expression, Expression]()
      var default : Option[Expression] = None
      
      a.assign.foreach(t => {
        t match {
          case (_: UndefinedExpression, e) => seqValues += processExpression(e, ftpe.copy(tpe = underlyingTpe))
          case (_: DefaultAssignPattern, e) => default = Some(processExpression(e, ftpe.copy(tpe = underlyingTpe)))
          case (i, e) => mapValues += ((i, processExpression(e, ftpe.copy(tpe = underlyingTpe))))
        }
      })
      (seqValues.toSeq, mapValues.isEmpty) match {
        case (s, true) => SeqValues(a.tokens, s, ftpe.kind, ftpe.tpe)
        case (Seq(), false) => MappedValues(a.tokens, mapValues, default, ftpe.kind, ftpe.tpe)
        case (_, false) => critical(a, s"unexpected assign pattern leading to both mapped and sequential assignment in ${a.serialize}"); UndefinedExpression(a.tokens)
      }
    }
    
    def tpeW(e: Expression): Type = {
      e match {
        case c: DoCast =>
          (c.tpe.widthOption, c.expr.tpe.widthOption) match {
            case (Some(UnknownWidth()), Some(w)) => 
              debug(e, s"Using underlying casted expression width: ${w} for expression ${e.serialize}")
              c.tpe.mapWidth(_ => w)
            case _ => c.tpe
          }
        case _ => e.tpe
      }
    }
    
    def processExpression(e: Expression, expected: FullType): Expression = {
      val ftpe = (e.tpe, e.kind, expected) match {
        case (_: UnknownType, UnknownExpressionKind, _) => expected
        case (_: UnknownType, k, _) => FullType(expected.tpe, k)
        case (t, UnknownExpressionKind, _) => FullType(t, expected.kind)
        case (t, k, _) => FullType(t, k)
      }
      trace(s"Process expression ${e.serialize} e.tpe = ${e.tpe.serialize} ; expected = ${expected.serialize}")
      
      val exp = e match {
        case r@ReplicatePattern(_, _, f:FillingBitPattern, _, _) =>
          val newPattern = f.bit match {
              case "'0" => BoolLiteral(e.tokens, false, ftpe.kind)
              case "'1" => BoolLiteral(e.tokens, true, ftpe.kind)
              case _ => critical(f, s"unsupported filling bit pattern ${f.bit}"); f
            }
          val details = s"Using `${f.bit}` as simple Bool (${newPattern.serialize})"
          warn(r, s"Syntax glitch: Replicating a filling pattern defeats the automated *filling* system. $details")
          r.copy(pattern = newPattern)
          
        case FillingBitPattern(_, bit, _, _) => getFilling(e, ftpe, bit)
        case a: AssignPattern =>
          a.assign match {
            // usual fill them all
            case Seq((DefaultAssignPattern(_), FillingBitPattern(_, bit, _, _))) => 
              getFilling(e, ftpe, bit)
            case _ => getSeqValues(a, ftpe)
          }
        
        case d@DoPrim(_, _:PrimOps.BoolOp, args, _, _) =>
          val clean = args match {
            case Seq(f: FillingBitPattern, e) => Seq(getFilling(f, FullType(tpeW(e), ftpe.kind), f.bit), e)
            case Seq(e, f: FillingBitPattern) => Seq(e, getFilling(f, FullType(tpeW(e), ftpe.kind), f.bit))
            case s => s
          }
          d.copy(args = clean)
        
        case _ => e
      }
      exp match {
        case c:Concat =>
          // some special case to handle :)
          val stdArgs = c.args.filter { 
            case _: FillingBitPattern => false 
            case _ => true
          }
          ((c.args.size - stdArgs.size), stdArgs.isEmpty) match {
            case (0, _) => exp.mapExpr(processExpression(_, ftpe)) // Nothing to do
              
            case (1, false) =>
              trace(c, s"concat with fillingbit pattern ${c.serialize}\n with tpe ${expected.serialize}")

              val wExprs = stdArgs.map(_.tpe.getWidthExpression)
              val bgOptions = wExprs.map(_.evalBigIntOption)
              
              def reductor(a:Option[BigInt], b: Option[BigInt]): Option[BigInt] = {
                (a, b) match { 
                  case (Some(a: BigInt), Some(b: BigInt)) => Some(a+b)
                  case _ => None
                }
              }
              val fullBg = bgOptions.reduce(reductor)
              
              val expW = expected.tpe.getWidthExpression
              
              val fbWidth = (fullBg, expW.evalBigIntOption) match {
                case (Some(sum), Some(total)) => Width(total-sum)
                case (_, total) => 
                  val sumWidthExpression = (wExprs.zip(bgOptions).map {
                      case (_, Some(bg)) => Number(UndefinedInterval, s"$bg")
                      case (e, _) => e
                    }).reduce((a, b) => {
                      DoPrim(ui, PrimOps.Add(ui), Seq(a, b))
                    })
                  
                  val totalWidthExpresion = total match {
                    case Some(bg) => Number(UndefinedInterval, s"$bg")
                    case _ => expW
                  }
                  Width(DoPrim(ui, PrimOps.Sub(ui), Seq(totalWidthExpresion, sumWidthExpression)))
              }
              val utpeW = UIntType(ui, fbWidth, NumberDecimal)
              val args = c.args.map {
                case p@FillingBitPattern(_, bit, _, _) => getFilling(p, ftpe.copy(tpe = utpeW), bit, forceWidth = true)
                case a => processExpression(a, ftpe)
              }
              c.copy(args = args)
              
              
            case(1, true) => processExpression(c.args.head, ftpe) // weird edge case: concat with single bit pattern
              
            case _ => 
              critical(c, s"Illegal concat with multiple filling bit patterns: ${c.serialize}")
              c // no need for further processing
          }
          
        case _ => exp.mapExpr(processExpression(_, ftpe))
      }
    }
    
    def processStatement(s: Statement): Statement = {
      s match {
        case c: Connect => c.copy(expr = processExpression(c.expr, FullType(c.loc.tpe, c.loc.kind)))
        case p: DefParam => p.mapExpr(processExpression(_, FullType(p.tpe, p.kind)))
        case i: DefInstance =>
          // add support for remoteTypes
          val portMap = i.portMap.map(a => {
            a match {
              case r: RemoteLinked => a.mapExpr(processExpression(_, r.remoteFullTypeOrUnknown))
              case _ => a.mapExpr(processExpression(_, FullType(ut, UnknownExpressionKind)))
            }
          })
          val paramMap = i.paramMap.map(a => {
            a match {
              case r: RemoteLinked => a.mapExpr(processExpression(_, r.remoteFullTypeOrUnknown))
              case _ => a.mapExpr(processExpression(_, FullType(ut, UnknownExpressionKind)))
            }
          })
          i.copy(portMap = portMap, paramMap = paramMap)
          
        case _ => s.mapExpr(processExpression(_, FullType(ut, UnknownExpressionKind))).mapStmt(processStatement)
      }
    }
    
    m.mapStmt(processStatement) match {
      case d: DefModule => d.mapParam(p => p.mapExpr(processExpression(_, FullType(p.tpe, p.kind))))
      case mod => mod 
    }
  }
}