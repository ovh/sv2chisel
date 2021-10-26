// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._
// implicits
import sv2chisel.ir.refreshTypes._
import sv2chisel.ir.evalExpression._
import sv2chisel.ir.expressionWidth._

import collection.mutable.{HashMap, ArrayBuffer}

class LegalizeExpressions(val llOption: Option[logger.LogLevel.Value] = None) extends DescriptionBasedTransform {
  implicit var srcFile = currentSourceFile
  implicit var stream = currentStream
  
  def processDescription(d: Description): Description = {
    // Insert cast wherever needed 
    // update kind whenever needed to push cast as UInt as far as possible:
    // ref.U + 1.U should be (ref+1).U
    // to achieve such a behavior 
    // we need :
    // processExpression does not enforce cast during the recursive walk 
    // use a docast function to enforce casts
    // Note : explict DoCast are not required for Number (HwExpressionKind is the cast)
    
    // returns None <> do not cast
    def doCastIfCompat(e: Expression, toKind: ExpressionKind, toTpe: Type): Expression = {
      toTpe match {
        case v: VecType => trace(e, s"doCastIfCompat ${e.serialize} : ${e.tpe.serialize}/${e.kind} as ${toTpe.serialize} $toKind")
        case _ => 
      }
      
      trace(e, s"doCastIfCompat ${e.serialize} : ${e.tpe.serialize}/${e.kind} as ${toTpe.serialize} $toKind")
      ((e.kind, toKind) match {
        case (UnknownExpressionKind, _) => 
          critical(e, s"Unknown expression kind for expression ${e.serialize}")
          false
        case (SwExpressionKind, SwExpressionKind) => 
          (e.tpe, toTpe) match {
            case (_: BoolType, _: BoolType) => false
            case (_: BoolType, _) => true
            case (_, _: BoolType) => true
            case _ => false
          }
        case (HwExpressionKind, HwExpressionKind) => 
          (e.tpe, toTpe) match {
            case (t1, t2) if (t1.getClass == t2.getClass) => false
            case (u: UIntType, b: BoolType) => true
            case (_: SIntType, _: UIntType) => true
            case (_: UIntType, _: SIntType) => true
            case (_, _) if(toTpe.getClass.getSuperclass.isInstance(e.tpe)) => 
              debug(s"Aborting cast from ${e.tpe.serialize} to ${toTpe.serialize}")
              false
            case _ => true
          }
        case (SwExpressionKind, HwExpressionKind) => true
        case _ => critical(e, s"Illegal cast attempted from ${e.serialize} to $toKind $toTpe"); false
      }) match {
        case false => e
        case true => DoCast(e.tokens, e, toKind, toTpe)
      }
    }

    def doCast(e: Expression, kind: ExpressionKind, tpe: Type): Expression = {
      trace(e, s"doCast ${e.serialize} as ${tpe.serialize} $kind")
      (tpe, kind, e.kind) match {
        case (_, UnknownExpressionKind, SwExpressionKind) => doCastIfCompat(e, SwExpressionKind, tpe)
        case (_, UnknownExpressionKind, _) => e // nothing to do here
        case (_, SwExpressionKind, UnknownExpressionKind) => e.mapKind(_ => SwExpressionKind)
        case (_, _, UnknownExpressionKind) =>
          e match {
            case u: UndefinedExpression => u
            case _ =>
              critical(e, s"docast Unknown expression kind for expression ${e.serialize}")
              e
          }
        
        case (_: UnknownType, _, _) => 
          (e.kind, kind) match {
            case (SwExpressionKind, HwExpressionKind) => DoCast(e.tokens, e, kind, tpe) 
            case _ => e // nothing to do 
          }
        
        case _ =>  
          e match {
            case u: UndefinedExpression => u
            case n: Number => 
              tpe match {
                case b: BoolType => n.getInt match {
                  case 0 => BoolLiteral(n.tokens, false, kind)
                  case _ => BoolLiteral(n.tokens, true, kind)
                }
                case v: VecType => doCastIfCompat(n.copy(kind = kind), kind, tpe)
                  
                case _ => n.copy(kind = kind)
              }
            case f: FillingBitPattern => f.copy(tpe = tpe)
            case a@AssignPattern(_, Seq((DefaultAssignPattern(_), FillingBitPattern(_, _, _, _))), _, _) => 
              a.copy(kind = kind, tpe=tpe) // special case handled in remove pattern
            case c: Concat => c.copy(kind = kind, tpe=tpe) // further handled in RemoveConcats
            // case r: ReplicatePattern => r.copy(kind = kind, tpe=tpe)
            case _ => doCastIfCompat(e, kind, tpe)
          }
      }
    }
    
    def commonKind(s: Seq[Expression]): (Boolean, Option[ExpressionKind]) = {
      var seenHw = false
      var seenSw = false
      def mtch(k: ExpressionKind): Unit = {
        k match {
          case SwExpressionKind => seenSw = true
          case HwExpressionKind => seenHw = true
          case _ =>
        }
      }
      
      def commonKindRec(s: Seq[Expression]): Option[ExpressionKind] = {
        s match {
          case Seq() => None
          case Seq(a) => mtch(a.kind); Some(a.kind)
          case s => 
            mtch(s.head.kind)
            commonKindRec(s.tail) match {
              case Some(k) if (k == s.head.kind) => Some(k)
              case _ => None
            }
        }
      }
      val k = commonKindRec(s)
      (seenHw && seenSw, k)
    }
    
    def commonType(s: Seq[Expression]): Option[Type] = {
      s match {
        case Seq() => None 
        case Seq(a) => Some(a.tpe)
        case s => 
          commonType(s.tail) match {
            // map interval to avoid false negative due to different source tokens
            case Some(k) if (k == s.head.tpe.mapInterval(_ => k.tokens)) => Some(k)
            case _ => None
          }
      }
    }
    
    def legalizeHwKinds[T <: Type](s: Seq[Expression], baseHwType: T, selfCast: Boolean = false): Seq[Expression] = {
      s.map(e => {
        require(e.kind == HwExpressionKind, "Only HwExpressionKind expected here")
        (e.tpe, baseHwType, selfCast) match {
          case (_, _, true) => 
            trace(e, s"legalizing type for expression ${e.serialize} : ${e.tpe.serialize}")
            e.tpe match {
              case u: UnknownType => doCast(e, e.kind, TypeOf(e.tokens, e))
              case _ => e
            }
            
            
          case (_, u: UnknownType, _) => e
          case (_:BoolType, _:BoolType, _) => e
          case (_:UIntType, UIntType(_, UnknownWidth(), _), _) => e
          case (_:BoolType, _:UIntType, _) => e
          case (_:UIntType, _:BoolType, _) => e // there should be a cast here 
          case (UIntType(_, w1, _), UIntType(_, w2, _), _) if(Utils.cleanTokens(w1) == Utils.cleanTokens(w2)) => e
          case (t1, t2, _) if(Utils.cleanTokens(t1) == Utils.cleanTokens(t2)) => e  
          case _ => 
            // allow subclasses not to be casted => much too permissive // MUST BE REFACTORED
            if(baseHwType.getClass.getSuperclass.isInstance(e.tpe)){
              debug(e, s"Aborting cast from ${e.tpe.serialize} to ${baseHwType.serialize}")
              e
            } else {
              doCast(e, e.kind, baseHwType)
            }
        }
      })
    }
    
    def defaultToHw(s: Seq[Expression], baseHwType: Type, selfLegalizeHw: Boolean = false): (Type, Seq[Expression]) = {
      // first pass : have even kinds
      val even = s.map(e => {
        e.kind match {
          case SwExpressionKind => doCast(e, HwExpressionKind, baseHwType)
          case _ => e
        }
      })
      // second pass : legalize hw types if required
      commonType(even) match {
        case Some(t) => (t, even)
        case None => (baseHwType, legalizeHwKinds(even, baseHwType, selfLegalizeHw))
      }
    }
    
    // "if needed" means all Sw is acceptable
    def castThemAll(s: Seq[Expression], baseHwType: Type, required: Option[ExpressionKind] = None, selfLegalizeHw: Boolean = false): (ExpressionKind, Seq[Expression]) = {
      (commonKind(s), required) match {
        case ((_, Some(SwExpressionKind)), Some(HwExpressionKind)) => (HwExpressionKind, s.map(doCast(_, HwExpressionKind, baseHwType)))
        case ((_, Some(SwExpressionKind)), _) => (SwExpressionKind, s) 
        
        case ((_, Some(HwExpressionKind)), _) => (HwExpressionKind, legalizeHwKinds(s, baseHwType, selfLegalizeHw))
        case ((_, Some(UnknownExpressionKind)), _) => 
          warn(s.head, s"Unable to infer common expression kind for ${s.map(_.serialize).mkString("(",", ",")")}")
          (UnknownExpressionKind, s)
        case ((true, None), _) => (HwExpressionKind, defaultToHw(s, baseHwType, selfLegalizeHw)._2)

        case ((false, None), _) => (UnknownExpressionKind, s) // nothing to do 
      }
    }
    
    def castTypeAll(s: Seq[Expression], baseHwType: Type, required: Option[ExpressionKind] = None): (ExpressionKind, Type, Seq[Expression]) = {
      (commonKind(s), commonType(s), required) match {
        // Rules based on kinds
        // > required HW
        case ((_, Some(SwExpressionKind)), _, Some(HwExpressionKind)) => (HwExpressionKind, baseHwType, s.map(doCast(_, HwExpressionKind, baseHwType)))
        // > Make uneven kinds even (NB: uneven kinds means uneven types)
        case ((true, None), _, _) => 
          val (tpe, se) = defaultToHw(s, baseHwType)
          (HwExpressionKind, tpe, se)
        case ((false, None), _, _) => (UnknownExpressionKind, UnknownType(), s)
        // > no casts required for all SW
        case ((_, Some(SwExpressionKind)), _, _) => (SwExpressionKind, UnknownType(), s) 
        case ((_, Some(UnknownExpressionKind)), _, _) => 
          warn(s.head, s"Unable to infer common expression kind for ${s.map(_.serialize).mkString("(",", ",")")}")
          (UnknownExpressionKind, UnknownType(), s)
        
        // Rules based on types
        // > do nothing for even types 
        case ((_,Some(HwExpressionKind)), Some(t), _) => (HwExpressionKind, t, s)
        // usual rules applies for uneven types
        case ((_,Some(HwExpressionKind)), _, _) => (HwExpressionKind, baseHwType, legalizeHwKinds(s, baseHwType))

      }
    }
    
    def extractVecType(tpe: Type): Option[Type] = {
      tpe match {
        case v: VecType => 
          v.tpe match {
            case Seq(t) => Some(t)
            case _ => None
          }
        case u: UIntType => Some(BoolType(u.tokens))
        case s: SIntType => Some(BoolType(s.tokens))
        case _ => None
      }
    }
    
    def processExpressionRec(e: Expression, expected: ExpressionKind, baseTpe: Type, requireWidth: Boolean = false): Expression = {
      val baseUInt = UIntType(UndefinedInterval, UnknownWidth(), NumberDecimal)
      e match {
        case r: Reference => r // nothing to do yet (done in doCast whenever necessary)

        case s: SubField => s.mapExpr(processExpressionRec(_, expected, s.tpe))

        case s: SubIndex =>
          // note need to check index of what => might need further casting ???
          // actually nope => required only for DoPrim
          // Global solution: generated files will include implicits to handle all this  
          val expr = processExpressionRec(s.expr, expected, s.tpe)
          val index = processExpressionRec(s.index, UnknownExpressionKind, UnknownType())
          val ind = (index.kind, index.tpe) match {
            case (HwExpressionKind, u: UIntType) => index 
            case (HwExpressionKind, s: SIntType) => index
            case (HwExpressionKind, _) => doCast(index, HwExpressionKind, baseUInt)
            case _ => index 
          }
          
          trace(s, s"SubIndex: ${s.serialize} - index kind : ${ind.kind}")
          // val tpe = extractVecType(expr.tpe) match {
          //   case Some(t) => t
          //   case _ => critical(s, s"Unexpected type for index/subrange ${expr.tpe}"); s.tpe
          // }
          // trace(s, s"SubIndex ${s.serialize}: ${tpe}")
          s.copy(expr = expr, index = ind, kind = expr.kind).refreshedType
          
        case s: SubRange =>
          val expr = processExpressionRec(s.expr, expected, s.tpe)
          val left = processExpressionRec(s.left, UnknownExpressionKind, UnknownType())
          val right = processExpressionRec(s.right, UnknownExpressionKind, UnknownType())
          val (kind, cast) = castThemAll(Seq(left, right), baseUInt)
          val sb = s.copy(expr = expr, left = cast(0), right = cast(1), kind = expr.kind).refreshedType
          trace(s, s"SubRange ... => ${sb.serialize}: ${sb.tpe.serialize}")
          sb
        
        case p:DoPrim => 
          // IMPORTANT NOTICE 
          // USING UINT as default type is a major choice which might lead to errors 
          // it is a logical default for sw types though
          // best would be to have a complete refactor of 
          // Number, (U|S)IntLit, (U|S)IntType, IntType  
          // Need to discriminate against due to some special operator 
          val castTpe = p.tpe match {
            case _:UnknownType => baseTpe
            case t => t  
          }
          
          (p.op, p.args.head.tpe) match {
            case (o: PrimOps.CeilLog2, _) => 
              val expr = processExpressionRec(p.args(0), UnknownExpressionKind, UnknownType()) 
              val e = expr.kind match {
                case UnknownExpressionKind => expr.mapKind(k => SwExpressionKind)
                case _ => expr
              }
              p.copy(args = Seq(e))
              
            case (o: PrimOps.GetWidth, _) => p.mapExpr(processExpression(_, HwExpressionKind, UnknownType())) // nothing else to ensure here ?
              
            case (o: PrimOps.InlineIf, _) => 
              // NOTE: not using Rec here, instantiating a new thread with final cast
              trace(p, s"InlineIf: ${p.serialize}")
              val pred = processExpression(p.args(0), expected, BoolType(UndefinedInterval))
              
              val conseq = processExpressionRec(p.args(1), expected, castTpe)
              val alt = processExpressionRec(p.args(2), expected, castTpe)
              val (kind, tpe, cast) = castTypeAll(Seq(conseq, alt), castTpe, Some(expected))
              
              val args = (pred.kind, kind) match {
                case (SwExpressionKind, HwExpressionKind) => 
                  Seq(doCast(pred, HwExpressionKind, BoolType(UndefinedInterval)), cast(0), cast(1))
                case (HwExpressionKind, SwExpressionKind) =>
                  critical(p, "TODO : proper cast of args to HW")
                  Seq(pred, cast(0), cast(1))
                case _ => Seq(pred, cast(0), cast(1))
              }
              
              val res = p.copy(tpe = tpe, args = args, kind = kind)
              if(castTpe.getClass.getSuperclass.isInstance(tpe)){
                res
              } else {
                doCast(res, kind, castTpe)
              }
            
            case (_: PrimOps.UnaryOp, _: VecType) =>
              debug(p, "Caught Unary Op of a VecType (must be converted to UInt first)")
              val expr = processExpressionRec(p.args.head, expected, castTpe)
              val exp = (expr.tpe, castTpe) match {
                case (_: VecType, _: VecType ) => doCast(expr, HwExpressionKind, baseUInt)
                case (_: VecType, t ) => doCast(expr, HwExpressionKind, t)
                case _ => expr
              }
              p.copy(args = Seq(exp), kind = exp.kind, tpe = exp.tpe)
            
            case (o@(_: PrimOps.And | _: PrimOps.Or | _: PrimOps.Not), _) => 
              val boolTpe = BoolType(UndefinedInterval)
              
              val exprs = p.args.map(processExpressionRec(_, expected, boolTpe))
              trace(p, s"Processing op $o")
              exprs.foreach(a => trace(a, s"${a.serialize} - ${a}"))
              
              val (kind, args) = castThemAll(exprs, boolTpe, Some(expected))
              trace(p, s"Processing op $o (after cast)")
              args.foreach(a => trace(a, s"${a.serialize} - ${a}"))
              val res = o match {
                case (_: PrimOps.And | _: PrimOps.Or) =>
                  // force bool type on both side
                  args match {
                    case Seq(a, b) => Seq(doCast(a, kind, boolTpe), doCast(b, kind, boolTpe))
                    case l =>
                      fatal(p, s"Got unexpected number of arguments ${l.length} for a And/Or op where 2 were expected")
                      Seq(UndefinedExpression(), UndefinedExpression())
                  }

                case _: PrimOps.Not =>
                  // force bool type on both side
                  args match {
                    case Seq(a) => Seq(doCast(a, kind, boolTpe))
                    case l =>
                      fatal(p, s"Got unexpected number of arguments ${l.length} for a Not where 1 was expected")
                      Seq(UndefinedExpression())
                  }
              }
              trace(p, s"Processing op $o (after final cast)")
              res.foreach(a => trace(a, s"${a.serialize} - ${a}"))
              p.copy(args = res, kind = kind, tpe = boolTpe)
            
            case (o: PrimOps.BoolOp, _) => 
              val boolTpe = BoolType(UndefinedInterval)
              
              val exprs = p.args.map(processExpressionRec(_, expected, UnknownType()))
              val (kind, args) = castThemAll(exprs, baseUInt, Some(expected))

              val res = o match {
                case (_: PrimOps.Eq | _: PrimOps.Neq) =>
                  // force equality of types but can be anything
                  // if types are not the same, compare values casted as UInt
                  args match {
                    case Seq(a, b) if (a.tpe.getClass == b.tpe.getClass) => Seq(a, b)
                    case Seq(a, b) if (a.tpe.isInstanceOf[BoolType]) => Seq(a, doCast(b, kind, a.tpe))
                    case Seq(a, b) if (b.tpe.isInstanceOf[BoolType]) => Seq(doCast(a, kind, b.tpe), b)
                    case Seq(a, b) => Seq(doCast(a, kind, baseUInt), doCast(b, kind, baseUInt))
                    case l => 
                      fatal(p, s"Got unexpected number of arguments ${l.length} for a Eq/Neq op where 2 were expected")
                      Seq(UndefinedExpression(), UndefinedExpression())
                  }
                  
                case _ => // Lt, Gt, Lte, Gte
                  // must be comparable values
                  args match {
                    case Seq(a, b) if (a.tpe.getClass == b.tpe.getClass) => 
                      a.tpe match {
                        case (_: IntType | _: UIntType | _:SIntType) => Seq(a, b)
                        case _ => Seq(doCast(a, kind, baseUInt), doCast(b, kind, baseUInt))
                      }
                    case Seq(a, b) => Seq(doCast(a, kind, baseUInt), doCast(b, kind, baseUInt))
                    case l => 
                      fatal(p, s"Got unexpected number of arguments ${l.length} for a comparison operator where 2 were expected")
                      Seq(UndefinedExpression(), UndefinedExpression())
                  }
              }
              p.copy(args = res, kind = kind, tpe = boolTpe)
              
              
            case (o: PrimOps.NumOp, _) => 
              val exprs = p.args.map(processExpressionRec(_, expected, baseTpe))
              trace(p, s"Processing NumOp $o")
              exprs.foreach(e => trace(e, s"${e.serialize} -- $e"))
              val (kind, tpe, args) = castTypeAll(exprs, baseUInt, None)
              
              trace(p, s"Processing NumOp $o (after castTypeAll)")
              args.foreach(e => trace(e, s"${e.serialize} -- $e"))
              val cast = args.map(a => { (a.tpe, kind) match {
                case (_:BoolType, HwExpressionKind) => doCast(a, kind, UIntType(UndefinedInterval, UnknownWidth(),NumberDecimal))
                case (_:BoolType, SwExpressionKind) => doCast(a, kind, IntType(UndefinedInterval, NumberDecimal))
                case _ => a
              }})
              
              trace(p, s"Processing NumOp $o (after cast)")
              cast.foreach(e => trace(p, s"${e.serialize} -- $e"))
              
              p.copy(args = cast, kind = kind, tpe = tpe)
              
            case (o: PrimOps.ShiftOp, _) => 
              val (e: Expression, shft: Expression) = p.args match {
                case Seq(expr, sh) => 
                  lazy val pshft = processExpression(sh, UnknownExpressionKind, UnknownType())
                  val exp = processExpressionRec(expr, expected, baseTpe)
                  (exp.tpe, o) match {
                    case (_:SIntType, _:PrimOps.Shr) => (exp, processExpression(sh, exp.kind, baseUInt))
                    case (_: IntType | _: UIntType, _) =>  (exp, pshft)
                    case _ => (doCast(exp, exp.kind, baseUInt), pshft) // SInt LogShr is here as well
                  }
                case l => 
                  fatal(p, s"Got unexpected number of arguments ${l.length} for a ShiftOp where 2 were expected")
                  (UndefinedExpression(), UndefinedExpression())
              }
              p.copy(args = Seq(e,shft), kind = e.kind, tpe = e.tpe)
            
            case (o: PrimOps.Par, _) =>
              // process through
              val exprs = p.args.map(processExpressionRec(_, expected, baseTpe))
              p.copy(args = exprs, kind = exprs(0).kind, tpe = exprs(0).tpe)
            
            case (o: PrimOps.BitNeg, _) => //bitwise operator ... not sure how they need to be managed ...
              val arg = processExpressionRec(p.args.head, expected, castTpe)
              // the point is : to be equivalent to verilog inference 
              // the bitneg must apply on a sized literal
              // if size is unknown it won't extend properly
              val a = (expected, arg.tpe.widthOption, castTpe.widthOption) match {
                case (HwExpressionKind, Some(UnknownWidth()), Some(UnknownWidth())) => arg // nothing to do
                case (HwExpressionKind, Some(UnknownWidth()), Some(w)) => 
                  arg match {
                    case n: Number => 
                      DoCast(UndefinedInterval, n, expected, UIntType(UndefinedInterval, w, n.base))
                    case _ => 
                      debug(o, "Automated cast within BitNeg might be inaccurate")
                      DoCast(UndefinedInterval, arg, expected, arg.tpe.mapWidth(_ => w))
                  }
                case (HwExpressionKind, None, Some(w)) => 
                  // use intermediary UInt anyway to be able to apply the Neg operator
                  DoCast(UndefinedInterval, arg, expected, UIntType(UndefinedInterval, w, NumberDecimal))
                  
                case _ => arg // nothing to do (no width or SwExpressionKind where width does not make sense)
              }
              p.copy(args = Seq(a), kind = a.kind, tpe = a.tpe)
              
            case (o: PrimOps.BitOp, _) => //bitwise operator (except BitNeg)
              val exprs = p.args.map(processExpressionRec(_, expected, baseTpe))
              val (kind, tpe, args) = castTypeAll(exprs, baseUInt, None)
              p.copy(args = args, kind = kind, tpe = tpe)
              
            case (o: PrimOps.RedOp, _) => //reduction operator (unary)
              val exprs = p.args.map(processExpressionRec(_, expected, baseUInt))
              val (kind, args) = castThemAll(exprs, baseUInt, Some(HwExpressionKind))
              p.copy(args = args, kind = HwExpressionKind, tpe = BoolType(UndefinedInterval))
            
            case _ => Utils.throwInternalError("Impossible")
          }
          
        
        case c: DoCall => c.mapExpr(processExpression(_, UnknownExpressionKind, UnknownType())) // nothing else to ensure here ?
        
        case c: DoCast => // user cast ($signed / $unsigned)
          val cast = c.mapExpr(processExpression(_, UnknownExpressionKind, UnknownType())) 
          
          val castWOption = cast.tpe.widthOption match {
            case Some(UnknownWidth()) => None 
            case Some(w) => Some(w)
            case _ => None
          }
          
          val baseWOption =  baseTpe.widthOption match {
            case Some(UnknownWidth()) => None 
            case Some(w) => Some(w)
            case _ => None
          }
          val baseWidth = baseWOption match {
            case Some(w) => w.expr.evalBigIntOption()
            case None => None 
          }
          
          val tpe = (cast.tpe, castWOption, baseWOption, baseWidth) match {
            case (u: UIntType, None, _, Some(i)) => u.copy(width = Width(i))
            case (s: SIntType, None, _, Some(i)) => s.copy(width = Width(i))
            
            case (u: UIntType, None, Some(w), None) => u.copy(width = w)
            case (s: SIntType, None, Some(w), None) => s.copy(width = w)
            
            case (_: SIntType, _, _, _) => 
              if(requireWidth) 
                critical(c, s"Unknown width for user cast to signed (${cast.tpe.serialize}), this will result into faulty sign extension and likely broken design")
              cast.tpe

            case (_: UIntType, _, _, _) => cast.tpe // don't care about width here => .asUInt 
            
            case _ =>
              critical(c, s"Unsupported user cast to type ${cast.tpe.serialize} (baseTpe: ${baseTpe.serialize})")
              cast.tpe
          }
          cast.copy(tpe = tpe)
        
        case c: Concat => // might be too aggressive ?
          c.args.foreach(e => trace(e, s"Before Concat expr: ${e.serialize} - ${e.kind} - ${e.tpe}"))
          val exprs = c.args.map(processExpressionRec(_, expected, baseTpe))
          exprs.foreach(e => trace(e, s"After Concat expr: ${e.serialize} - ${e.kind} - ${e.tpe}"))
          val (kind, args) = castThemAll(exprs, baseUInt, Some(expected), true)
          args.foreach(e => debug(e, s"Concat args: ${e.serialize} - ${e.kind} - ${e.tpe}"))
          
          // let's try to compute the actual width of this concat
          // 1st build the expression as a sum of each terms width
          // 2nd then try to evaluate this expression if it is only made of
          val tpe = c.copy(args = args).getWidthOption() match {
            case Some(e) =>
              val w = e.evalBigIntOption() match {
                case Some(bg) => Width(bg)
                case None => Width(e)
              }
              UIntType(UndefinedInterval, w, NumberDecimal)
            case None => baseUInt
          }
          
          c.copy(args = args, kind = kind, tpe = tpe)
          
        case r: ReplicatePattern => 
          val scaler = processExpression(r.scaler, SwExpressionKind, IntType(UndefinedInterval, NumberDecimal))
          val pat = processExpressionRec(r.pattern, expected, baseTpe)
          val ui = UndefinedInterval
          val bound = DoPrim(ui, PrimOps.Sub(ui), Seq(scaler, Number(ui, "1")), scaler.kind, scaler.tpe)
          val tpe = PackedVecType(ui, Seq(pat.tpe), bound, true)
          r.copy(scaler = scaler, pattern = pat, kind = pat.kind, tpe = tpe)
        
        case u: UndefinedExpression => u
        case d: DefaultAssignPattern => d
        case n: Number => n
        case n: DontCare => n
        case s: StringLit => 
          expected match {
            case HwExpressionKind => 
              val sl = s.copy(kind = HwExpressionKind)
              baseTpe match {
                case v: VecType =>
                  v.tpe match {
                    case Seq(t: UIntType) => t.width.expr.evalBigIntOption match {
                      // nothing pad properly directly here
                      case Some(i) if i == 8 => sl.copy(width = Width(UndefinedInterval,v.getWidth))
                    }
                    case _ => sl
                  }
                case _ => DoCast(UndefinedInterval, sl, HwExpressionKind, baseTpe)
              }
            case _ => s 
          }
          
        case f: FillingBitPattern => 
          (expected, f.kind) match {
            case (_, UnknownExpressionKind) => f.copy(kind = expected)
            case _ => f
          }
        case a: AssignPattern => 

          val assigns = a.assign.zipWithIndex.map { case (t, i) => {
            val localBaseType = baseTpe match {
              case v: VecType => v.tpe match {
                case Seq(t) => t
                case _ => critical(a, s"Unsupported MixedVecType"); UnknownType() 
              }
              case b: BundleType => 
                // try to rely on name when defined (could otherwise try sequential but it is risky)
                t._1 match {
                  case n:NamedAssign => b.fields.find(_.name == n.name).map(_.tpe).getOrElse(UnknownType())
                  case _ => UnknownType()
                }
              case u:UIntType => BoolType(u.tokens)
              case _ => UnknownType() // not usable
            }
            (processExpression(t._1, UnknownExpressionKind, UnknownType()),
              processExpressionRec(t._2, expected, localBaseType))
          }}
          val (kind, tpe, values) = castTypeAll(assigns.map(_._2), baseUInt, None)

          val resultingType = (baseTpe, tpe) match {
            case (_, _:UnknownType) => baseTpe // non usuable
            case (v: VecType, _) => v.mapType(_ => tpe)
            case _ => baseTpe
          }
          trace(a, s"Processing AssignPattern with baseType: ${baseTpe.serialize} -- retrieved innerType ${tpe.serialize}")
          
          a.copy(assign = assigns.map(_._1).zip(values), kind = kind, tpe = resultingType)
        
        case na: NamedAssign => na.mapExpr(processExpressionRec(_, expected, baseTpe))
        
        case t: TypeInst => t.mapType(processType)
        
        case _ => 
          warn(e, s"unsupported expression: ${e.serialize}")
          e
      }
      
    }
    
    def processExpression(e: Expression, kind: ExpressionKind, tpe: Type, requireWidth: Boolean = false): Expression = {
      // to do : propagate expected type ?
      // handle 0.U => true.B conversion
      trace(e, s"Entering processExpression for ${e.getClass.getName} : ${e.serialize}")
      doCast(processExpressionRec(e.mapType(processType), kind, tpe, requireWidth), kind, tpe)
    }

    def processType(t: Type): Type = {
      trace(t, s"Entering processType for ${t.getClass.getName} : ${t.serialize}")
      t.mapWidth(_.mapExpr(processExpression(_, UnknownExpressionKind, UnknownType()))).mapType(processType)
    }
    
    def processStatement(s: Statement): Statement = {
      trace(s, s"Entering processStatement for ${s.getClass.getName}")
      (s match {
        case c: Connect => 
          val loc = processExpression(c.loc, HwExpressionKind, UnknownType())
          trace(c, s"c.loc ${c.loc.serialize} : ${c.loc.tpe.serialize}")
          trace(c, s"loc tpe  ${loc.serialize} : ${loc.tpe.serialize}")
          val expr = processExpression(c.expr, HwExpressionKind, loc.tpe, true)
          c.copy(loc = loc, expr = expr)
          
        case c: Conditionally => c.mapExpr(processExpression(_, HwExpressionKind, BoolType(UndefinedInterval)))
        case p: Print => p.mapExpr(processExpression(_, HwExpressionKind, UnknownType()))
        case t: Stop => t.mapExpr(processExpression(_, HwExpressionKind, UnknownType()))
        case d: DefLogic => d.copy(init = processExpression(d.init, HwExpressionKind, d.tpe))
        case i: DefInstance =>  
          val ports = i.portMap.zipWithIndex.map(t => t._1 match {
            
            case na@NamedAssign(_,_,_, SourceFlow, _, _) => 
              trace(na, s"Processing port ${na.name} (#${t._2}) of instance ${i.name} of module ${i.module.serialize} : ${na.remoteType.getOrElse(na.tpe).serialize}")
              na.mapExpr(processExpression(_, HwExpressionKind, na.remoteType.getOrElse(na.tpe)))
            case na@NoNameAssign(_,_, SourceFlow, _, _, _) => 
              trace(na, s"Processing port #${t._2} (${na.remoteName.getOrElse("<unknown>")}) of instance ${i.name} of module ${i.module.serialize} : ${na.remoteType.getOrElse(na.tpe).serialize}")
              na.mapExpr(processExpression(_, HwExpressionKind, na.remoteType.getOrElse(na.tpe)))
              
            case na@NamedAssign(_,_,_, SinkFlow, _, _) => 
              trace(na, s"Processing port ${na.name} (#${t._2}) of instance ${i.name} of module ${i.module.serialize} : ${na.remoteType.getOrElse(UnknownType()).serialize}")
              val expr = processExpression(na.expr, HwExpressionKind, UnknownType())
              na.remoteType match {
                case Some(remoteTpe) => 
                  // Create assignExpr with cast if necessary
                  val refExpr = Reference(na.tokens, na.name, Seq(i.name), remoteTpe, HwExpressionKind, SourceFlow)
                  val refCast = processExpression(refExpr, HwExpressionKind, expr.tpe)
                  if(refExpr != refCast){
                    na.copy(expr = expr, assignExpr = Some(refCast))
                  } else {
                    na.copy(expr = expr)
                  }
                case None => na.copy(expr = expr)
              }
              
            case na@NoNameAssign(_,_, SinkFlow, _, _, _) => 
              val remoteName = na.remoteName.getOrElse("<???>")
              trace(na, s"Processing port #${t._2} ($remoteName) of instance ${i.name} of module ${i.module.serialize} : ${na.remoteType.getOrElse(UnknownType()).serialize}")
              val expr = processExpression(na.expr, HwExpressionKind, UnknownType())
              na.remoteType match {
                case Some(remoteTpe) => 
                  // Create assignExpr with cast if necessary
                  val refExpr = Reference(na.tokens, remoteName, Seq(i.name), remoteTpe, HwExpressionKind, SourceFlow)
                  val refCast = processExpression(refExpr, HwExpressionKind, expr.tpe)
                  if(refExpr != refCast){
                    if (remoteName == "<???>") {
                      critical(na, s"Unknown remote ref for port #${t._2} ($remoteName) of instance ${i.name} of module ${i.module.serialize} : ${remoteTpe.serialize}")
                    }
                    na.copy(expr = expr, assignExpr = Some(refCast))
                  } else {
                    na.copy(expr = expr)
                  }
                case _ => na.copy(expr = expr)
              }
              
            case p => p.mapExpr(processExpression(_, HwExpressionKind, UnknownType()))
          })
          val params = i.paramMap.map(_.mapExpr(processExpression(_, SwExpressionKind, UnknownType())))
          i.copy(portMap = ports, paramMap = params)
        case p: DefParam => p.mapExpr(processExpression(_, p.kind, p.tpe))
        case f: ForGen => f.mapExpr(processExpression(_, SwExpressionKind, UnknownType()))
        case i: IfGen => i.mapExpr(processExpression(_, SwExpressionKind, BoolType(UndefinedInterval)))
        case _ => s
      }).mapStmt(processStatement).mapType(processType)
    }
    
    d.mapStmt(processStatement(_)) match {
      case m: Module => m.mapParam(p => p.mapExpr(processExpression(_, p.kind, p.tpe)))
      case d => d
    }
  }
}