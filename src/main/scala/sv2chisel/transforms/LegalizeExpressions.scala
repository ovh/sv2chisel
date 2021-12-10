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
import sv2chisel.ir.expressionToLiteral._

class LegalizeExpressions(val options: TranslationOptions) extends DescriptionBasedTransform {
  val baseUInt = UIntType(UndefinedInterval, UnknownWidth(), NumberDecimal)
  
  def processDescription(d: Description): Description = {
    // for refreshedType
    implicit val srcFile = currentSourceFile
    implicit val stream = currentStream
    // Insert cast wherever needed 
    // update kind whenever needed to push cast as UInt as far as possible:
    // ref.U + 1.U should be (ref+1).U
    // to achieve such a behavior 
    // we need :
    // processExpression does not enforce cast during the recursive walk 
    // use a docast function to enforce casts
    // Note : explict DoCast are not required for Number (HwExpressionKind is the cast)
    
    def sameLength(a: VecType, b: VecType): Boolean = {
      trace(a, s"from: ${a.serialize} - ${a.bound.serialize} to: ${b.serialize} - ${b.bound.serialize}")
      (a.bound.evalBigIntOption, b.bound.evalBigIntOption) match {
        case (Some(wa), Some(wb)) => wa == wb
        case _ => Utils.eqRawExpr(a.bound, b.bound)
      }
    }
    
    def shouldCastHW(from: Type, to: Type): Boolean = {
      (from, to) match {
        case (t1, t2) if (Utils.eq(t1, t2)) => false
        case (_, _:UnknownType) => 
          trace(from, s"Aborting cast from ${from.serialize} to UnknownType()")
          false
        case (_:UnknownType, _) => 
          debug(from, s"Aborting cast from UnknownType() to ${to.serialize}")
          // debug(from, Utils.getStackHere(this.getClass.getName))
          false
        case (_, to:TypeOf) if(!shouldCastHW(from, to.expr.tpe)) => 
          trace(from, s"Aborting cast from ${from.serialize} to ${to.serialize} as underlying TypeOf do not require it")
          false
        case (_, to:UserRefType) if(!shouldCastHW(from, to.tpe)) => 
          trace(from, s"Aborting cast from ${from.serialize} to ${to.serialize} as underlying UserRefType do not require it")
          false

        case (_: UIntType, _: UIntType) => false
        case (_: SIntType, _: SIntType) => false // should depend on width ?
        case (from: VecType, to: VecType) => 
          (from.tpe, to.tpe) match {
            case (Seq(fromEltTpe), Seq(toEltTpe)) => shouldCastHW(fromEltTpe, toEltTpe) || !sameLength(from, to)
            case _ => fatal(from, "Unsupported mixed vec - aborting cast"); false
          }
          
        case (_: BoolType, _: BoolType) => false
        case (_: BoolType, _: UIntType) => false
        case (_: EnumType, _: UIntType) => false
        case (_: UIntType, _: EnumType) => false
        // checking to.getClass.getSuperclass.isInstance(from) is non-sense given our IR Type structure
        case _ => true
      }
    }
    
    // returns None <> do not cast
    def doCastIfCompat(e: Expression, toKind: ExpressionKind, toTpe: Type): Expression = {
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
        case (HwExpressionKind, HwExpressionKind) => shouldCastHW(e.tpe, toTpe)
          
        case (SwExpressionKind, HwExpressionKind) => true
        case _ => critical(e, s"Illegal cast attempted from ${e.serialize} to $toKind $toTpe"); false
      }) match {
        case false => e
        case true => DoCast(e.tokens, e, toKind, Utils.cleanTokens(toTpe))
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
              critical(e, s"docast Unknown expression kind for expression ${e.serialize} as ${tpe.serialize}@$kind")
              e
          }
        // special case for "anyhardware" holder
        case (_:DataType, HwExpressionKind, HwExpressionKind) => e // nothing to do
        case (_:DataType, HwExpressionKind, _) => doCastIfCompat(e, HwExpressionKind, baseUInt)
        case (_:DataType, k, ke) =>
          critical(e, s"Aborting cast of ${e.serialize}(@$ke) to DataType@$k")
          e
          
        case (_: UnknownType, _, _) =>
          (e.kind, kind) match {
            case (SwExpressionKind, HwExpressionKind) => 
              e.toLiteralOption match {
                case Some(l) => l
                case None =>
                  critical(e, s"Aborting cast of ${e.serialize}(@Sw) to UnknownType()@Hw")
                  e
              }
              
            case _ => 
              trace(e, s"Aborting cast to UnknownType(): ${e.serialize}")
              e // nothing to do 
          }
        
        case _ =>  
          e match {
            case u: UndefinedExpression => u
            case n: Number => 
              tpe match {
                case _: BoolType => n.getInt match {
                  case Some(0) => BoolLiteral(n.tokens, false, kind)
                  case Some(1) => BoolLiteral(n.tokens, true, kind)
                  case _ =>
                    warn(n, s"Unexpected number ${n.serialize} for bool conversion")
                    BoolLiteral(n.tokens, true, kind)
                }
                case _: VecType => doCastIfCompat(n.copy(kind = kind), kind, tpe)
                  
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
              case _: UnknownType => doCast(e, e.kind, TypeOf(e.tokens, e))
              case _ => e
            }
            
            
          case (_, _: UnknownType, _) => e
          case (_:BoolType, _:BoolType, _) => e
          case (_:UIntType, UIntType(_, UnknownWidth(), _), _) => e
          case (_:BoolType, _:UIntType, _) => e
          case (_:UIntType, _:BoolType, _) => e // there should be a cast here 
          case (UIntType(_, w1, _), UIntType(_, w2, _), _) if(Utils.cleanTokens(w1) == Utils.cleanTokens(w2)) => e
          case (t1, t2, _) if(Utils.cleanTokens(t1) == Utils.cleanTokens(t2)) => e  
          case _ => if(shouldCastHW(e.tpe, baseHwType)) doCast(e, e.kind, baseHwType) else e
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
          required match {
            case Some(k@SwExpressionKind) => (k, s)
            case Some(HwExpressionKind) => 
              val (_, se) = defaultToHw(s, baseHwType)
              (HwExpressionKind, se)
            case _ =>
              warn(s.head, s"Unable to infer common kind for ${s.map(_.serialize).mkString("(",", ",")")}")
              (UnknownExpressionKind, s)
          }
          
        case ((true, None), _) => (HwExpressionKind, defaultToHw(s, baseHwType, selfLegalizeHw)._2)

        case ((false, None), _) => (UnknownExpressionKind, s) // nothing to do 
      }
    }
    
    def castTypeAll(s: Seq[Expression], baseHwType: Type, required: Option[ExpressionKind]): (ExpressionKind, Type, Seq[Expression]) = {
      (commonKind(s), commonType(s), required) match {
        // Rules based on kinds
        // > required HW
        case ((_, Some(SwExpressionKind)), _, Some(HwExpressionKind)) => 
          val updated =  s.map(doCast(_, HwExpressionKind, baseHwType))
          val upTpe = (baseHwType, commonType(updated)) match {
            case (_:UnknownType, Some(t)) => t
            case (b, _) => b
          }
          (HwExpressionKind, upTpe, updated)
          
        // > Make uneven kinds even (NB: uneven kinds means uneven types)
        case ((true, None), _, _) => 
          val (tpe, se) = defaultToHw(s, baseHwType)
          (HwExpressionKind, tpe, se)
        case ((false, None), _, _) => (UnknownExpressionKind, UnknownType(), s)
        // > no casts required for all SW
        case ((_, Some(SwExpressionKind)), _, _) => (SwExpressionKind, UnknownType(), s) 
        case ((_, Some(UnknownExpressionKind)), ct, _) => 
          (required, ct) match {
            case (Some(k), Some(t)) => (k, t, s.map(doCast(_, k, t)))
            case (Some(k@SwExpressionKind), None) => (k, UnknownType(), s.map(doCast(_, k, UnknownType())))
            case (Some(HwExpressionKind), None) => 
              val (tpe, se) = defaultToHw(s, baseHwType)
              (HwExpressionKind, tpe, se)
            case _ =>
              warn(s.head, s"Unable to infer common type & kind for ${s.map(_.serialize).mkString("(",", ",")")}")
              (UnknownExpressionKind, UnknownType(), s)
          }
        
        // Rules based on types
        // > do nothing for even types 
        case ((_,Some(HwExpressionKind)), Some(t), _) => (HwExpressionKind, t, s)
        // usual rules applies for uneven types
        case ((_,Some(HwExpressionKind)), _, _) => (HwExpressionKind, baseHwType, legalizeHwKinds(s, baseHwType))

      }
    }
    
    def processExpressionRec(e: Expression, expected: ExpressionKind, baseTpe: Type, requireWidth: Boolean = false)(implicit lhs: Boolean): Expression = {
      e match {
        case r: Reference => r // nothing to do yet (done in doCast whenever necessary)

        case s: SubField => s.mapExpr(processExpressionRec(_, expected, s.tpe))

        case s: SubIndex =>
          // note need to check index of what => might need further casting ???
          // actually nope => required only for DoPrim
          // Global solution: generated files will include implicits to handle all this  
          val processedExpr = processExpressionRec(s.expr, expected, s.tpe)
          val expr = processedExpr.tpe match {
            case UserRefType(_,_,_,_:EnumType) => doCast(processedExpr, expected, baseUInt)
            case UserRefType(_,_,_,_:BundleType) => doCast(processedExpr, expected, baseUInt)
            case _:EnumType => doCast(processedExpr, expected, baseUInt)
            case _:BundleType => doCast(processedExpr, expected, baseUInt)
            case _ => processedExpr
          }
          val index = processExpressionRec(s.index, UnknownExpressionKind, UnknownType())
          val ind = (index.kind, index.tpe) match {
            case (HwExpressionKind, _: UIntType) => index 
            case (HwExpressionKind, _: SIntType) => index
            case (HwExpressionKind, _) => doCast(index, HwExpressionKind, baseUInt)
            case _ => index 
          }
          
          trace(s, s"SubIndex: ${s.serialize} - index kind : ${ind.kind}")
          s.copy(expr = expr, index = ind, kind = expr.kind).refreshedType
          
        case s: SubRange =>
          val processedExpr = processExpressionRec(s.expr, expected, s.tpe)
          val expr = processedExpr.tpe match {
            case _ if(lhs) => processedExpr 
            case UserRefType(_,_,_,_:EnumType) => doCast(processedExpr, expected, baseUInt)
            case UserRefType(_,_,_,_:BundleType) => doCast(processedExpr, expected, baseUInt)
            case _:EnumType => doCast(processedExpr, expected, baseUInt)
            case _:BundleType => doCast(processedExpr, expected, baseUInt)
            case _ => processedExpr
          }
          
          val left = processExpressionRec(s.left, UnknownExpressionKind, UnknownType())
          val right = processExpressionRec(s.right, UnknownExpressionKind, UnknownType())
          val (_, cast) = castThemAll(Seq(left, right), baseUInt)
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
            case (_: PrimOps.CeilLog2, _) => 
              val expr = processExpressionRec(p.args(0), UnknownExpressionKind, UnknownType()) 
              val e = expr.kind match {
                case UnknownExpressionKind => expr.mapKind(_ => SwExpressionKind)
                case _ => expr
              }
              p.copy(args = Seq(e))
              
            case (_: PrimOps.GetWidth, _) => 
              val processed = p.mapExpr(processExpression(_, HwExpressionKind, UnknownType()))
              processed.copy(tpe = IntType(), kind = SwExpressionKind)
              
            case (_: PrimOps.InlineIf, _) => 
              // NOTE: not using Rec here, instantiating a new thread with final cast
              val pred = processExpression(p.args(0), expected, BoolType(UndefinedInterval))
              
              val conseq = processExpressionRec(p.args(1), expected, castTpe)
              val alt = processExpressionRec(p.args(2), expected, castTpe)
              val (kind, tpe, cast) = castTypeAll(Seq(conseq, alt), castTpe, Some(expected))
              trace(p, s"InlineIf: ${p.serialize} kind: $kind tpe: $tpe cast: $cast")
              
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
              trace(p, s"Processing comparison ${p.serialize}")
              val exprs = p.args.map(processExpressionRec(_, expected, UnknownType())) match {
                case Seq(a, b) => (a.tpe, b.tpe) match {
                    case (_:UnknownType, _:UnknownType) => 
                      // let's try again as UInts
                      trace(o, s"No type known - attempting cast as ${baseUInt.serialize}")
                      p.args.map(processExpressionRec(_, expected, baseUInt))
                    case (t, _:UnknownType) =>
                      trace(o, s"One type known - attempting cast as ${t.serialize}")
                      Seq(a, processExpressionRec(p.args(1), expected, t))
                      
                    case (_:UnknownType, t) =>
                      trace(o, s"One type known - attempting cast as ${t.serialize}")
                      Seq(processExpressionRec(p.args(0), expected, t), b)
                      
                    case (t1, t2) => 
                      trace(o, s"Both type known: ${t1.serialize} ; ${t2.serialize}")
                      Seq(a, b)
                  }
                
                case l => 
                  fatal(p, s"Got unexpected number of arguments ${l.length} for a comparison op (2 were expected)")
                  Seq(UndefinedExpression(), UndefinedExpression())
              }
              
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
                case (_:VecType, HwExpressionKind) => doCast(a, kind, UIntType(UndefinedInterval, UnknownWidth(),NumberDecimal))
                case (_:BoolType, SwExpressionKind) => doCast(a, kind, IntType(UndefinedInterval, NumberDecimal))
                case _ => a
              }})
              
              val updatedTpe = (commonType(cast), commonKind(cast)) match {
                case (Some(t), (_, Some(HwExpressionKind))) => t
                case _ => tpe // do not mess up with SwKinds
              }
              
              trace(p, s"Processing NumOp $o (after cast)")
              cast.foreach(e => trace(p, s"${e.serialize} -- $e"))
              
              p.copy(args = cast, kind = kind, tpe = updatedTpe)
              
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
            
            case (_: PrimOps.Par, _) =>
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
                      DoCast(UndefinedInterval, n, expected, UIntType(UndefinedInterval, Utils.cleanTokens(w), n.base))
                    case _ => 
                      debug(o, "Automated cast within BitNeg might be inaccurate")
                      DoCast(UndefinedInterval, arg, expected, Utils.cleanTokens(arg.tpe.mapWidth(_ => w)))
                  }
                case (HwExpressionKind, None, Some(w)) => 
                  // use intermediary UInt anyway to be able to apply the Neg operator
                  DoCast(UndefinedInterval, arg, expected, UIntType(UndefinedInterval, Utils.cleanTokens(w), NumberDecimal))
                  
                case _ => arg // nothing to do (no width or SwExpressionKind where width does not make sense)
              }
              p.copy(args = Seq(a), kind = a.kind, tpe = a.tpe)
              
            case (_: PrimOps.BitOp, _) => //bitwise operator (except BitNeg)
              val exprs = p.args.map(processExpressionRec(_, expected, baseTpe))
              val (kind, tpe, args) = castTypeAll(exprs, baseUInt, None)
              p.copy(args = args, kind = kind, tpe = tpe)
              
            case (_: PrimOps.RedOp, _) => //reduction operator (unary)
              val exprs = p.args.map(processExpressionRec(_, expected, baseUInt))
              val (_, args) = castThemAll(exprs, baseUInt, Some(HwExpressionKind))
              p.copy(args = args, kind = HwExpressionKind, tpe = BoolType(UndefinedInterval))
            
            case _ => Utils.throwInternalError("Impossible")
          }
          
        
        case c: DoCall => 
          val updatedArgs = c.args.map(a => {
            a match {
              case r: RemoteLinked => 
                val kind = r.remoteKind match {
                  case None => 
                    critical(a, s"No remote kind found for argument ${a.serialize} in function call ${c.serialize}")
                    UnknownExpressionKind
                  case Some(k) => k 
                }
                val tpe = r.remoteType match {
                  case None => 
                    critical(a, s"No remote type found for argument ${a.serialize} in function call ${c.serialize}")
                    UnknownType()
                  case Some(t) => t 
                }
                r match {
                  case na: NoNameAssign => na.copy(expr = processExpression(na.expr, kind, tpe))
                  case na: NamedAssign => na.copy(expr = processExpression(na.expr, kind, tpe))
                }
                
              case _ =>
                critical(a, s"No remote kind and type for argument ${a.serialize} in function call ${c.serialize}")
                a
            }
          })
          c.copy(args = updatedArgs)
        
        case c: DoCast => // user cast ($signed / $unsigned / custom)
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
            case (_: UserRefType, _, _, _) => cast.tpe // don't care about width here => .asTypeOf(<user-type>)
            
            case _ =>
              critical(c, s"Unsupported user cast to type ${cast.tpe.serialize} (baseTpe: ${baseTpe.serialize})")
              cast.tpe
          }
          cast.copy(tpe = tpe)
        
        case c: Concat => // might be too aggressive ?
          c.args.foreach(e => trace(e, s"Before Concat expr: ${e.serialize}(${e.getClass.getName}) - ${e.kind} - ${e.tpe.serialize}(${e.tpe.getClass.getName})"))
          val exprs = c.args.map(processExpressionRec(_, expected, baseTpe))
          exprs.foreach(e => trace(e, s"After Concat expr: ${e.serialize}(${e.getClass.getName}) - ${e.kind} - ${e.tpe.serialize}(${e.tpe.getClass.getName})"))
          
          val (kind, args) = castThemAll(exprs, baseUInt, Some(expected), true)
          
          val tpe = options.removeConcat.useChiselCat match {
            case true => 
              // let's try to compute the actual width of this concat
              // 1st build the expression as a sum of each terms width
              // 2nd then try to evaluate this expression if it is only made of numbers
              c.copy(args = args).getWidthOption match {
                case Some(e) =>
                  val w = e.evalBigIntOption() match {
                    case Some(bg) => Width(bg)
                    case None => Width(e)
                  }
                  UIntType(UndefinedInterval, w, NumberDecimal)
                case None => baseUInt
              }
              
            case false => UnknownType() // proper Type update to be handled by RemoveConcats
          }
          
          c.copy(args = args, kind = kind, tpe = tpe)
          
        case r: ReplicatePattern => 
          val scaler = processExpression(r.scaler, SwExpressionKind, IntType(UndefinedInterval, NumberDecimal))
          
          val subtype = baseTpe match {
            case u: UIntType => BoolType(u.tokens)
            case v: VecType =>
              v.tpe match {
                case Seq(t) => t
                case _ => critical(r, "Unsupported mixed vec type") ; baseUInt
              }
            case _ =>  
              critical(r, s"Expected a VecType for replicate pattern: ${r.serialize} ")
              baseUInt
          }
          
          val pat = processExpression(r.pattern, expected, subtype)
          
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
                    case Seq(t: UIntType) => 
                      t.width.expr.evalBigIntOption match {
                        // nothing pad properly directly here
                        case Some(i) if i == 8 => sl.copy(width = Width(UndefinedInterval,v.getLen))
                        case _ => sl
                      }
                    case _ => sl
                  }
                case _: UnknownType => sl // nothing to do
                case _: UIntType => sl // nothing to do
                case _ => 
                  warn(s, s"Probably inconsistent stringLit baseType: $baseTpe")
                  DoCast(UndefinedInterval, sl, HwExpressionKind, Utils.cleanTokens(baseTpe))
              }
            case _ => s 
          }
          
        case f: FillingBitPattern => 
          (expected, f.kind) match {
            case (_, UnknownExpressionKind) => f.copy(kind = expected)
            case _ => f
          }
        case a: AssignPattern => 

          val assigns = a.assign.map(t => {
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
          })
          val (kind, tpe, values) = castTypeAll(assigns.map(_._2), baseUInt, None)

          val resultingType = (baseTpe, tpe) match {
            case (_, _:UnknownType) => baseTpe // non usuable
            case (v: VecType, _) => v.mapType(_ => tpe)
            case _ => baseTpe
          }
          trace(a, s"Processing AssignPattern with baseType: ${baseTpe.serialize} -- retrieved innerType ${tpe.serialize}")
          
          val updatedKind = a.kind match {
            case UnknownExpressionKind => kind
            case k => k  
          }
          
          a.copy(assign = assigns.map(_._1).zip(values), kind = updatedKind, tpe = resultingType)
        
        case na: NamedAssign => na.mapExpr(processExpressionRec(_, expected, baseTpe))
        
        case t: TypeInst => if(t.name.isDefined) t else t.mapType(processType) // no need to process references
        
        case u: UIntLiteral => u 
        
        case _ => 
          warn(e, s"unsupported expression: ${e.serialize}")
          e
      }
      
    }
    
    def processExpression(e: Expression, kind: ExpressionKind, tpe: Type, requireWidth: Boolean = false)(implicit lhs: Boolean): Expression = {
      // to do : propagate expected type ?
      // handle 0.U => true.B conversion
      trace(e, s"Entering processExpression for ${e.getClass.getName} : ${e.serialize}")
      doCast(processExpressionRec(e.mapType(processType), kind, tpe, requireWidth), kind, tpe)
    }

    def processType(t: Type)(implicit lhs: Boolean): Type = {
      trace(t, s"Entering processType for ${t.getClass.getName} : ${t.serialize}")
      t.mapWidth(_.mapExpr(processExpression(_, UnknownExpressionKind, UnknownType()))).mapType(processType)
    }
    
    def processStatement(s: Statement): Statement = {
      trace(s, s"Entering processStatement for ${s.getClass.getName}")
      (s match {
        case c: Connect => 
          val loc = processExpression(c.loc, HwExpressionKind, UnknownType())(lhs = true)
          trace(c, s"c.loc ${c.loc.serialize} : ${c.loc.tpe.serialize}")
          trace(c, s"loc tpe  ${loc.serialize} : ${loc.tpe.serialize}")
          val expr = processExpression(c.expr, HwExpressionKind, loc.tpe, true)(lhs = false)
          c.copy(loc = loc, expr = expr)
          
        case c: Conditionally => 
          val updatedPred = processExpression(c.pred, UnknownExpressionKind, BoolType(UndefinedInterval))(lhs = false)
          updatedPred.kind match {
            case SwExpressionKind => 
              info(c, "Converting Initially-infered hardware condition (when) into generative condition (if)")
              IfGen(c.tokens, c.attributes, updatedPred, c.conseq, c.alt)
            case _ => c.copy(pred = updatedPred)
          }
          
        case p: Print => p.mapExpr(processExpression(_, HwExpressionKind, UnknownType())(lhs = false))
        case t: Stop => t.mapExpr(processExpression(_, HwExpressionKind, UnknownType())(lhs = false))
        case d: DefLogic => d.copy(init = processExpression(d.init, HwExpressionKind, d.tpe)(lhs = false))
        case i: DefInstance =>  
          val ports = i.portMap.zipWithIndex.map(t => t._1 match {
            
            // SourceFlow Assign <=> remote input port (assign is a local source feeding the remote sink port)
            // all NamedAssign + NoNameAssign with remoteName defined
            case r:RemoteLinked if(r.flow == SourceFlow && r.remoteName.isDefined) => 
              trace(r, s"Processing port ${r.remoteName.get} (#${t._2}) of instance ${i.name} of module ${i.module.serialize} : ${r.remoteType.getOrElse(r.tpe).serialize}")
              
              // need to use a reference to avoid cast to remote type which might contain references out of scope
              val ref = Reference(
                UndefinedInterval, 
                r.remoteName.get, 
                if(i.ioBundleConnect) Seq(i.name, "io") else Seq(i.name),
                r.remoteType.getOrElse(r.tpe),
                r.remoteKind.getOrElse(r.kind),
                SourceFlow // local source
              ) 
              // processing local assignor expression whose type must comply with remote input
              r.mapExpr(processExpression(_, HwExpressionKind, TypeOf(UndefinedInterval, ref))(lhs = false))
            
            // remaining NoNameassign
            case na@NoNameAssign(_,_, SourceFlow, _, _, _, _) =>
              trace(na, s"Processing port #${t._2} (${na.remoteName.getOrElse("<unknown>")}) of instance ${i.name} of module ${i.module.serialize} : ${na.remoteType.getOrElse(na.tpe).serialize}")
              warn(na, s"Port #${t._2} of instance ${i.name} of module ${i.module.serialize} is unnamed, this might result in a cast containing remote references unknown in instantiation scope.")
              na.mapExpr(processExpression(_, HwExpressionKind, na.remoteType.getOrElse(na.tpe))(lhs = false))
            
            // SinkFlow Assign <=> remote output port (assign is a local sink fed by the remote source port)
            case r:RemoteLinked if(r.flow == SinkFlow) => 
              val remoteName = r.remoteName.getOrElse("<???>")
              trace(r, s"Processing port #${t._2} ($remoteName) of instance ${i.name} of module ${i.module.serialize} : ${r.remoteType.getOrElse(UnknownType()).serialize}")
              
              // processing local assigned expression (which cannot be cast to remain assignable)
              // remote output type must comply (be cast if required)
              // resulting whole expression to be 
              val updatedAssignee = processExpression(r.expr, HwExpressionKind, UnknownType())(lhs = true)
              
              val path = if(i.ioBundleConnect) Seq(i.name, "io") else Seq(i.name)
              
              r.remoteType match {
                case Some(remoteTpe) => 
                  // if assignor type is known let's create the corresponding reference
                  val refExpr = Reference(r.tokens, remoteName, path, remoteTpe, HwExpressionKind, SourceFlow)
                  // cast this reference to the local assignee 
                  // which might be reference or subfield, subIndex, subRange ...
                  val refCast = updatedAssignee match {
                    case _: SubRange => 
                      // too complex for vecconvert implicits ...
                      processExpression(refExpr, HwExpressionKind, updatedAssignee.tpe)(lhs = false)
                    case _ => 
                      processExpression(refExpr, HwExpressionKind, TypeOf(UndefinedInterval, updatedAssignee))(false)
                  }
                  
                  // Create assignExpr with cast if necessary
                  if(refExpr != refCast){
                    r match { // NB: full match as RemoteLinked is sealed => we will get warning if extended later
                      case na:NamedAssign => na.copy(expr = updatedAssignee, assignExpr = Some(refCast))
                      case na:NoNameAssign => na.copy(expr = updatedAssignee, assignExpr = Some(refCast))
                    }
                  } else {
                    r.mapExpr(_ => updatedAssignee)
                  }
                case _ => 
                  warn(r, s"Unknown remote type for port #${t._2} ($remoteName) of instance ${i.name} of module ${i.module.serialize}: casting by reference by default")
                  val refExpr = Reference(r.tokens, remoteName, path, UnknownType(), HwExpressionKind, SourceFlow)
                  val refCast = DoCast(r.tokens, refExpr, HwExpressionKind, TypeOf(UndefinedInterval,updatedAssignee))
                  r match {
                    case na:NamedAssign => na.copy(expr = updatedAssignee, assignExpr = Some(refCast))
                    case na:NoNameAssign => na.copy(expr = updatedAssignee, assignExpr = Some(refCast))
                  }
              }
              
            case p => p.mapExpr(processExpression(_, HwExpressionKind, UnknownType())(lhs = false))
          })
          // param types have been infered earlier
          val params = i.paramMap.map(a => a match {
            case r: RemoteLinked =>
              val castTpe = r.remoteType.getOrElse(UnknownType())
              a.mapExpr(processExpression(_, r.remoteKind.getOrElse(SwExpressionKind), castTpe)(lhs = false))
              
            case _ =>
              warn(a, s"Cannot ensure proper type legalization of parameter map of instance ${i.name}")
              a
          })
          i.copy(portMap = ports, paramMap = params)
        case p: DefParam => p.mapExpr(processExpression(_, p.kind, p.tpe)(lhs = false))
        case f: ForGen => f.mapExpr(processExpression(_, SwExpressionKind, UnknownType())(lhs = false))
        case i: IfGen => i.mapExpr(processExpression(_, SwExpressionKind, BoolType(UndefinedInterval))(lhs = false))
        case _ => s
      }).mapStmt(processStatement).mapType(processType(_)(lhs = false))
    }
    
    d.mapStmt(processStatement(_)) match {
      case m: DefModule => m.mapParam(p => p.mapExpr(processExpression(_, p.kind, p.tpe)(lhs = false)))
      case d => d
    }
  }
}