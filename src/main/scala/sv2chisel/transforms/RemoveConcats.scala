// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._
// implicits
import sv2chisel.ir.evalExpression._

import scala.annotation.tailrec
import collection.mutable.{HashMap, HashSet, ArrayBuffer}

class RemoveConcats(
  val llOption: Option[logger.LogLevel.Value] = None,
  val forceConcatsAsBundle : Boolean = false, 
  val noRHSasBundle : Boolean = true // TODO partially implemented
) extends DefModuleBasedTransform {
  implicit var srcFile = currentSourceFile
  implicit var stream = currentStream
  private val ui = UndefinedInterval
  private val nv = NoVerilogAttribute
  
  def processModule(m: DefModule): DefModule = {
    val refs = new HashSet[String]()
    
    // Note: only useful for lhs refs (no doPrim operations except ignored concat)
    def getRef(e: Expression): Option[Reference] = {
      trace(s"getRef ${e.serialize}")
      e match {
        case r: Reference => Some(r)
        case s: SubField => getRef(s.expr)
        case s: SubIndex => getRef(s.expr)
        case s: SubRange => getRef(s.expr)
        case _: Concat => None
        // most of it
        case _ => None
      }
    }
    
    // small first pass => hashset of used names to avoid creating conflicts
    def visitStatement(s: Statement): Unit = {
      s match {
        case l: DefLogic => refs += l.name
        case p: Port => refs += p.name
        case _ => s.foreachStmt(visitStatement)
      }
    }
    
    m.foreachParam(p => refs += p.name)
    m.foreachStmt(visitStatement)
    
    // SECOND PASS => walk to concat
    // not assigned & used in a port map => port output => emit accordingly
    //  > intermediate val <instname_portname> = Wire(new Bundle { field ...}
    //  > rename direct calls to fields as <instname_portname>.<field>
    // assigned & used in a port map => port input => emit accordingly
    val usedGenNames = new HashMap[String, Int]()
    
    def getNewNameInBundle(hint: String, usedNamesInBundle: HashMap[String, Int]): String = {
      if (usedNamesInBundle.contains(hint)) {
        usedNamesInBundle(hint) += 1
        s"${hint}_${usedNamesInBundle(hint)}"
      } else {
        usedNamesInBundle += ((hint, -1))
        hint
      }
    }
    
    def getNewName(hint: String) : String = {
      if (refs.contains(hint)) {
        val name = hint + "_auto"
        if (usedGenNames.contains(name)) {
          usedGenNames(name) += 1
          s"${name}_${usedGenNames(name)}"
        } else {
          usedGenNames += ((name, -1))
          name
        }
      } else if (usedGenNames.contains(hint)){
        usedGenNames(hint) += 1
        s"${hint}_${usedGenNames(hint)}"
      } else {
        usedGenNames += ((hint, -1))
        hint
      }
    }
    
    def getFullName(e: Expression): String = {
      e match {
        case n: Number => n.value
        case r: Reference => r.name
        case s: SubField => getFullName(s.expr) + s"_${s.name}"
        case s: SubIndex => getFullName(s.expr) + "_" + getFullName(s.index)
        case s: SubRange => getFullName(s.expr) + "_" + getFullName(s.left) + "_" + getFullName(s.right)
        case _ => 
          val str = ArrayBuffer[String]()
          e.foreachExpr(str += getFullName(_))
          str.collect{case s if s != "" => s}.mkString("","_","")
      }
    }
    
    def getNewNameExpr(e: Expression): String = {
      getNewName(getFullName(e)).stripSuffix("_")
    }
    def getNewLocalNameExpr(e: Expression): String = {
      getFullName(e).stripSuffix("_")
    }
    
    def getBundleFields(args: Seq[Expression]): Seq[(Field, Expression)] = {
      val localBundleNames = new HashMap[String, Int]()
      args.map(a => {
        val hint = getNewLocalNameExpr(a) match {
          case "" => "x"
          case s => s
        }
        trace(a, s"${a.serialize}: $hint")
        val name = getNewNameInBundle(hint, localBundleNames)
        // We are screwed up right here on tpe in most cases
        // fortunately not within the current code base
        val actualTpe = a.tpe match {
          case TypeOf(_, e) => e.tpe
          case t => t
        }
        
        val (tpe, value) = actualTpe match {
          case u: UnknownType => critical(a, s"Unknown type for expression ${a.serialize} to be required for generated Bundle Field during emission..."); (u, a)
          case (UIntType(_, Width(_, Number(_, "1",_,_,_)), _)) => 
            a match {
              case Number(t, "0",_,_,k) => (BoolType(t), BoolLiteral(t, false, k))
              case Number(t, "1",_,_,k) => (BoolType(t), BoolLiteral(t, true, k))
              case _ => 
                warn(a, s"Unexpected value ${a.serialize} for conversion to Bool of ${a.tpe.serialize}")
                (a.tpe, a)
            }
          case v: VecType =>
          v.getWidth.evalBigIntOption match {
            // beware that width = bound + 1 
            case Some(i) => (v.mapBound(_ => Number(UndefinedInterval, (i-1).toString)), a)
            case _ => (v, a)
          }
          case t => (t, a)
        }
        a match {
          case _:Concat => 
            // nested ... to do !?
            critical(a, "Unsupported nested concat operators")  
          case _ => 
        }
        (Field(ui, NoVerilogAttribute, name, Default, tpe), value)
      })
    }
    
    def processLHS(lhs: Expression, name: String): (Statement, Statement, Type) = {
      debug(lhs, s"Processing Concat ${lhs.serialize} as lhs expression.")
      trace(lhs, s"$lhs")
      val fields = lhs match {
        case Concat(_,args, _, _, _) => getBundleFields(args)
        case _ => Utils.throwInternalError(s"Unexpected expression ${lhs.serialize}") 
      }
      fields match {
        case Seq() => (EmptyStmt, EmptyStmt, UnknownType())
        case fs => 
          val tpe = BundleType(ui, fs.map(_._1))
          val pre = DefWire(name, tpe)
          val post = fs.map(f => {
              val loc = SubField(ui, Reference(ui, name, Seq()), f._1.name, HwExpressionKind, f._1.tpe)
              // cleaning ugly self typing used only to provide material for bundle field declaration
              val lhs = f._2 match {
                case DoCast(_, from, HwExpressionKind, TypeOf(_, to)) if(from == to) => 
                  from
                case DoCast(_, from, HwExpressionKind, TypeOf(_, DoCast(_, to, _, _:UIntType))) if(from == to) => 
                  from
                case e => e 
              }
              Connect(ui, nv, lhs, loc, true, false)
            })
          (pre, SimpleBlock(ui, post), tpe)
      }
    }
    
    def processRHS(rhs: Expression, lhs: Expression, doCast: Boolean): (Expression, Statement) = {
      val preStmts = ArrayBuffer[Statement]() 
      def processRHSrec(e: Expression): Expression = {
        // to do : rework this part
        // for rec process lhs should be of the type of current expression, not first provided lhs
        // mapExpr cannot be done in the same way for local concat 
        val (expr, stmt) = processRHS(e, lhs, doCast)
        appendOption(preStmts, stmt)
        expr
      }
      
      
      rhs.mapExpr(processRHSrec) match {
        case Concat(_,args, _, _, _) => 
          debug(rhs, s"Processing Concat ${rhs.serialize} as rhs expression.")
          trace(rhs, s"$rhs")
          val newName = getNewNameExpr(lhs)
          
          val fields = getBundleFields(args)
          fields.foreach(f => {debug(f._1, s"field: ${f._1.serialize} "); debug(f._2, s"expression: ${f._2.serialize}")})
          val (stmt, tpe) = fields match {
            case Seq() => (EmptyStmt, UnknownType())
            case fs => 
              val tpe = BundleType(ui, fs.map(_._1))
              val s = Seq(DefWire(newName, tpe)) ++
                fs.map(f => {
                  val loc = SubField(ui, Reference(ui, newName, Seq()), f._1.name, HwExpressionKind, f._1.tpe)
                  Connect(ui, nv, loc, f._2, true, false)
                })
              (SimpleBlock(ui, s), tpe)
          }
          val ref = Reference(ui, newName, Seq(), tpe, HwExpressionKind, SourceFlow)
          if (doCast) {
            (DoCast(ui, ref, lhs.kind, TypeOf(ui, lhs)), getStatement(preStmts,stmt))
          } else {
            (ref, getStatement(preStmts,stmt))
          }
          
        case e => (e, getStatement(preStmts, EmptyStmt))
      }
    }
    
    def appendOption(b: ArrayBuffer[Statement], s: Statement): Unit = {
      s match {
        case EmptyStmt =>
        case _ => b += s
      }
    }
    
    def getStatement(a: ArrayBuffer[Statement], s: Statement): Statement = {
      (a.isEmpty, s) match {
        case (true, _) => s
        case _ => SimpleBlock(ui, a ++ Seq(s))
      }
    }
    
    def processConnect(c: Connect): Statement = {
      val preStmts = ArrayBuffer[Statement]() 
      val postStmts = ArrayBuffer[Statement]()
      
      val (expr, stmt) =  if(noRHSasBundle){
        (legalizeConcatArgs(c.expr), EmptyStmt)
      } else {
        processRHS(c.expr, c.loc, true)
      }
      appendOption(preStmts, stmt)
      
      val connect = c.loc match {
        case lhs: Concat =>
          val newName = getNewName(s"auto_concat")
          val (pre, post, tpe) = processLHS(lhs, newName)
          appendOption(preStmts, pre)
          appendOption(postStmts, post)
          val ref = Reference(c.tokens, newName, Seq(), tpe, HwExpressionKind, SinkFlow)
          // remove useless asUInt cast
          val castExpr = expr match {
            case DoCast(_,e,_, _:UIntType) => e
            case e => e
          }
          c.copy(loc = ref, expr = DoCast(ui, castExpr, ref.kind, TypeOf(ui, ref)))
        
        case _ => c.copy(expr = expr)
      }
      if (preStmts.isEmpty && postStmts.isEmpty) {
        connect
      } else {
        preStmts.prepend(Comment(UndefinedInterval,
          s"NOTE: The following statements are auto generated due to the use of concatenation: you may hence want to refactor it\n"
        ))
        // clean any token in the statement to avoid issues with comment re-integration
        val pre = preStmts.map(Utils.cleanTokens)
        val post = postStmts.map(Utils.cleanTokens)
        SimpleBlock(connect.tokens, pre ++ Seq(connect) ++ post)
      }
    }
    
    
    def processDefLogic(d: DefLogic): Statement = {
      val preStmts = ArrayBuffer[Statement]() 
      val loc = Reference(ui, s"${d.name}", Seq(), d.tpe, HwExpressionKind, SourceFlow)
      val (init, stmt) = processRHS(d.init, loc, true)
      appendOption(preStmts, stmt)
      
      if (preStmts.isEmpty) {
        d
      } else {
        preStmts.prepend(Comment(UndefinedInterval,
          s"NOTE: The following statements are auto generated due to the use of concatenation: you may hence want to refactor it\n"
        ))
        // clean any token in the statement to avoid issues with comment re-integration
        val pre = preStmts.map(Utils.cleanTokens)
        SimpleBlock(UndefinedInterval, pre ++ Seq(d.copy(init = init)))
      }
    }
    
    def warnHiddenConcat(e: Expression): Unit = {
      e.foreachExpr(warnHiddenConcat)
      e match {
        case _: Concat => critical(e, "Unmanaged hidden concatenation ... Consider upgrading RemoveConcat transform to add proper support for this one.")
        case _ =>
      }
    }
      
    def legalizeConcatArgs(expr: Expression): Expression = {
      expr.mapExpr(legalizeConcatArgs) match {
        case c: Concat => 
          c.mapExpr(exp => exp match {
            case DoCast(t, from, HwExpressionKind, TypeOf(tt, to)) if(from == to) => 
              DoCast(t, from, HwExpressionKind, UIntType(tt, UnknownWidth(), NumberDecimal))
            case DoCast(t, from, HwExpressionKind, TypeOf(tt, DoCast(_, to, _, _:UIntType))) if(from == to) => 
              DoCast(t, from, HwExpressionKind, UIntType(tt, UnknownWidth(), NumberDecimal))
            case e => e.tpe match {
              case _: UIntType => e 
              case _: SIntType => e 
              case _: BoolType => e
              case _ => DoCast(e.tokens, e, HwExpressionKind, UIntType(e.tpe.tokens, UnknownWidth(), NumberDecimal))
            }
          })
        case exp => exp
      }
    }
    
    @tailrec
    def containsStringLit(s: Seq[Expression]): Boolean = {
      s match {
        case Seq() => false
        case se => se.head match {
          case _: StringLit => true 
          case _ => containsStringLit(se.tail)
        }
      }
    }
    
    def asScalaString(c: Concat): Expression = {
      val fmt = ArrayBuffer[String]()
      val exprs = ArrayBuffer[Expression]()
      for (e <- c.args) {
        e match {
          case s: StringLit => fmt += "\"" + s.string + "\""
          case DoPrim(_, PrimOps.Add(_), Seq(StringLit(_, "0", _, _), e), _, _) =>
            debug(c, s"Hack: Inserting expression ${e.serialize} within string $fmt (probably incomplete)")
            fmt += "%e"
            exprs += e
            
          case _: DoPrim => critical(e, s"Unsuported conversion to string for operator ${e.serialize}: Ignored")
          case _ => critical(e, s"Unsuported conversion to string for expression ${e.serialize}: Ignored")
        }  
      }
      RawScalaExprWrapper(c.tokens, fmt.mkString(" + "), exprs.toSeq)
    }
    
    def processSwValue(v: Option[Expression]): Option[Expression] = {
      v match {
        case Some(c: Concat) => 
          if(containsStringLit(c.args)){
            Some(asScalaString(c))
          } else {
            critical(c, s"Unable to remove concat for sw expression (TODO?) ${c.serialize}")
            v
          }
        case _ => v
      }
    }
    
    def processInstance(i: DefInstance): Statement = {
      val preStmts = ArrayBuffer[Statement]() 
      val postStmts = ArrayBuffer[Statement]()
      
      val paramMap = i.paramMap.map(p => {
        p match {
          case na: NamedAssign => na.copy(expr = processSwValue(Some(na.expr)).get)
          case na: NoNameAssign => na.copy(expr = processSwValue(Some(na.expr)).get)
          case _ => p
        }  
      })
      
      val portMap = i.portMap.map(p => {
        val (exp, name) = p match {
          case NamedAssign(_, n, e, _, _, _, _) => (Some(e), s"_$n")
          case NoNameAssign(_, e, _, _, _, _, _) => (Some(e), "")
          case _ => (None, "")
        }
        val expr = exp match {
          case None => p
          case Some(c@Concat(_,_,_,_, SourceFlow)) => // this concat is a source feeding the input port of the instance
            info(p, s"Remove Concat Towards instance ${p.serialize}")
            val (kind, tpe) = p match {
              case r: RemoteLinked => 
                val kind = r.remoteKind match {
                  case None => 
                    warn(p, s"No remote kind found for port ${r.getName} in instance ${i.name} of module ${i.module.serialize}")
                    UnknownExpressionKind
                  case Some(k) => k 
                }
                r.remoteType match {
                  case None => 
                    warn(p, s"No remote type found for port ${r.getName} in instance ${i.name} of module ${i.module.serialize}")
                    (kind, UnknownType())
                  case Some(t) => (kind, t)
                }
                
              case _ => 
                warn(c, s"Proper concat removal of ${c.serialize} cannot be guaranteed as the expected type is Unknown")
                (UnknownExpressionKind, UnknownType())
            }
            val (expr, stmt) = processRHS(c, Reference(ui, s"${i.name}$name", Seq(), tpe, kind, SinkFlow), false)
            appendOption(preStmts, stmt)
            expr
            
          case Some(c@Concat(_,_,_,_, SinkFlow)) => 
            info(p, s"Remove Concat from instance ${p.serialize}")
            val newName = getNewName(s"${i.name}$name")
            val (pre, post, tpe) = processLHS(c, newName)
            appendOption(preStmts, pre)
            appendOption(postStmts, post)
            Reference(ui, newName, Seq(), tpe, HwExpressionKind, SinkFlow) 
          
          case Some(c@Concat(_,_,_,_, UnknownFlow)) => 
            critical(c, s"Unable to remove the following concat because its flow is unknown. Full expression: ${c.serialize}.")
            c
            
          case Some(e) => e
        }
        p match {
          case na: NamedAssign => na.copy(expr = expr)
          case nna: NoNameAssign => nna.copy(expr = expr)
          case _ => p
        }
      })
      if (preStmts.isEmpty && postStmts.isEmpty) {
        i.copy(paramMap = paramMap)
      } else {
        preStmts.prepend(Comment(UndefinedInterval,
          s"NOTE: The following statements are auto generated due to the use of concatenation in port-map of instance ${i.name}\n" +
          s"      This default translation is very verbose, you may hence want to refactor it by:\n" +
          s"         > (TO DO AUTOMATICALLY?) Remove existing wire declaration used in concat {<w1>, <w2>, ...} and rename those wire as <bundleName>.<w1> wherever used.\n" +
          s"         > Reuse same autogenerated bundle for in and out of extra (use chiselTypeOf())\n"
        ))
        // clean any token in the statement to avoid issues with comment re-integration
        val pre = preStmts.map(Utils.cleanTokens)
        val post = postStmts.map(Utils.cleanTokens)
        SimpleBlock(i.tokens, pre ++ Seq(i.copy(portMap = portMap, paramMap = paramMap)) ++ post)
      }
    }
    
    def processStatement(s: Statement): Statement = {
      val res = s match {
        case c: Connect => processConnect(c)
        case d: DefLogic => if (forceConcatsAsBundle) processDefLogic(d) else d
        case p: DefParam => if (p.kind == SwExpressionKind) p.copy(value = processSwValue(p.value)) else p
        case i: DefInstance => processInstance(i)
        case _ => // default concat removal within expressions
          if(!forceConcatsAsBundle) {
            s.mapStmt(processStatement)
          } else {
            val hint = s match {
              case n: HasName => n.name
              case _ => "auto" 
            }
            val preStmts = ArrayBuffer[Statement]()
            
            def processExpression(e: Expression): Expression = {
              val loc = Reference(ui, s"${hint}", Seq(), e.tpe, HwExpressionKind, SourceFlow)
              val (expr, stmt) = processRHS(e, loc, false)
              appendOption(preStmts, stmt)
              expr.mapExpr(processExpression)
            }
            
            val stmt = s.mapStmt(processStatement).mapExpr(processExpression)
            
            if (preStmts.isEmpty) {
              stmt
            } else {
              preStmts.prepend(Comment(UndefinedInterval,
                s"NOTE: The following statements are auto generated due to the use of concatenation: you may hence want to refactor it\n"
              ))
              // clean any token in the statement to avoid issues with comment re-integration
              val pre = preStmts.map(Utils.cleanTokens)
              SimpleBlock(UndefinedInterval, pre ++ Seq(stmt))
            }
          }
      }
      if(forceConcatsAsBundle){
        res.foreachExpr(warnHiddenConcat)
        res
      } else {
        res.mapExpr(legalizeConcatArgs)
      }
    }
    
    m.mapStmt(processStatement).mapParam(p => p.copy(value = processSwValue(p.value)))
  }
}