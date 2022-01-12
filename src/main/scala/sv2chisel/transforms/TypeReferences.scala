// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._
import sv2chisel.ir.refreshTypes._
import sv2chisel.ir.widthExpressionType._

import org.antlr.v4.runtime.CommonTokenStream

class TypeReferences(val options: TranslationOptions) extends DescriptionBasedTransform {
  // for refreshedType -- initial declaration to be in scope of process expression 
  implicit var srcFile : Option[SourceFile] = None
  implicit var stream : Option[CommonTokenStream] = None
  
  private var defDescription: Option[String] = None 
  
  def processDescription(d: Description): Description = {
    // for refreshedType -- getting actual pointers
    srcFile = currentSourceFile
    stream = currentStream
    d match {
      case m: Module => defDescription = Some(m.name) ; processModule(m)
      case p: DefPackage => defDescription = Some(p.name) ; processPackage(p)
      case d => d
    }
  }
  
  // Common functions
  def visitStatement(s: Statement)(implicit refStore: RefStore): Unit = {
    processImportStatement(s, refStore) match {
      case l: DefLogic => refStore += ((WRef(l.name), FullType(l.tpe, HwExpressionKind)))
      case p: Port => refStore += ((WRef(p.name), FullType(p.tpe, HwExpressionKind)))
      case f: DefFunction => refStore += ((WRef(f.name), FullType(f.tpe, HwExpressionKind, portRefs = f.ports)))
      case p: DefParam => refStore += ((WRef(p.name), FullType(p.tpe, p.kind, defDescription = defDescription)))
      case t: DefType => 
        val kind = t.tpe match {
          case e: EnumType => 
            val w = e.tpe match {
              case _:UnknownType => Width(BigInt(e.fields.length - 1).bitLength)
              case _ => Width(e.tpe.getWidthExpression)
            }
            val tpe = EnumFieldType(UndefinedInterval, w)
            
            e.fields.foreach(f => {
              // weird but seems standard to flatten => needs to be explicit in scala
              refStore += WRef(f.name) -> FullType(tpe, e.kind, implicitPath=Seq(t.name), defDescription=defDescription)
              // not sure if used in this way ?
              refStore += WRef(f.name, Seq(t.name)) -> FullType(tpe, e.kind, defDescription = defDescription) 
            })
            HwExpressionKind
          case _:BundleType => HwExpressionKind  
          case _ => UnknownExpressionKind
        }
        refStore += WRef(t.name) -> FullType(t.tpe, kind, true, defDescription = defDescription)
      case f: ForGen =>
        f.init match {
          case na: NamedAssign => refStore += ((WRef(na.name), FullType(IntType(na.tokens, NumberDecimal), SwExpressionKind)))
          case _ => 
        }
      case _ => 
    }
    s.foreachStmt(visitStatement)
  }
  
  def processExpression(e: Expression)(implicit refStore: RefStore): Expression = {
    trace(e, s"Entering processExpression for ${e.getClass.getName}: ${e.serialize}")
    val proc = e.mapExpr(processExpression)
    trace(e, s"Continuing processExpression for ${e.getClass.getName}: ${e.serialize} - ${e.tpe.serialize}")
    proc.mapType(processType(_)) match {
      case r: Reference => 
        refStore.get(r) match {
          case Some(ftpe) => 
            val tpe = Utils.cleanTokens(ftpe.tpe) // do not refer to remote tokens
            (ftpe.tpeRef) match {
              case true => TypeInst(r.tokens, tpe, Some(r.name), r.path, HwExpressionKind, UnknownFlow)
              case false => r.copy(tpe = processType(tpe), kind = ftpe.kind, path = r.path ++ ftpe.implicitPath)
            }          
          case _ => 
            warn(r, s"Undeclared reference ${r.serialize}")
            r
        }
      case s: SubField =>
        val tpe = s.expr.tpe match {
          case b: BundleType => b
          case UserRefType(_,_,_, b: BundleType) => b
          case t => t
        }
        tpe match {
          case b: BundleType => // this is the only one expected here 
            b.fields.collect{case Field(_, _, name, _, tpe) if (name == s.name) => tpe} match {
              case Seq(t) => s.copy(tpe = t, kind = s.expr.kind)
              case _ => 
                 critical(s, s"Unable to propagate type '${b.serialize}' for expression '${s.serialize}' because there are multiple fields named as '${s.name}'")
                 s
            }
          case t => critical(s, s"Type mismatch for expression '${s.serialize}' got '${t.serialize}' while expecting a BundleType"); s
        }
      case s: SubIndex => s.refreshedType
      case s: SubRange => s.refreshedType
      case c: Concat => 
        debug(c, s"Concat: ${c.serialize}")
        c.args.foreach(a => debug(a, s"${a.serialize} ---- ${a.getClass.getName} ${a.tpe.serialize} => ${a.tpe}"))
        c
      case c: DoCall => 
        if(refStore.contains(c.fun)){
          val fdef = refStore(c.fun)
          val cleanTpe = Utils.cleanTokens(fdef.tpe) // do not refer to remote tokens
          val scopedTpe = cleanTpe match {
            case u: UserRefType if(!refStore.contains(u)) => 
              // simple attempt to apply current call scope to type
              val attempt = u.copy(path = c.fun.path ++ u.path)
              if(refStore.contains(attempt)){
                attempt
              } else {
                warn(c, s"Unable to retrieve scope for user-defined type ${u.serialize}. This will likely cause further errors.")
                u
              }
            case t => t 
          }
          
          val remoteArgsTypes = fdef.portRefs.map(p => (p.name, FullType(p.tpe, HwExpressionKind)))
          val updatedArgs = processAssignSeq(c.args, remoteArgsTypes, s"for call of function `${c.fun.serialize}`")
          
          c.copy(tpe = processType(scopedTpe), kind = fdef.kind, args = updatedArgs)        
        } else {
          warn(c, s"Undeclared reference ${c.fun.serialize}")
          c
        }
        
      case c@DoCast(_, _, _, u: UserRefType) =>
        if(!refStore.contains(u)){
          critical(c, s"Undeclared cast type ${u.serialize}")
          c
        } else {
          c.copy(kind = refStore(u).kind) // only retrieve kind associated to type declaration
        }

      case exp: Expression => exp
    }
  }
  
  def processType(t: Type, defDes: Option[String] = None)(implicit refStore: RefStore): Type = {
    trace(t, s"Entering processType for ${t.getClass.getName}: ${t.serialize}")
    t.mapType(processType(_, defDes)).mapWidth(_.mapExpr(processExpression)) match {
      case u: UserRefType =>  
        if(refStore.contains(u)){
          val tpe = Utils.cleanTokens(refStore(u).tpe)
          u.copy(tpe = processType(tpe, refStore(u).defDescription))
        } else {
          // need to fetch from the remote scope itself
          defDes match {
            case Some(d) =>
              refStore.get(WRef(u.name, d +: u.path)) match {
                case Some(f) => u.copy(tpe = processType(Utils.cleanTokens(f.tpe), f.defDescription))
                case _ => critical(u, s"Undeclared type ${u.serialize} in scope $d") ; u
              }
            case _ => critical(u, s"Undeclared type ${u.serialize}") ; u
          }

        }
      case tpe => tpe
    }
  }
  
  def processAssignSeq(assign: Seq[Assign], ref: Seq[(String, FullType)], ctx: String): Seq[Assign] = {
    // process ports
    val nna = assign.collect({case nna:NoNameAssign => nna})
    val na = assign.collect({case na:NamedAssign => na})
    val aa = assign.collect({case aa:AutoAssign => aa})
    (aa, nna, na) match {
      case (Seq(), Seq(), Seq()) => assign
      case (aa, Seq(), Seq()) => critical(aa.head, s"Unsupported auto-assign $ctx"); assign
      case (Seq(), nna, Seq()) =>
        if(nna.length != ref.length){
          critical(nna.head, s"Unnamed assignation list length (${nna.length}) mismatches declaration length (${ref.length}) $ctx")
          assign
        } else {
          nna.zip(ref).map({case (a, (n, ft)) => {
            a.copy(remoteName = Some(n), remoteKind = Some(ft.kind), remoteType = Some(Utils.cleanTok(ft.tpe)))
          }})
        }
      case (Seq(), Seq(), na) =>
        val refM = ref.toMap.mapValues(ft => ft.copy(tpe = Utils.cleanTok(ft.tpe)))
        na.map(a => {
          refM.get(a.name) match {
            case None => 
              critical(a, s"Unable to retrieve remote `${a.name}` parameter or port $ctx")
              a
            case Some(ft) => a.copy(remoteType = Some(ft.tpe), remoteKind = Some(ft.kind))
          }
        })
        
      case _ => critical(assign.head, s"Unsupported mixed assign methods $ctx"); assign
    }
  }
  
  def processStatement(s: Statement)(implicit refStore: RefStore): Statement = {
    trace(s, s"Entering processStatement for ${s.getClass.getName}")
    s.mapStmt(processStatement).mapExpr(processExpression).mapType(processType(_)) match {
      case i: DefInstance =>
        // let's fetch remote module if known
        currentProject.get.findModule(i.module.serialize) match {
          case Some(m) => 
            val ctx = s"for instance `${i.name}` of module `${i.module.serialize}`"
            val portMap = processAssignSeq(i.portMap, m.ports.map(p => (p.name, FullType(p.tpe,HwExpressionKind))),ctx)
            val paramMap = processAssignSeq(i.paramMap, m.params.map(p => (p.name, FullType(p.tpe, p.kind))), ctx)
            val isBlackbox = m match {
              case _:ExtModule => true
              case _ => false
            }
            i.copy(portMap = portMap, paramMap = paramMap, ioBundleConnect = isBlackbox)

          case _ => i
        }

      case st => st
    }
  }
  
  /**
   * Processing Packages References
   */
  def processPackage(p: DefPackage): DefPackage = {
    implicit val refs = new RefStore() 
    refs ++= remoteRefs
    // add local refs
    p.foreachStmt(visitStatement)
    val processed = processStatement(p.body)
    forceRefsRefresh() // be sure to refresh refs for upcoming package imports
    // propagate to local refs & record them
    p.copy(refs = Some(refs), body = processed)
  }
  
  /**
   * Processing Module References
   */
  def processModule(m: Module): Module = {
    forceRefsRefresh() // ensure that we have latest results
    implicit val ref2Type = new RefStore() 
    ref2Type ++= remoteRefs
    
    //FIRST PASS => fill ref2Type    
    m.foreachStmt(visitStatement)
    m.clock match {
      case Some(c) => ref2Type += ((WRef(c), FullType(BoolType(UndefinedInterval), HwExpressionKind)))
      case _ => 
    }
    m.reset match {
      case Some(r) => ref2Type += ((WRef(r), FullType(BoolType(UndefinedInterval), HwExpressionKind)))
      case _ => 
    }
    
    m.foreachParam(p => ref2Type += ((WRef(p.name), FullType(p.tpe, p.kind))))
    
    // SECOND PASS => use ref2Type to fill reference type
    m.copy(body = processStatement(m.body), params = m.params.map(_.mapExpr(processExpression)))
  }
}