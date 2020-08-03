// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._
import sv2chisel.ir.refreshTypes._

import collection.mutable.{HashMap, ArrayBuffer}

class TypeReferences(val llOption: Option[logger.LogLevel.Value] = None) extends DescriptionBasedTransform {
  implicit var srcFile = currentSourceFile
  implicit var stream = currentStream
  
  def processDescription(d: Description): Description = {
    d match {
      case m: Module => processModule(m)
      case p: DefPackage => processPackage(p)
      case d => d
    }
  }
  
  // Common functions
  def visitStatement(s: Statement)(implicit refStore: RefStore): Unit = {
    s match {
      case l: DefLogic => refStore += ((l.name, FullType(l.tpe, HwExpressionKind)))
      case p: DefParam => 
        trace(p, s"${p.name}: ${p.kind} ${p.tpe.serialize}")
        refStore += ((p.name, FullType(p.tpe, p.kind)))
      case t: DefType => refStore += ((t.name, FullType(t.tpe, UnknownExpressionKind, true)))
      case f: ForGen =>
        f.init match {
          case na: NamedAssign => refStore += ((na.name, FullType(IntType(na.tokens, NumberDecimal), SwExpressionKind)))
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
    proc.mapType(processType) match {
      case r: Reference => 
        if(refStore.contains(r.name)){
          val tpe = refStore(r.name).tpe.mapInterval(_ => r.tokens)
          refStore(r.name).tpeRef match {
            case true => TypeInst(r.tokens, tpe, Some(r.name), HwExpressionKind, UnknownFlow)
            case false => r.copy(tpe = processType(tpe), kind = refStore(r.name).kind)
          }          
        } else {
          warn(r, s"Undeclared reference ${r.name}")
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
      case exp: Expression => exp
    }
  }
  
  def processType(t: Type)(implicit refStore: RefStore): Type = {
    trace(t, s"Entering processType for ${t.getClass.getName}: ${t.serialize}")
    t.mapType(processType).mapWidth(_.mapExpr(processExpression)) match {
      case u: UserRefType =>  
        if(!refStore.contains(u.name)){
          critical(u, s"Undeclared type ${u.name}")
          u
        } else {
          val tpe = refStore(u.name).tpe.mapInterval(_ => u.tokens)
          u.copy(tpe = processType(tpe)) // could be optimized with some cache system ?
        }
      case tpe => tpe
    }
  }
  
  def processAssignSeq(assign: Seq[Assign], ref: Seq[(String, Type)]): Seq[Assign] = {
    // process ports
    val nna = assign.collect({case nna:NoNameAssign => nna})
    val na = assign.collect({case na:NamedAssign => na})
    val aa = assign.collect({case aa:AutoAssign => aa})
    (aa, nna, na) match {
      case (Seq(), Seq(), Seq()) => assign
      case (aa, Seq(), Seq()) => critical(aa.head, s"Unsupported auto-assign"); assign
      case (Seq(), nna, Seq()) =>
        if(nna.length != ref.length){
          critical(nna.head, s"Unnamed assignation list length (${nna.length}) mismatches declaration length (${ref.length})")
          assign
        } else {
          nna.zip(ref).map({case (a, (n, t)) => a.copy(remoteName = Some(n), remoteType = Some(Utils.cleanTok(t)))})
        }
      case (Seq(), Seq(), na) =>
        val refM = ref.toMap.mapValues(v => Some(Utils.cleanTok(v)))
        na.map(a => {a.copy(remoteType = refM.getOrElse(a.name, None))})
        
      case _ => critical(assign.head, s"Unsupported mixed assign methods"); assign
    }
  }
  
  def processStatement(s: Statement)(implicit refStore: RefStore): Statement = {
    trace(s, s"Entering processStatement for ${s.getClass.getName}")
    s.mapStmt(processStatement).mapExpr(processExpression).mapType(processType) match {
      case i: DefInstance =>
        // let's fetch remote module if known
        currentProject.get.findModule(i.module.serialize) match {
          case Some(m) => 
            val portMap = processAssignSeq(i.portMap, m.ports.map(p => (p.name, p.tpe)))
            val paramMap = processAssignSeq(i.paramMap, m.params.map(p => (p.name, p.tpe)))
            i.copy(portMap = portMap, paramMap = paramMap)

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
    
    forceRefsRefresh() // be sure to refresh refs for upcoming package imports
    // propagate to local refs & record them
    p.copy(refs = Some(refs), body = processStatement(p.body))
  }
  
  /**
   * Processing Module References
   */
  def processModule(m: Module): Module = {
    currentProject.get.clearDescriptionCache() // ensure that we have latest results
    implicit val ref2Type = new RefStore() 
    ref2Type ++= remoteRefs
    
    //FIRST PASS => fill ref2Type    
    m.foreachStmt(visitStatement)
    m.foreachPort(p => ref2Type += ((p.name, FullType(p.tpe, HwExpressionKind))))
    m.foreachParam(p => ref2Type += ((p.name, FullType(p.tpe, p.kind))))
    
    // SECOND PASS => use ref2Type to fill reference type
    m.copy(body = processStatement(m.body))
  }
}