// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

class CheckUseBeforeDecl(val options: TranslationOptions) extends DescriptionBasedTransform {
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
  
  def visitExpression(e: Expression)(implicit refStore: RefStore): Unit = {
    e.foreachExpr(visitExpression)
    e.foreachType(visitType)
    e match {
      case r: Reference if(!refStore.contains(r)) => 
          critical(r, s"Undeclared reference ${r.serialize} at current point, scala will not compile")
      case _ =>
    }
  }
  
  def visitType(t: Type)(implicit refStore: RefStore): Unit = {
    t.foreachType(visitType)
    t.foreachWidth(_.foreachExpr(visitExpression))
    t match {
      case u: UserRefType if(!refStore.contains(u)) =>  
          critical(u, s"Undeclared type ${u.serialize} in current scope, scala will not compile")
      case _ => 
    }
  }
  
  def visitStatement(s: Statement)(implicit refStore: RefStore): Unit = {
    // record
    processImportStatement(s, refStore) match {
      case l: DefLogic => refStore += ((WRef(l.name), FullType(l.tpe, HwExpressionKind)))
      case p: DefParam => refStore += ((WRef(p.name), FullType(p.tpe, p.kind)))
      case t: DefType => 
        refStore += ((WRef(t.name), FullType(t.tpe, HwExpressionKind)))
        t.tpe match {
          case e: EnumType => e.fields.foreach(f => {
            refStore += ((WRef(f.name), FullType(e.tpe, e.kind))) // weird but seems standard to flatten
            refStore += ((WRef(f.name, Seq(t.name)), FullType(e.tpe, e.kind))) // not sure if used in this way ?
          })
          case _ =>  
        }
        
      case f: DefFunction => refStore += ((WRef(f.name), FullType(f.tpe, HwExpressionKind)))
      case p: Port => refStore += ((WRef(p.name), FullType(p.tpe, HwExpressionKind)))
      case f: ForGen =>
        f.init match {
          case na: NamedAssign => refStore += ((WRef(na.name), FullType(IntType(na.tokens, NumberDecimal), SwExpressionKind)))
          case _ => 
        }
      case _ => 
    }
    // visit
    s.foreachStmt(visitStatement)
    s.foreachExpr(visitExpression)
    s.foreachType(visitType)
  }
  
  /**
   * Processing Packages References
   */
  def processPackage(p: DefPackage): DefPackage = {
    implicit val refs = new RefStore() 
    refs ++= remoteRefs
    // propagate to local refs & record them
    visitStatement(p.body)
    forceRefsRefresh() // must be done after visitStatement due to the import statement side effect
    debug(p, s"Updating refs for package ${p.name}")
    p.copy(refs = Some(refs))
  }
  
  /**
   * Processing Module References
   */
  def processModule(m: Module): Module = {
    implicit val ref2Type = new RefStore() 
    ref2Type ++= remoteRefs
    
    //SINGLE PASS => references shall be registered in ref2Type before use    
    m.foreachParam(p => ref2Type += ((WRef(p.name), FullType(p.tpe, SwExpressionKind))))
    visitStatement(m.body)
    m
  }
}