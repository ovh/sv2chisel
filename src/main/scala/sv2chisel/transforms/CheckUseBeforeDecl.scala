// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashMap, ArrayBuffer}

class CheckUseBeforeDecl(val llOption: Option[logger.LogLevel.Value] = None) extends DescriptionBasedTransform {
  implicit lazy val srcFile = currentSourceFile
  implicit lazy val stream = currentStream
  
  def processDescription(d: Description): Description = {
    d match {
      case m: Module => processModule(m)
      case p: DefPackage => processPackage(p)
      case d => d
    }
  }
  
  // Common functions
  
  def processExpression(e: Expression)(implicit refStore: RefStore): Expression = {
    e.mapExpr(processExpression).mapType(processType) match {
      case r: Reference => 
        if(!refStore.contains(r.name)){
          critical(r, s"Undeclared reference ${r.name} at current point, scala will not compile")
        }
        r
      case exp: Expression => exp
    }
  }
  
  def processType(t: Type)(implicit refStore: RefStore): Type = {
    t.mapType(processType).mapWidth(_.mapExpr(processExpression)) match {
      case u: UserRefType =>  
        if(!refStore.contains(u.name)){
          critical(u, s"Undeclared type ${u.name} in current scope, scala will not compile")
        }
        u
      case tpe => tpe
    }
  }
  
  def processStatement(s: Statement)(implicit refStore: RefStore): Statement = {
    // record
    s match {
      case l: DefLogic => refStore += ((l.name, FullType(l.tpe, HwExpressionKind)))
      case p: DefParam => refStore += ((p.name, FullType(p.tpe, p.kind)))
      case t: DefType => refStore += ((t.name, FullType(t.tpe, HwExpressionKind)))
      case f: ForGen =>
        f.init match {
          case na: NamedAssign => refStore += ((na.name, FullType(IntType(na.tokens, NumberDecimal), SwExpressionKind)))
          case _ => 
        }
      case _ => 
    }
    // visit
    s.mapStmt(processStatement).mapExpr(processExpression).mapType(processType)
  }
  
  /**
   * Processing Packages References
   */
  def processPackage(p: DefPackage): DefPackage = {
    implicit val refs = new RefStore() 
    refs ++= remoteRefs
    // propagate to local refs & record them
    p.copy(refs = Some(refs), body = processStatement(p.body))
  }
  
  /**
   * Processing Module References
   */
  def processModule(m: Module): Module = {
    implicit val ref2Type = new RefStore() 
    ref2Type ++= remoteRefs
    
    //FIRST PASS => fill ref2Type    
    m.foreachPort(p => ref2Type += ((p.name, FullType(p.tpe, HwExpressionKind))))
    m.foreachParam(p => ref2Type += ((p.name, FullType(p.tpe, SwExpressionKind))))
    m.copy(body = processStatement(m.body))
    
    // SECOND PASS => use ref2Type to fill reference type
  }
}