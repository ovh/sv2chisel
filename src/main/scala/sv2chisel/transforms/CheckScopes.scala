// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashMap, ArrayBuffer}

class CheckScopes(val llOption: Option[logger.LogLevel.Value] = None) extends DescriptionBasedTransform {
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
          critical(r, s"Undeclared reference ${r.name} in current scope, scala will not compile")
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
  
  def processStatement(s: Statement, refStore: RefStore): Statement = {
    // record
    implicit val current = refStore
    s match {
      case l: DefLogic => 
        refStore += ((l.name, FullType(l.tpe, HwExpressionKind)))
        l.mapExpr(processExpression).mapType(processType)
        
      case p: DefParam => 
        refStore += ((p.name, FullType(p.tpe, p.kind)))
        p.mapExpr(processExpression).mapType(processType)
        
      case t: DefType => 
        refStore += ((t.name, FullType(t.tpe, HwExpressionKind)))
        t.mapExpr(processExpression).mapType(processType)
        
      case f: ForGen =>
        f.init match {
          case na: NamedAssign => refStore += ((na.name, FullType(IntType(na.tokens, NumberDecimal), SwExpressionKind)))
          case _ => 
        }
        val localStore = new RefStore()
        localStore ++= refStore
        f.mapExpr(processExpression).mapType(processType).mapStmt(processStatement(_, localStore))
        
      case i: IfGen =>
        val localStore = new RefStore()
        localStore ++= refStore
        i.mapExpr(processExpression).mapType(processType).mapStmt(processStatement(_, localStore))
        
      case stmt => stmt.mapExpr(processExpression).mapType(processType).mapStmt(processStatement(_, refStore))
    }
  }
  
  /**
   * Processing Packages References
   */
  def processPackage(p: DefPackage): DefPackage = {
    val refs = new RefStore() 
    refs ++= remoteRefs
    // propagate to local refs & record them
    p.copy(refs = Some(refs), body = processStatement(p.body, refs))
  }
  
  /**
   * Processing Module References
   */
  def processModule(m: Module): Module = {
    val ref2Type = new RefStore() 
    ref2Type ++= remoteRefs
       
    m.foreachPort(p => ref2Type += ((p.name, FullType(p.tpe, HwExpressionKind))))
    m.foreachParam(p => ref2Type += ((p.name, FullType(p.tpe, SwExpressionKind))))
    m.copy(body = processStatement(m.body, ref2Type))
    
  }
}