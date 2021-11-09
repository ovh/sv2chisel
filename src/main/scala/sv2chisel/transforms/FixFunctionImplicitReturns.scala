// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

/** 
  * 
  */
class FixFunctionImplicitReturns(val llOption: Option[logger.LogLevel.Value] = None) extends DescriptionBasedTransform {

  private def processFunction(f: DefFunction): DefFunction = {
    // first pass on function body: lookup connect with function name
    var matchingConnects = 0
    var lastIsConnect = false
    
    def visitStatement(s: Statement): Unit = {
      s match {
        case Connect(_,_,Reference(_,n,_,_,_,_),_,_,_) if(f.name == n) => 
          matchingConnects += 1
          lastIsConnect = true
          
        case _ => lastIsConnect = true
      }
      s.foreachStmt(visitStatement) 
    }
    f.foreachStmt(visitStatement)
    
    debug(f, s"Function `${f.name}` analysis: matching connects:$matchingConnects; lastIsConnect:$lastIsConnect")
    

    def removeLastImplicit(s: Statement): Statement = {
      s match {
        case Connect(_,_,Reference(_,n,_,_,_,_),e,_,_) if(f.name == n) => ExpressionStatement(e)
        case _ => s.mapStmt(removeLastImplicit) 
      }
    }
    
    // second pass on function body:
    // either remove the last implicit statement
    // or add proper declaration for explicit intermediate connection
    if(matchingConnects == 1 && lastIsConnect){
      f.mapStmt(removeLastImplicit)
    } else {
      val ue = UndefinedExpression()
      val returnValueDef = DefLogic(UndefinedInterval, NoVerilogAttribute, f.name, f.tpe, ue, ue, ue, LogicWire)
      val finalReturn = ExpressionStatement(Reference(UndefinedInterval, f.name, Seq(), f.tpe, f.kind, SourceFlow))
        
      val body = f.body match {
        case b: SimpleBlock => b.copy(stmts = returnValueDef +: b.stmts :+ finalReturn) 
        case s => SimpleBlock(UndefinedInterval, Seq(returnValueDef, s, finalReturn))
      }
      f.copy(body = body)
    }
  }
  
  
  def processDescription(d: Description): Description = {
    def processStatement(s: Statement): Statement = {
      s match {
        case f: DefFunction => processFunction(f)
        case _ => s.mapStmt(processStatement) 
      }
      
    }
    
    d match {
      case m: Module => m.mapStmt(processStatement)
      case p: DefPackage => p.mapStmt(processStatement)
      case _ => d
    }
  }
  
}