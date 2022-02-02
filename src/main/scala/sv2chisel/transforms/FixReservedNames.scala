// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._


/** only fixes reset for now until a proper reset inference */
class FixReservedNames(val options: TranslationOptions) extends DescriptionBasedTransform {
  implicit var srcFile = currentSourceFile
  implicit var stream = currentStream
  
  implicit def stringToSnake(s: String) = new SnakeString(s)
  
  def processDescription(d: Description): Description = {
    d match {
      case m: Module => m.copy(body = processStatement(m.body))
      case d => d
    }
  }
  
  def processExpression(e: Expression): Expression = {
    def processRef(r: Reference): Reference = r.path match {
      case Seq() if(r.name == "reset") => r.copy(name = "ureset") 
      case _ => r 
    }
    e match {
      case a: Assign => processAssign(a)
      case _ =>
        e.mapExpr(processExpression).mapType(processType).mapWidth(_.mapExpr(processExpression)) match {
          case r: TypeInst if(r.name.map(_ == "reset").getOrElse(false)) => r.copy(name = Some("ureset")) 
          case r: Reference => processRef(r)
          case c: DoCall => c.copy(fun = processRef(c.fun))
          case exp => exp
        }
    }
  }
  
  def processAssign(a: Assign): Assign = {
    a.mapExpr(processExpression) match {
      case na: NamedAssign => na.copy(assignExpr = na.assignExpr.map(processExpression))
      case na: NoNameAssign => na.copy(assignExpr = na.assignExpr.map(processExpression))
      case aa => aa
    }
  }
  
  def processType(t: Type): Type = {
    t.mapType(processType).mapWidth(_.mapExpr(processExpression)) match {
      case u: UserRefType if(u.name == "reset") => u.copy(name = "ureset") 
      case tpe => tpe 
    }
  }
  
  def processStatement(s: Statement): Statement = {
    s match {
      case i: DefInstance => 
        val name = if (i.name == "reset") "ureset" else i.name
        i.copy(name = name, portMap = i.portMap.map(processAssign(_)))
        
      case _ => 
        s.mapStmt(processStatement).mapExpr(processExpression).mapType(processType) match {
          case l: DefLogic if (l.name == "reset") => l.copy(name = "ureset")
          case p: DefParam if (p.name == "reset") => p.copy(name = "ureset")
          case t: DefType if (t.name == "reset") => t.copy(name = "ureset")
          case f: DefFunction if (f.name == "reset") => f.copy(name = "ureset")
          case p: Port if (p.name == "reset") => p.copy(name = "ureset")
          case st => st 
        }
    }
  }
}