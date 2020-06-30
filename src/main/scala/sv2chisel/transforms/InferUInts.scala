// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashMap, ArrayBuffer}

class InferUInts(val llOption: Option[logger.LogLevel.Value] = None) extends DefModuleBasedTransform {
  
  class UsageStore() {
    private val arithmeticUsageCount = new HashMap[String, Int]()
    private val stringAssignedCount = new HashMap[String, Int]()
    // simple apply or slice operator translation
    private val indexAccessCount = new HashMap[String, Int]()
    private val rangeAccessCount = new HashMap[String, Int]()
    // much harder conversion > will be avoided in all cases
    // see connect & update implicits within fpga-vac-chisel (PR 20)
    private val indexAssignCount = new HashMap[String, Int]()
    private val rangeAssignCount = new HashMap[String, Int]()
    
    def registerArithUsage(ref: String): Unit = {
      val count = arithmeticUsageCount.getOrElse(ref, 0) + 1
      arithmeticUsageCount.update(ref, count)
    }
    def registerAssignedString(ref: String): Unit = {
      val count = stringAssignedCount.getOrElse(ref, 0) + 1
      stringAssignedCount.update(ref, count)
    }
    def registerIndexAccess(ref: String): Unit = {
      val count = indexAccessCount.getOrElse(ref, 0) + 1
      indexAccessCount.update(ref, count)
    }
    def registerIndexAssign(ref: String): Unit = {
      val count = indexAssignCount.getOrElse(ref, 0) + 1
      indexAssignCount.update(ref, count)
    }
    def registerIndex(ref: String, assign: Boolean): Unit = {
      if (assign) {
        registerIndexAssign(ref)
      } else {
        registerIndexAccess(ref)
      }
    }
    
    def registerRangeAccess(ref: String): Unit = {
      val count = rangeAccessCount.getOrElse(ref, 0) + 1
      rangeAccessCount.update(ref, count)
    }
    def registerRangeAssign(ref: String): Unit = {
      val count = rangeAssignCount.getOrElse(ref, 0) + 1
      rangeAssignCount.update(ref, count)
    }
    def registerRange(ref: String, assign: Boolean): Unit = {
      if (assign) {
        registerRangeAssign(ref)
      } else {
        registerRangeAccess(ref)
      }
    }

    // Note : all the following could be written without closing/ locking the store
    // however it illustrates perfectly well the system with proper use of require & assert
    
    private var closed = false
    private val currentType = new HashMap[String, Type]()
    
    def registerType(ref: String, tpe: Type): Unit = {
      require(!closed, "Cannot register new type when Store is closed") // => user error
      currentType.update(ref, tpe)
    }
    
    def doConvertToUInt(ref: String): Boolean = {
      if(indexAssignCount.contains(ref) || rangeAssignCount.contains(ref)){
        trace(s"Ref $ref is assigned with index or range > no conversion to UInt")
        false
      } else {
        val bitRefs = indexAccessCount.getOrElse(ref, 0) + rangeAccessCount.getOrElse(ref, 0)
        val arithCount = arithmeticUsageCount.getOrElse(ref, 0) 
        trace(s"Ref $ref > bitRefs: $bitRefs ; arithCount: $arithCount")
        arithCount >= bitRefs
      }
    } 
    
    // note unsupported shall not raise error but just do nothing 
    private def uintLeafType(path: String, tpe: Type): Type = {
      tpe match {
        case v: VecType => 
          v.tpe match {
            case Seq(t: BoolType) => 
              // end of recursion let's return an UInt depending on
              if(stringAssignedCount.contains(path)){
                info(tpe, s"Converting $path to Vec of Char(UInt(8.W)) based on its usage in the module")
                v.asCharVecType()
              } else if (doConvertToUInt(path)) {
                info(tpe, s"Converting $path to UInt based on its usage in the module")
                v.asUIntType() 
              } else {
                v
              }

            case Seq(t) => v.mapType(_ => uintLeafType(s"$path[_]",t)) // walk the complex type
            case _ => v // unsupported mixed vec types
          }
        case b: BundleType => 
          b.copy(fields = b.fields.map(f => {
            f.copy(tpe = uintLeafType(s"$path.${f.name}", f.tpe))
          }))
        case _ => tpe // unsupported
      }
    }
    
    def getType(ref: String): Type = {
      uintLeafType(ref, currentType(ref))
    }
    
    def traceIt(): Unit = {
      trace("arithmeticUsageCount: " + arithmeticUsageCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("indexAccessCount: " + indexAccessCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("rangeAccessCount: " + rangeAccessCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("indexAssignCount: " + indexAssignCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("rangeAssignCount: " + rangeAssignCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("currentType: " + currentType.map(t => s"${t._1} -> ${t._2.serialize}").mkString("\n", "\n", "\n"))
    }
  }
    
  def processModule(m: DefModule): DefModule = {
    val store = new UsageStore()
    
    //FIRST PASS => fill store
    // assign : true is the expression is an assignement target
    // returns 
    def visitExpression(e: Expression, assign: Boolean): Option[String] = {
      e.foreachType(visitType)
      e match {
        case r: Reference => Some(r.serialize)
        case s: SubField => 
          s.expr match {
            case r: Reference => Some(s"${r.serialize}.${s.name}")  
            case exp => visitExpression(exp, assign) match {
                case None => None 
                case Some(ref) => Some(s"$ref.${s.name}")
              }
          }
        case s: SubIndex => 
          s.expr match {
            case r: Reference => 
              trace(s, s"registering index ${if(assign) "assign" else "access"} for ${r.serialize}")
              store.registerIndex(r.serialize, assign)
              Some(s"${r.serialize}[_]")
            case exp =>
              visitExpression(exp, assign) match {
                case None => None 
                case Some(ref) => 
                  trace(s, s"registering index ${if(assign) "assign" else "access"} for ${ref}")
                  store.registerIndex(ref, assign)
                  Some(s"$ref[_]")
              }
          }
          
        case s: SubRange => 
          s.expr match {
            case r: Reference => 
              trace(s, s"registering range ${if(assign) "assign" else "access"} for ${r.serialize}")
              store.registerRange(r.serialize, assign)
              Some(s"${r.serialize}[_]")
            case exp =>
              visitExpression(exp, assign) match {
                case None => None 
                case Some(ref) => 
                  trace(s, s"registering range ${if(assign) "assign" else "access"} for ${ref}")
                  store.registerRange(ref, assign)
                  Some(s"$ref[_]")
              }
          }
        
        case p: DoPrim =>
          if(assign) critical(p, "Should not try to assign to a primary op")
          p.args.foreach(a => {
            visitExpression(a, assign) match {
              case None => //
              case Some(ref) => store.registerArithUsage(ref)
            }
          })
          None
        case c: DoCast =>
          if(assign) critical(c, "Should not try to assign to a cast operator")
          visitExpression(c.expr, assign)
          None
        case c: DoCall =>
          if(assign) critical(c, "Should not try to assign to a function call")
          c.args.foreach(visitExpression(_, assign))
          None
        case c: Concat =>
          // could be both way 
          c.args.foreach(visitExpression(_, assign))
          None
        case n: NamedAssign => None // TODO ??? 
        case n: NoNameAssign => None // TODO ???
        
        // Litteral & others
        case _ => None
      }
    }
    
    def visitType(t: Type): Unit = {
      t.foreachWidth(_.foreachExpr(visitExpression(_, false)))
      t.foreachType(visitType)
    }
    
    def visitStatement(s: Statement): Unit = {
      s.foreachType(visitType)
      s match {
        case c: Connect => 
          // (c.loc, c.expr) match {
          //   case (r: Reference, s: StringLit) => store.registerAssignedString(r.serialize)
          //   case _ =>
              visitExpression(c.loc, true)
              visitExpression(c.expr, false)
          // }
          
          
        case l: DefLogic => 
          store.registerType(l.name, l.tpe)
          s.foreachExpr(visitExpression(_, false))
          
        case i: DefInstance => i.portMap.foreach(p => {
          val (flow, exp) = p match {
            case na: NamedAssign => (Some(na.flow), na.expr)
            case na: NoNameAssign => (Some(na.flow), na.expr)
            case _ => (None, UndefinedExpression())
          }
          
          flow match {
            case None => // nothing to do ; no warning
            case Some(SourceFlow) => visitExpression(exp, false)
            case Some(SinkFlow) => visitExpression(exp, true)
            case _ => 
              exp match {
                case _:UndefinedExpression => // useless to warn user
                case _ => 
                  warn(p, s"Unknown flow for port ${p.serialize} of instance ${i.name}. Treating it as potential Source which might prevent its conversion to UInt")
                  visitExpression(exp, true)
              }
          }
        })
        case _ => 
          s.foreachStmt(visitStatement)
          s.foreachExpr(visitExpression(_, false))
      }
    }
    
    m.foreachStmt(visitStatement)
    m.foreachPort(p => store.registerType(p.name, p.tpe))
    
    store.traceIt()
    
    // SECOND PASS => use store to update declaration AND SubIndex / SubRange Usage Expressions
    
    def processStatement(s: Statement): Statement = {
      s match {
        case d: DefLogic => d.copy(tpe = store.getType(d.name))
        case _ => s.mapStmt(processStatement)
      }
    }
    
    def processPort(p: Port): Port = {
      p.copy(tpe = store.getType(p.name))
    }
    
    m.mapPort(processPort).mapStmt(processStatement)
  }
}