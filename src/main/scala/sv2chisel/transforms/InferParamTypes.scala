// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashMap}

class InferParamTypes(val llOption: Option[logger.LogLevel.Value] = None) extends DefModuleBasedTransform {
  
  class UsageStore() {
    private val intUsageCount = new HashMap[String, Int]()
    private val booleanUsageCount = new HashMap[String, Int]()
    // private val convertedToBoolean = new HashSet[String]()

    
    def registerIntUsage(ref: String): Unit = {
      val count = intUsageCount.getOrElse(ref, 0) + 1
      intUsageCount.update(ref, count)
    }
    def registerBoolUsage(ref: String): Unit = {
      val count = booleanUsageCount.getOrElse(ref, 0) + 1
      booleanUsageCount.update(ref, count)
    }

    def doConvertToBoolean(p: DefParam): DefParam = {
      ??? // TO DO
    } 
    
  }
  
  
  def processModule(m: DefModule): DefModule = {
    // 
    // // first pass collect param usages 
    // def visitPort(p: Port): Port = {
    // 
    // }
    // 
    // m.foreachPort(visitPort)
    // m.foreachStmt(visitStmt)
    // 
    
    // SECOND PASS : Take decisions 
    def getType(name: String, e: Expression): Type = {
      e match {
        // note : might need better merge strategy, allowing some UnknownTypes
        // it will currently fail for a string param + ref 
        case r: Reference => 
          // this will be caught afterwards in LegalizeParamDefaults 
          // critical(e, s"Default param value $name contains some reference ($r) this is likely to result into illegal scala...") 
          r.tpe match {
            case _: UnknownType => IntType(UndefinedInterval, NumberDecimal)
            case _ => debug(e, s"Known type for parameter $name = ${r.tpe} "); r.tpe
          }
        
        case _: Number => IntType(UndefinedInterval, NumberDecimal)
        case _: StringLit => StringType(UndefinedInterval, UnknownWidth())
        
        case DoPrim(_, _: PrimOps.InlineIf, args, _, _) => args match {
          case Seq(cond@_, conseq, alt) => 
            val a = getType(name, conseq)
            val b = getType(name, alt)
            if (a != b) Utils.throwInternalError(s"Unable to infer type for param $name")
            a
            
          case _ => Utils.throwInternalError(s"Unexpected number of arguments (${args.size}) for primop InlineIf while 3 were expected ")
        }
        case DoPrim(_, _, args, _, _) => args.map(a => getType(name, a)) match {
          case Seq() => IntType(UndefinedInterval, NumberDecimal)
          case Seq(t) => t 
          case s => s.reduce((a, b) => { if(a != b) Utils.throwInternalError(s"Unable to infer type for param $name"); a })
        }
          
        case _ => 
          warn(e, s"Unable to infer proper type for param $name. (resolved as Int)") 
          IntType(UndefinedInterval, NumberDecimal)
      }
    }
    
    def processParam(p: DefParam): DefParam = {
      (p.tpe, p.value) match {
        case (t: UnknownType, None ) => p.copy(tpe = IntType(t.tokens, NumberDecimal))
        case (_: UnknownType, Some(v) ) =>  p.copy(tpe = getType(p.name, v))
          
        case _ => p
      }
    }
    
    m.mapParam(processParam)
  }
}