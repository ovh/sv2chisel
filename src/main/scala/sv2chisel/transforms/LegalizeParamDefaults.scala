// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashSet, ArrayBuffer}

// PASS TO DO : legalize PARAM value (cannot contain reference => wont compile)
// Move those paramaters such that param can be options 
// None => local param takes
// suggest simpler logic as comment

// TO DO : chiselize module with default parameters using companion objects
// NOTE : hash_table has too many args for such an automated approach allowing to provide any combination of args => 1 << N !!! =>  N = 21 for hash table 
// Might be an approach to be enforced in project mode 
// => remove default for parameter that are always set
// => provide companion corresponding to actually used 


// for now only provide convert override_auto_computed_<param_name> as Option[Type] = None + warn bad practice 
// TODO??? + provide companion object without default with original names for easy override 

class LegalizeParamDefaults(val llOption: Option[logger.LogLevel.Value] = None) extends DefModuleBasedTransform {
  // TO DO : add proper error management within passes
    
  def processModule(m: DefModule): DefModule = {
    val stmts = ArrayBuffer[Statement]() 
    val knownParams = new HashSet[String]()
    
    // SINGLE PASS
    def isLegalExpression(e: Expression): Boolean = {
      var allLegalSubExprs = true
      e.foreachExpr( expr => allLegalSubExprs &= isLegalExpression(expr))
      
      allLegalSubExprs && (e match {
        case r:Reference if(remoteRefs.contains(r)) => true // remote ref in scope
        case Reference(_, name, Seq(), _, _, _) => 
          if (knownParams.contains(name)) {
            false
          } else {
            critical(e, "Unknown local reference used as default param, this will likely not pass scala compilation")
            true
          }
          
        case _:Reference => true // this is fine => undefined remote ref not to be legalized
        case _ => true
      })
    }
    
    def processParam(p: DefParam): DefParam = {
      p.value match {
        case None => p
        case Some(v) =>  
          isLegalExpression(v) match {
            case true => p
            case false => 
              // do whatever is needed to legalize this default port value
              val warning = s"Legalizing ugly verilog syntax: ${p.name} has a default value which contains (at least) one reference to another parameter."
              warn(p, warning)
              val msg = warning + (if(stmts.isEmpty) s"\n[HINT] Such dependent default values for class instantiations are not allowed in scala and must be adapted. \nIMPORTANT: Please note that you most likely do not want to do that in chisel since you can compute some `localparam` before specifying IOs Widths. \n           This 1-1 legalization should be refactored and shall not serve as syntax example!" else " (Please see [HINT] above.)")
              val autoName = "override_auto_computed_" + p.name
              // warning to be inserted as comment as well 
              stmts += Comment(UndefinedInterval, "[WARNING] " + msg)
              val template = s"""$autoName match {
              |  case None => %e
              |  case Some(v) => v
              |}
              | """.stripMargin
              val init = RawScalaExprWrapper(v.tokens,template, Seq(v))
              stmts += DefParam(UndefinedInterval,NoVerilogAttribute, p.name, p.tpe, Some(init), p.kind)
              
              val tpe = p.tpe match {
                case _: StringType => RawScalaType("Option[String]") 
                case _ => RawScalaType("Option[Int]") 
              }
              p.copy(name = autoName, value = Some(RawScalaExpression("None", SwExpressionKind)), tpe = tpe)
          }
      }
    }
    m.foreachParam(p => knownParams += (p.name))
    
    
    // WARNING : handle the added statement pre-io
    val mod = m.mapParam(processParam).asInstanceOf[Module]
    mod.copy(body = (mod.body, stmts.toSeq) match {
        case (b: Statement, Seq()) => b
        case (s: Block, st) => s.prependStmts(st)
        case (s: Statement, st) => SimpleBlock(s.tokens, st ++ Seq(s))
      }
    )
  }
}