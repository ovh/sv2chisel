// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashSet, ArrayBuffer}

/** Legalize PARAM value (cannot contain reference => wont compile)
  * Move those paramaters such that param can be options 
  * None => local param takes
  * suggest simpler logic as comment
  * Provide convert override_auto_computed_<param_name> as Option[Type] = None + warn bad practice 
  */
class LegalizeParamDefaults(val llOption: Option[logger.LogLevel.Value] = None) extends DefModuleBasedTransform {
  
  def processModule(m: DefModule): DefModule = {
    val stmts = ArrayBuffer[Statement]() 
    val knownParams = new HashSet[String]()
    
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
        case _ => 
          var allLegalSubTypes = true
          e.foreachType(_.foreachWidth(w => allLegalSubTypes &= isLegalExpression(w.expr)))
          allLegalSubTypes
      })
    }
    
    def processParam(p: DefParam, name: String): DefParam = {
      p.value match {
        case None => p
        case Some(v) =>  
          isLegalExpression(v) match {
            case true => p
            case false => 
              // do whatever is needed to legalize this default port value
              val warning = s"Legalizing ugly verilog syntax: ${p.name} in module $name has a default value which contains (at least) one reference to another parameter."
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
              val init = RawScalaExprWrapper(v.tokens,template, Seq(v), SwExpressionKind)
              stmts += DefParam(UndefinedInterval,NoVerilogAttribute, p.name, p.tpe, Some(init), p.kind)
              
              val tpe = p.tpe match {
                case _: StringType => RawScalaType("Option[String]") 
                case _ => RawScalaType("Option[Int]") 
              }
              p.copy(name = autoName, value = Some(RawScalaExpression("None", SwExpressionKind)), tpe = tpe)
          }
      }
    }
    
    def processExtParam(p: DefParam, name: String): DefParam = {
      p.value match {
        case None => p
        case Some(v) =>  
          isLegalExpression(v) match {
            case true => p
            case false => 
              // do whatever is needed to legalize this default port value
              warn(p, s"Parameter ${p.name} in blackbox ${name} has a default value which contains references to other parameters. Commenting the value out: execution will crash if no value is passed at instantiation")
              
              p.copy(value = Some(RawScalaExprWrapper(v.tokens,"??? /* %e */", Seq(v), p.kind)))
          }
      }
    }
      
    m.foreachParam(p => knownParams += (p.name))
    m match {
      case mod: Module =>
        // SINGLE PASS
        mod.mapParam(processParam(_, mod.name)).copy(body = (mod.body, stmts.toSeq) match {
          case (b: Statement, Seq()) => b
          case (s: Block, st) => s.prependStmts(st)
          case (s: Statement, st) => SimpleBlock(s.tokens, st ++ Seq(s))
        })
      case e: ExtModule => e.mapParam(processExtParam(_, e.name))
    }
  }
}