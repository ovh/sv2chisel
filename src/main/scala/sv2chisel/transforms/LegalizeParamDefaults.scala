// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashSet, ArrayBuffer, HashMap, LinkedHashMap}

/** Legalize PARAM value (cannot contain reference => wont compile)
  * Move those paramaters such that param can be options 
  * None => local param takes
  * suggest simpler logic as comment
  * Provide convert override_auto_computed_<param_name> as Option[Type] = None + warn bad practice 
  */
class LegalizeParamDefaults(val options: TranslationOptions) extends DefModuleBasedTransform {
  
  object Action extends Enumeration {
    val comment, overrideOption, move, ignore = Value
    
    // be sure to centralize the remaning function
    def renameOverride(p: String) = s"override_computed_$p"
  }
  
  case class ParamDef(name: String, legal: Boolean)
  class ParamUse(name : String, params: Seq[ParamDef]) {
    // counting is useful to support unamed assigns
    case class ParamRemoteUse(legal: Boolean, count: Int)
    private val store = LinkedHashMap[String, ParamRemoteUse](params.map(p => p.name -> ParamRemoteUse(p.legal, 0)):_*)
    
    private var remoteInst = 0
    
    def registerInst: Unit = remoteInst += 1
    
    def register(p: String): Unit = {
      store.get(p) match {
        case Some(v) => store(p) = v.copy(count = v.count + 1)
        case None => fatal(s"Ignoring usage registration of undefined parameter $p of module ${name}")
      }
    }
    def register(i: Int): Unit = if(i < params.length) register(params(i).name) else {
      fatal(s"Ignoring usage registration of parameter #$i among the ${params.length} of module ${name}")
    }
    
    // core decision logic
    def getAction(p: String): Action.Value = {
      import LegalizeParamDefaultOptions.LegalizeMethod
      store.get(p) match {
        case None =>
          fatal(s"Cannot provide action for undefined parameter $p of module ${name}")
          Action.ignore
          
        case Some(use) => 
          options.legalizeParamDefault.legalizeMethod match {
            case LegalizeMethod.comment => if(!use.legal) Action.comment else Action.ignore
            case LegalizeMethod.overrideOption => if(!use.legal) Action.overrideOption else Action.ignore
            
            case LegalizeMethod.moveOrComment if(use.legal) => Action.ignore
            case LegalizeMethod.moveOrComment if(use.count == 0 && remoteInst > 0) => Action.move
            case LegalizeMethod.moveOrComment => Action.comment
            
            case LegalizeMethod.moveOrOverride if(use.legal) => Action.ignore
            case LegalizeMethod.moveOrOverride if(use.count == 0 && remoteInst > 0) => Action.move 
            case LegalizeMethod.moveOrOverride => Action.overrideOption
          }
      }
    }
    def getAction(i: Int): (Action.Value, String) = {
      if(i < params.length) {
        (getAction(params(i).name), params(i).name)
      } else {
        fatal(s"Attempting to get parameter #$i among the ${params.length} of module ${name}")
        (Action.ignore, "")
      }
    }
    
    
    def dumpUsage: String = {
      val str = ArrayBuffer[String]()
      str += s"Dumping use of parameters for module ${name}"
      store.foreach { case (name, count) => str += s"  | $name -> $count"}
      str.mkString("\n")
    }
    
  }
  
  
  
  
  // 1 - registering use of parameters (DefInstance)
  // 2 - convert locally & update remotely (can be done at the same time as soon as decision does not depend on context)
  
  val modParamStore = new HashMap[String, ParamUse]()
  override val preprocessDescription = Some(((d: Description) => {
    d match {
      // required because some modules will be processed in advance
      case m: DefModule if(!modParamStore.contains(m.name)) => 
        modParamStore += m.name -> preprocessModule(m)
        m
      case d => d
    }
  }))
  
  def preprocessModule(m: DefModule): ParamUse = {
    val knownParams = new HashSet[String]()
    
    def isLegalExpression(e: Expression): Boolean = {
      var allLegalSubExprs = true
      e.foreachExpr( expr => allLegalSubExprs &= isLegalExpression(expr))
      
      allLegalSubExprs && (e match {
        case Reference(_, name, Seq(), _, _, _) => !knownParams.contains(name)
        case _:Reference => true // this is fine => undefined remote ref not to be legalized
        case _ => 
          var allLegalSubTypes = true
          e.foreachType(_.foreachWidth(w => allLegalSubTypes &= isLegalExpression(w.expr)))
          allLegalSubTypes
      })
    }
    
    def preprocessStatement(s: Statement): Unit = {
      s match {
        case i: DefInstance => 
          val storeOption = modParamStore.get(i.module.serialize) match {
            case Some(s) => Some(s)
            case None => 
              currentProject.get.findModule(i.module.serialize) match {
                case Some(m) => 
                  // need to process in advance
                  debug(i, s"Preprocessing module ${m.name} in advance")
                  val store = preprocessModule(m)
                  modParamStore += m.name -> store
                  Some(store)
                case None => None
              }
          }
          storeOption match {
            case Some(store) => 
              store.registerInst
              i.paramMap.zipWithIndex.foreach { case (a, index) => {
                a match {
                  case na: NamedAssign => store.register(na.name)
                  case _: NoNameAssign => store.register(index)
                  case _ =>
                }
              }}
              
            case _ => // no warning required because this module won't be emitted
          }

        case _ => 
      }
      s.foreachStmt(preprocessStatement)
    }
    m.foreachStmt(preprocessStatement)
    
    val defs = m.params.map(p => {
      knownParams += p.name
      ParamDef(p.name, p.value.map(isLegalExpression(_)).getOrElse(true))
    })
    
    new ParamUse(m.name, defs)
  }
  
  def processModule(m: DefModule): DefModule = {
    val ui = UndefinedInterval
    val stmts = ArrayBuffer[Statement]()
    val assigns = new HashMap[String, Expression]()
    
    def overrideOption(p: DefParam, name: String): DefParam = {
      p.value match {
        case None => p
        case Some(v) =>  
          // do whatever is needed to legalize this default port value
          val warning = s"Legalizing ugly verilog syntax: ${p.name} in module $name has a default value which contains (at least) one reference to another parameter."
          warn(p, warning)
          val msg = warning + (if(stmts.isEmpty) s"\n[HINT] Such dependent default values for class instantiations are not allowed in scala and must be adapted. \nIMPORTANT: Please note that you most likely do not want to do that in chisel since you can compute some `localparam` before specifying IOs Widths. \n           This 1-1 legalization should be refactored and shall not serve as syntax example!" else " (Please see [HINT] above.)")
          val autoName = Action.renameOverride(p.name)
          // warning to be inserted as comment as well 
          stmts += Comment(ui, "[WARNING] " + msg)
          val init = RawScalaExprWrapper(v.tokens, s"$autoName.getOrElse(%e)", Seq(v), p.kind, p.tpe)
          stmts += DefParam(ui,NoVerilogAttribute, p.name, p.tpe, Some(init), p.kind)
          assigns += p.name -> init
          
          p.copy(name = autoName, value = Some(RawScalaExpression("None", SwExpressionKind)), tpe = OptionType(p.tpe))
      }
    }
    
    def comment(p: DefParam, name: String): DefParam = {
      p.value match {
        case None => p
        case Some(v) =>  
            // simply add a ??? with comment : will work if never used
            warn(p, s"Parameter ${p.name} in ${name} has a default value which contains references to other parameters. Commenting the value out: execution will crash if no value is passed at instantiation")
            
            p.copy(value = Some(RawScalaExprWrapper(v.tokens,"??? /* %e */", Seq(v), p.kind, p.tpe)))
      }
    }
    
    def legalizeParam(store: ParamUse, p: DefParam, name: String): Option[DefParam] = {
      p.value match {
        case None => Some(p)
        case Some(v) =>  
          store.getAction(p.name) match {
            case Action.ignore => Some(p)
            case Action.overrideOption => 
              info(p, s"Override parameter ${p.name} in $name")
              Some(overrideOption(p, name))
            case Action.comment => 
              info(p, s"Commenting parameter ${p.name} in $name")
              Some(comment(p, name))
            case Action.move => 
              info(p, s"Moving parameter ${p.name} in $name")
              stmts += p
              assigns += p.name -> v // for blackboxes
              None
          }
      }
    }
    
    def processStatement(s: Statement): Statement = {
      s.mapStmt(processStatement) match {
        case i: DefInstance => 
           modParamStore.get(i.module.serialize) match {
            case Some(store) => 
              val paramMap = i.paramMap.zipWithIndex.map { case (a, index) => {
                val (action, name) = a match {
                  case na: NamedAssign => (store.getAction(na.name), na.name)
                  case _: NoNameAssign => store.getAction(index)
                  case _ => (Action.ignore, "")
                }
                (action, a) match {
                  case (Action.overrideOption, na: NamedAssign) => 
                    val newName = Action.renameOverride(na.name)
                    val newValue = RawScalaExprWrapper(ui, "Some(%e)", Seq(na.expr), na.expr.kind, na.expr.tpe)
                    info(i, s"Renaming parameter call of module ${i.module.serialize} from ${na.name} to $newName")
                    na.copy(name = newName, expr = newValue)
                  case (Action.move, _) => 
                    fatal(a, s"Parameter $name has been removed from params of module ${i.module.serialize} while it is used in instance ${i.name}")
                    a
                  case _ => a
                }
              }}
              i.copy(paramMap = paramMap)
              
            case _ =>  
              warn(i, s"Cannot find module ${i.module.serialize} in current project")
              i
          }

        case st => st
      }
    }
    
    // case of blackboxes:
    // 1. override option 
    // - def param renamed and transformed to option
    // - param map expression = auto_<>.getOrElse(<value>)
    val ctx = m match {
      case _: Module => s"module ${m.name}"
      case _: ExtModule => s"blackbox ${m.name}"
    }
    
    val params = modParamStore.get(m.name) match {
      case Some(store) => m.params.flatMap(legalizeParam(store, _, ctx))
      case _ =>
        fatal(m, s"Cannot find local storage for module ${m.name}")
        m.params
    }
    val body = (m.body, stmts.toSeq) match {
      case (b: Statement, Seq()) => b
      case (s: Block, st) => s.prependStmts(st)
      case (s: Statement, st) => SimpleBlock(s.tokens, st ++ Seq(s))
    }
    
    m match {
      case mod: Module => mod.copy(params = params, body = body).mapStmt(processStatement)
      case e:ExtModule => 
        val paramMap = m.params.map(p => {
          assigns.get(p.name) match {
            case Some(e) => NamedAssign(ui, p.name, e, SourceFlow)
            case _ => NamedAssign(ui, p.name, Reference(ui, p.name, Seq(), p.tpe, p.kind), SourceFlow)
          }
        })
        e.copy(params = params, body = body, paramMap = paramMap)
    }
  }
}