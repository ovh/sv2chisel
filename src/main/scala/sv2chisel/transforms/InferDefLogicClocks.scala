// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashMap, HashSet, ArrayBuffer, LinkedHashMap, LinkedHashSet}

/** Resolve logic as wire or reg with appropriate clock (TODO: and reset)
  * Also legalize port reg 
  *   > input reg raise [fatal] (non-sense) (resolved as input wire port)
  *   > output reg are resolved with the generation of an intermediate reg and wire
  */
class InferDefLogicClocks(val options: TranslationOptions) extends DescriptionBasedTransform {
  val ui = UndefinedInterval
  val na = NoVerilogAttribute
  val seenModules = new HashMap[String, DefModule]()
  
  def processDescription(d: Description): Description = {
    d match {
      case m: Module =>       
        if (seenModules.contains(m.name)) {
          debug(m, s"Ignoring multiple processing for module ${m.name}")
          seenModules(m.name)
        } else {
          processModuleCore(m)
        }
        
      case p: DefPackage => processPackage(p)
        
      case _ => d
    }
  }
  
  private def processFunction(f:DefFunction): DefFunction = {
    def processStatement(s: Statement): Statement = {
      s match {
        case l: DefLogic => l.resolution match { // register forbidden in function in verilog
          case LogicRegister(_,_,_,preset) => l.copy(resolution = LogicWire(preset))
          case LogicUnresolved(v) => l.copy(resolution = LogicWire(v))
          case r => l.copy(resolution = r)
        }
        case _ => s.mapStmt(processStatement) 
      }
    }
    f.mapStmt(processStatement)
  }
  
  private def processPackage(p: DefPackage): DefPackage = {
    def processStatement(s: Statement): Statement = {
      s match {
        case f: DefFunction => processFunction(f)
        case _ => s.mapStmt(processStatement) 
      }
      
    }
    p.mapStmt(processStatement)
  }
  
  case class Scope(conds: Seq[Expression]) {
    def rebase(base: Scope): Option[Scope] = {
      base.conds match {
        case Seq() => Some(this)
        case s if(s.length > this.conds.length) => None
        case _ => 
          // check common ancestors
          base.conds.zip(this.conds).filterNot { case (a, b) => Utils.eq(a, b)} match {
            case Seq() =>
              // return scope after latest common ancestor
              Some(Scope(this.conds.drop(base.conds.length)))
            case _ => None 
          }
      }
    }
    def +(cond: Expression): Scope = this.copy(conds = conds :+ cond)
    def !+(cond: Expression): Scope = {
      val not = DoPrim(ui, PrimOps.Not(ui), Seq(cond))
      this.copy(conds = conds :+ not)
    }
    
    def getCondExpr: Expression = {
      conds match {
        case Seq() => DontCare(ui)
        case s => s.reduce((a, b) => DoPrim(ui, PrimOps.And(ui), Seq(a,b)))
      }
    }
  }
  
  case class CondBindingSeq(bindings: Seq[(Expression, Option[Expression])]) {
    def :+(scope: (Expression, Option[String])) = 
      this.copy(bindings :+ scope._1 -> scope._2.map(Reference(ui, _, Seq())))
  }
  
  private def processModuleCore(m: Module): DefModule = {
    val refsInScope = new HashSet[WRef]() 
    refsInScope ++= remoteRefs.keys
    refsInScope ++= m.params.map(p => WRef(p.name))
    
    val def2binding = new HashMap[String, CondBindingSeq]()
    val def2scope = new HashMap[String, Scope]()
    val defIncomplete = new HashSet[String]()
    val clocksUsageCounts = new HashMap[String, Int]()
    def registerClockUsage(clock: String, reason: String): Unit = {
      if (clocksUsageCounts.contains(clock)) {
        clocksUsageCounts(clock) += 1
      } else {
        info(m, s"Registering a new clock `${clock}` for module ${m.name} ($reason)")
        clocksUsageCounts(clock) = 1
      }
    }
    
    var activeClock : Option[String] = None
    var currentScope = Scope(Seq())
    
    // TODO : retrieve reference target system from firrtl
    def getRefs(e: Expression): Seq[Reference] = {
      e match {
        case r: Reference => Seq(r)
        case s: SubField => getRefs(s.expr)
        case s: SubIndex => getRefs(s.expr)
        case s: SubRange => getRefs(s.expr)
        case c: Concat => c.args.flatMap(getRefs)
        case _ => 
          fatal(e, s"Unable to fetch proper reference for ${e.serialize}")
          Seq()
      }
    }
    
    //FIRST PASS => fill reg2clocks

    def getCond(node: SVNode, name: String): Option[Expression] = def2scope.get(name) match {
      case None =>
        critical(node, s"Connection to undeclared logic ${name}")
        None
      case Some(s) => currentScope.rebase(s) match {
        case None =>
          critical(node, s"Structural issue in scopes between declaration and usage of logic ${name}")
          None
        case Some(bindings) => Some(bindings.getCondExpr)
      }
    }
    
    def visitConnect(c: Connect)(implicit usage: HashSet[String]): Connect = {
      (c.continuous, c.blocking, activeClock) match {
        case (false, false, Some(clock)) =>  
          getRefs(c.loc).foreach(r => {
            def2binding.get(r.name) match {
              case None => warn(c, s"Connection to undeclared logic ${r.name}")
              case Some(s) => 
                getCond(c, r.name) match {
                  case Some(e) => 
                    def2binding(r.name) = s :+ e -> Some(clock)
                    usage += r.name
                  case _ => 
                }
                registerClockUsage(clock, "non blocking assignment")
            }
          })
        case _ => assignCombExpr(c, c.loc)
          
      }
      c // nothing to be modified yet
    }
    
    def assignCombExpr(node: SVNode, expr: Expression)(implicit usage: HashSet[String]): Unit = {
      getRefs(expr).foreach(r => {
        def2binding.get(r.name) match {
          case None => warn(node, s"Connection to undeclared logic ${r.name}")
          case Some(s) => 
            getCond(node, r.name) match {
              case Some(e) =>  
                def2binding(r.name) = s :+ e -> None
                usage += r.name
              case _ => 
            }
        }
      })
    }
    
    def visitClockRegion(c: ClockRegion)(implicit usage: HashSet[String]): Block = {
      if(!c.posedge) 
        critical(c,"Only posedge supported for emission (ignored, the resulting chisel won't work)")
        
      activeClock = c.clock match {
        case r: Reference => Some(r.name)
        case c => critical(c,s"Weird clock dude! ${c.serialize} (ignored, the resulting chisel won't work)") ; None
      }
      // Mapping Statements
      val reg = c.mapStmt(visitStatement)
      activeClock = None
      
      // Removing Always @ clock region for simple simple blocks
      reg.toSimpleBlock()
    }
    
    def visitInstance(i: DefInstance)(implicit usage: HashSet[String]): DefInstance = {
      val instModule = if(seenModules.contains(i.module.serialize)) {
        Some(seenModules(i.module.serialize))
      } else {
        currentProject.get.findModule(i.module.serialize) match {
          case Some(d) =>
            info(i, s"Processing in advance module ${i.module.serialize} instanciated as ${i.name} in current module ${m.name}.")
            val stream = currentStream
            val src = currentSourceFile
            updateContext(d.name)
            val res = Some(processDescription(d))
            currentStream = stream
            currentSourceFile = src
            res
          case None =>
              warn(i, s"Unable to find module module ${i.module.serialize} instanciated as ${i.name} in current module ${m.name} for clock inference processing.")
              None
        }
      }
      instModule match {
        case Some(d: DefModule) =>
          val clock = d.ports.zipWithIndex.collectFirst{
            case (Port(_,_,_,_,_,_,Some(n),_),p) => (n,p)
          }.getOrElse(("<unknown>", -1))
          
          val mappedDir = d.ports.map(p => p.name -> p.direction).toMap
          i.portMap.zipWithIndex.foreach(p => p._1 match {
            case na: NamedAssign => 
              if(na.name == clock._1) registerClockUsage(na.expr.serialize, "named port map to clocked submodule")
              (mappedDir.get(na.name), na.expr) match {
                case (_, _:UndefinedExpression) => // unconnected remote
                case (Some(_:Output), expr) => assignCombExpr(na, expr) 
                case _ => 
              }
              
            case na: NoNameAssign =>
              if(p._2 == clock._2) registerClockUsage(na.expr.serialize, "sequential port map to clocked submodule")
              (d.ports.lift(p._2).map(_.direction), na.expr) match {
                case (_, _:UndefinedExpression) => // unconnected remote
                case (Some(_:Output), expr) => assignCombExpr(na, expr) 
                case _ => 
              }

            case _ => 
          })
          i
        case _ => i // nothing to do 
      }
      
    }
    
    def visitStatement(s: Statement)(implicit usage: HashSet[String]): Statement = {
      s match {
        case c: ClockRegion => visitClockRegion(c)
        case c: Connect => visitConnect(c)
        case i: DefInstance => visitInstance(i)
        case p: Port => 
          def2binding += p.name -> CondBindingSeq(Seq())
          def2scope += p.name -> currentScope
          p
        
        case l: DefLogic => 
          def2binding += l.name -> CondBindingSeq(Seq())
          def2scope += l.name -> currentScope
          l
        case i: IfGen =>
          val prevScope = currentScope
          currentScope = prevScope + i.pred

          val conseqUsage = HashSet[String]()
          val conseq = visitStatement(i.conseq)(conseqUsage)

          val altUsage = HashSet[String]()
          currentScope = prevScope !+ i.pred
          val alt = visitStatement(i.alt)(altUsage)
          currentScope = prevScope
          
          // register all def only on one side
          defIncomplete ++= (conseqUsage ++ altUsage) -- (conseqUsage & altUsage) 
          usage ++= conseqUsage ++ altUsage // missing already marked as incomplete
          i.copy(conseq = conseq, alt = alt)
          
        // remaining question: ForGen => might induce the use of   
        case _ => s.mapStmt(visitStatement)
      }
    }

    // module used for second pass
    implicit val usage = HashSet[String]()
    val moduleNext = m.mapStmt(visitStatement) match {
      case m: Module => m
      case x =>
        critical(m, "Unexpected non-module, attempting runtime cast (will probably crash)")
        x.asInstanceOf[Module]
    }
    
    // SECOND PASS => use reg2clocks to fix registers definitions
    val rectifiedOutputRegStmt = ArrayBuffer[Statement]()
    val renameMap = new RenameMap()
    
    val module = clocksUsageCounts.keys.toSeq match {
      // to do add parameter to allow clock rename or not (no rename => rawmodule + withClock)
      case Seq() => moduleNext
      case Seq(e) => 
        renameMap.add(Rename(e, "clock", true)) // very important for submodules (and consistency)
        val m = moduleNext.mapPort(p => {
          if(p.name == e){
            p.copy(isDefaultClock = Some(e)) // required for clock propagation 
          } else {
            p
          }
        })
        m.copy(clock = Some("clock"))
      case _ => 
        critical(moduleNext, "Multi-clock currently unsupported")
        moduleNext.copy(clock = None) // not supported at emission for now
    }
    
    def hasValue(res: LogicResolution): Option[Expression] = {
      val value = res match {
        case LogicUnresolved(v) => v
        case LogicWire(v) => v
        case r:LogicRegister => r.preset
        case _ => UndefinedExpression()
      }
      value match {
        case _:UndefinedExpression => None
        case e => Some(e)
      }
    }
    
    def getNewLogicRes(p: SVNode, name: String, initial: LogicResolution, tpe: Type): (Boolean, LogicResolution) = {
      def getKind(clock: Option[Expression]): (Boolean, SimpleLogicResolution) = {
        val ue = UndefinedExpression()
        val value = hasValue(initial).getOrElse(UndefinedExpression())
        (clock.isDefined, clock.map(c => LogicRegister(c, ue, ue, value)).getOrElse(LogicWire(value)))
      }
      def2binding.get(name) match {
        case None => getKind(None)
        case Some(CondBindingSeq(Seq())) => getKind(None) // only declared, not connected
        
        case Some(CondBindingSeq(s)) => 
          val bindings = LinkedHashMap[Expression, Option[Expression]]()
          
          implicit val srcFile = currentSourceFile
          implicit val stream = currentStream
          
          s.foreach { case (e, clock) => {
            (bindings.get(Utils.cleanTokens(e)), clock) match {
              case (Some(None), None) => // silently ignore duplicates
              case (Some(None), Some(clk)) => 
                warn(p, s"Ignoring previous combinational assign in favor of clock ${clk.serialize} for logic $name in scope ${e.serialize}")
                bindings(e) = clock
                
              case (Some(Some(clk)), None) => 
                warn(p, s"Ignoring unexpected combinational assign for logic $name after clocked (${clk.serialize}) assign in scope ${e.serialize}")
                
              case (Some(Some(clk)), Some(clock)) if (Utils.eq(clk, clock)) => // silently ignore duplicates
              
              case (Some(Some(clk)), Some(clock)) =>
                critical(p, s"Ignoring unexpected second clocked assign (${clock.serialize}) after clocked assign (${clk.serialize}) in scope ${e.serialize}")
                
              case _ => // adding new binding
                bindings += e -> clock
            }
          }}
          
          val grouped = bindings.toSeq.groupBy(_._2).mapValues(_.map(b => Utils.cleanTokens(b._1)).reduce((a, b) => {
            (a, b) match {
              case (_:DontCare, _) => DontCare()
              case (_, _:DontCare) => DontCare()
              case _ => if(Utils.eqRawExpr(a,b)) a else DoPrim(ui, PrimOps.Or(ui), Seq(a, b))
            }
            
          }))
          // we need (would like) a groupBy preserving the order of the keys for more idiomatic conversions
          // so we retrieve the original order, getting unicity thanks to Set
          val orderedKeys = LinkedHashSet[Option[Expression]]() ++= bindings.toSeq.map(_._2)
          
          orderedKeys.toSeq.map(k => k -> grouped(k)) match { // NB: tuple members are inverted after the groupBy
            case Seq((clock, _:DontCare)) => getKind(clock)
              
            case Seq((clock, e)) if(defIncomplete.contains(name)) => 
              val (isClocked, res) = getKind(clock)
              res match {
                case LogicWire(_:UndefinedExpression) => // error
                  critical(p, s"Unable to guarantee complete scope definition for logic ${name} resolved as wire without user-provided default value => adding default 0.U.asTypeOf(${tpe.serialize}) & ignoring ${e.serialize} condition")
                  (isClocked, LogicWire(Utils.getDefaultHwZeroesExpr(tpe)))
                  
                // only covering SimpleLogicResolution
                // no more unresolved by design
                // LogicRegisters do not require default to prevent firrtl unconnected wire errors
                case _ => getKind(clock)
              }
              
            case Seq((clock, _)) => getKind(clock) // complete
            
            case s => 
              
              if(s.collectFirst { case (_, _:DontCare) => true }.isDefined){
                critical(p, s"Cannot deal with more than a generic definition for logic ${name}")
                getKind(s.head._1)
              } else {
                val gotSomeClock = s.collectFirst { case (Some(c), _) => c }.isDefined
                (gotSomeClock, LogicConditional(s.map(b => LogicBinding(b._2, getKind(b._1)._2))))
              }
          }
      }
    }
    
    def processPort(p: Port): Port = {
      val (isClocked, logicRes) = getNewLogicRes(p, p.name, p.resolution, p.tpe)
      
      def createIntermediateConnect(postfix: String) = {
        val regName = p.name + postfix
        val tmpName = p.name + "_tmp_name"
        val refp = Reference(ui, tmpName, Seq())
        val refr = Reference(ui, regName, Seq())
        rectifiedOutputRegStmt += DefLogic(ui, na, regName, p.tpe, logicRes)
        renameMap.add(Rename(p.name, regName, false))
        renameMap.add(Rename(tmpName, p.name, false))
        rectifiedOutputRegStmt += Connect(ui, na, refp, refr, true, false)
      }
      
      (p.resolution, p.direction, isClocked, hasValue(logicRes)) match {
        case (_:LogicUnresolved, _, false, None) => 
        case (_:LogicUnresolved, _:Output, false, Some(v)) => 
          info(p, s"Creating intermediate wire and connect for output port ${p.name} with default value ${v.serialize}")
          createIntermediateConnect("__out_default")
          
        case (_:LogicUnresolved, d, false, Some(v)) => 
          critical(p, s"Unexpected default values for ${d.serialize} port ${p.name} (Value ${v.serialize} ignored)")
          
        case (_:LogicUnresolved, _: Output, true, _) => 
          info(p, s"Creating proper register declaration and connect for implicit output reg ${p.name}")
          createIntermediateConnect("__out_reg")
          
        case (_:LogicUnresolved, d: Direction, true, _) => 
          critical(p, s"Unexpected ${d.serialize} port ${p.name} assigned in clocked region (Resolved as wire)")
          
        case (_:LogicRegister, _: Output, false, None) =>
          warn(p, s"Inconsistent declaration as register of ${p.name} while never assigned in a clocked region (always @<pos|neg>edge <clock>). (resolved as wire declaration)")
          
        case (_:LogicRegister, _: Output, false, Some(v)) =>
          warn(p, s"Inconsistent declaration as register of ${p.name} while never assigned in a clocked region (always @<pos|neg>edge <clock>). (resolved as wire declaration)")
          info(p, s"Creating intermediate wire and connect for output port ${p.name} with default value ${v.serialize}")
          createIntermediateConnect("__out_default")
        
        case (_:LogicWire, _: Output, true, _) =>
          warn(p, s"Inconsistent declaration as wire of ${p.name} while assigned in a clocked region (always @<pos|neg>edge <clock>). (Resolved creating proper register declaration and connect for this implicit output reg)")
          createIntermediateConnect("__out_reg")
          
        case (_:LogicWire, d: Direction, true, _) =>
          critical(p, s"Unexpected clocked assignment for declared ${d.serialize} port ${p.name} (resolved as wire)")
        
        case (_:LogicRegister, _: Output, true, _) => 
          info(p, s"Creating proper register declaration and connect for output reg ${p.name}")
          createIntermediateConnect("__out_reg")
          
        case (_:LogicRegister, d: Direction, true, _) =>
          critical(p, s"Unexpected ${d.serialize} port ${p.name} register. Cannot make sense of it (resolved to wire)")
          
        case _ => 
      }
      p.copy(resolution = LogicWire(UndefinedExpression()))
    }
    
    def processLogic(l: DefLogic): DefLogic = {
      val (isClocked, logicRes) = getNewLogicRes(l, l.name, l.resolution, l.tpe)
      
      (l.resolution, isClocked) match {
        case (_:LogicRegister, false) =>
          warn(l, s"Inconsistent declaration as register of ${l.name} while never assigned in a clocked region (always @<pos|neg>edge <clock>). (resolved as wire declaration)")
        
        case (_:LogicWire, true) =>
          warn(l, s"Inconsistent declaration as wire of ${l.name} while assigned in a clocked region (always @<pos|neg>edge <clock>). (resolved as register declaration) ")
          
        case _ =>
      }
      l.copy(resolution = logicRes)
    }
    
    def processStatement(s: Statement, refsInScope: HashSet[WRef]): Statement = {
      s match {
        case f: DefFunction => processFunction(f)
        case p: Port => 
          refsInScope += WRef(p.name)
          processPort(p)
          
        case l: DefLogic => 
          refsInScope += WRef(l.name)
          processLogic(l)
          
        case p: DefParam => 
          refsInScope += WRef(p.name)
          p
        case i: IfGen => 
          val conseq = processStatement(i.conseq, new HashSet[WRef]() ++ refsInScope)
          val alt = processStatement(i.alt, new HashSet[WRef]() ++ refsInScope)
          // NB: need to add a way to remove duplicated connect statement on IfGen (moved above) and to remove the IfGen if empty => no so trivial to do properly without messing with potential dependencies on delecaration order => only the perfectly duplicated alt & conseq might be candidate for this transform ?
          // the other idea would be to stop at first statement not movable 
          // critical(i, "TODO: remove duplicated statement from conseq & alt")
          i.copy(conseq = conseq, alt = alt)
          
        case f: ForGen =>
          val localStore = new HashSet[WRef]() ++ refsInScope
          f.init match {
            case na: NamedAssign => localStore += WRef(na.name)
            case _ => 
          }
          f.copy(stmt = processStatement(f.stmt, localStore))
          
        case _ => s.mapStmt(processStatement(_, refsInScope))
      }
    }
    
    // Note statement must be placed right after IOs because in scala vals cannot be used before declaration
    lazy val comment = Seq(Comment(ui,s"NOTE: The following statements are auto generated based on existing output reg of the original verilog source"))
    
    // NB: must be done before the match on rectifiedOutputRegStmt (mutable, modified by processStatement)
    val processed = module.mapStmt(processStatement(_, refsInScope)) 
    
    val updatedModule = (processed, rectifiedOutputRegStmt.size) match {
      case (b, 0) => b
      case (m: Module, _) => m.insertAfterPorts(SimpleBlock(ui, comment ++ rectifiedOutputRegStmt))
      case _ => 
        critical(processed, "Unexpected non-module: ignoring output register normalization")
        processed
    }
    currentProject.get.clearDescriptionCache() // clock & reset have been modified
    val res = renameLocalReferences(updatedModule, renameMap)
    seenModules += ((m.name, res))
    res
  }
}