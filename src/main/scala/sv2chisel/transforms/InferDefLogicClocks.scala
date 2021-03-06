// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashMap, HashSet, ArrayBuffer}

/** Resolve logic as wire or reg with appropriate clock (TODO: and reset)
  * Also legalize port reg 
  *   > input reg raise [fatal] (non-sense) (resolved as input wire port)
  *   > output reg are resolved with the generation of an intermediate reg and wire
  */
class InferDefLogicClocks(val llOption: Option[logger.LogLevel.Value] = None) extends DefModuleBasedTransform {
  
  val seenModules = new HashMap[String, DefModule]()
  
  def processModule(m: DefModule): DefModule = {
    if (seenModules.contains(m.name)) {
      debug(m, s"Ignoring multiple processing for module ${m.name}")
      seenModules(m.name)
    } else {
      processModuleCore(m)
    }
  }
  
  private def processModuleCore(m: DefModule): DefModule = {
    val reg2clocks = new HashMap[String, Option[String]]()
    val reg2critical = new HashSet[String]()
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
    def visitConnect(c: Connect): Connect = {

      (c.continuous, c.blocking, activeClock) match {
        case (false, false, Some(clock)) =>  
          getRefs(c.loc).foreach(r => {
            if(reg2clocks.contains(r.name)){
              reg2clocks(r.name) match {
                case Some(declClock) if (declClock != clock) => 
                  critical(c, s"Same declared signal ${r.name} driven by at least 2 different clocks: ${declClock} and ${clock}.")
                case None if(!reg2critical.contains(r.name)) => 
                  critical(c, s"Signal ${r.name} is driven both by clock ${clock} and assigned combinationally. This probably means that ${r.name} is either a wire or a reg depending on a generate parameter. Please review and fix generated code.")
                  reg2critical += r.name
                case _ => // fine : same name (or error already raised)
              }
            } else {
              reg2clocks += ((r.name, Some(clock)))
              registerClockUsage(clock, "non blocking assignment")
            }
          })
        case _ => 
          getRefs(c.loc).foreach(r => {
            if(reg2clocks.contains(r.name)){
              reg2clocks(r.name) match {
                case Some(declClock) if(!reg2critical.contains(r.name)) => 
                  critical(c, s"Signal ${r.name} is driven both by clock ${declClock} and assigned combinationally. This probably means that ${r.name} is either a wire or a reg depending on a generate parameter. Please review and fix generated code.")
                  reg2critical += r.name
                case _ => // fine : same kind (or error already raised)
              }
            } else {
              reg2clocks += ((r.name, None))
            }
          })
        
      }
      c // nothing to be modified yet
    }
    
    def visitClockRegion(c: ClockRegion): Block = {
      if(!c.posedge) 
        Utils.throwInternalError("Only posedge supported for emission")
        
      activeClock = c.clock match {
        case r: Reference => Some(r.name)
        case c => Utils.throwInternalError(s"Weird clock dude! ${c.serialize}") 
      }
      // Mapping Statements
      val reg = c.mapStmt(visitStatement)
      activeClock = None
      
      // Removing Always @ clock region for simple simple blocks
      reg.toSimpleBlock()
    }
    
    def visitInstance(i: DefInstance): DefInstance = {
      val instModule = if(seenModules.contains(i.module.serialize)) {
        Some(seenModules(i.module.serialize))
      } else {
        currentProject.get.findModule(i.module.serialize) match {
          case Some(d) =>
            info(i, s"Processing in advance module ${i.module.serialize} instanciated as ${i.name} in current module ${m.name}.")
            val stream = currentStream
            val src = currentSourceFile
            updateContext(d.name)
            val res = Some(processModule(d))
            currentStream = stream
            currentSourceFile = src
            res
          case None =>
              warn(i, s"Unable to find module module ${i.module.serialize} instanciated as ${i.name} in current module ${m.name} for clock inference processing.")
              None
        }
      }
      instModule match {
        case Some(d) =>
          val clock = d.ports.zipWithIndex.collectFirst{
            case (Port(_,_,_,_,_,_,_,_,_,Some(n),_),p) => (n,p)
          }.getOrElse(("<unknown>", -1))
          
          i.portMap.zipWithIndex.foreach(p => p._1 match {
            case na: NamedAssign if(na.name == clock._1) => registerClockUsage(na.expr.serialize, "named port map to clocked submodule")
            case na: NoNameAssign if(p._2 == clock._2) => registerClockUsage(na.expr.serialize, "sequential port map to clocked submodule")
            case _ => 
          })
          i
        case None => i // nothing to do 
      }
      
    }
    
    def visitStatement(s: Statement): Statement = {
      s match {
        case c: ClockRegion => visitClockRegion(c)
        case c: Connect => visitConnect(c)
        case i: DefInstance => visitInstance(i)
        case _ => s.mapStmt(visitStatement)
      }
    }

    // module used for second pass
    val module = m.mapStmt(visitStatement).asInstanceOf[Module]
    
    // SECOND PASS => use reg2clocks to fix registers definitions
    val rectifiedOutputRegStmt = ArrayBuffer[Statement]()
    val renameMap = new RenameMap()
    
    val (clock, updatedPorts) = clocksUsageCounts.keys.toSeq match {
      // to do add parameter to allow clock rename or not (no rename => rawmodule + withClock)
      case Seq() => (Some("clock"), module.ports)
      case Seq(e) => 
        renameMap.add(Rename(e, "clock", true)) // very important for submodules (and consistency)
        val ports = module.ports.map(p => {
          if(p.name == e){
            p.copy(isDefaultClock = Some(e)) // required for clock propagation 
          } else {
            p
          }
        })
        (Some("clock"), ports)
      case _ => 
        critical(module, "Multi-clock currently unsupported")
        (None, module.ports) // not supported at emission for now
    }
    
    
    def processPort(p: Port): Port = {
      val ui = UndefinedInterval
      val na = NoVerilogAttribute
      val clock = reg2clocks.getOrElse(p.name, None)
      
      def createPortRegConnect(c: String) = {
        val regName = p.name + "__out_reg"
        val tmpName = p.name + "_tmp_name"
        val refp = Reference(ui, tmpName, Seq())
        val refr = Reference(ui, regName, Seq())
        rectifiedOutputRegStmt += DefLogic(ui, na, regName, p.tpe, p.clock, p.reset, p.init, LogicRegister)
        renameMap.add(Rename(p.name, regName, false))
        renameMap.add(Rename(tmpName, p.name, false))
        rectifiedOutputRegStmt += Connect(ui, na, refp, refr, true, false)
      }
      
      (p.resolution, p.direction, clock) match {
        case (LogicUnresolved, _, None) => p.copy(resolution = LogicWire)
        case (LogicUnresolved, d: Output, Some(c)) => 
          info(p, s"Creating proper register declaration and connect for implicit output reg ${p.name}")
          createPortRegConnect(c)
          p.copy(clock = Reference(ui, c, Seq()), resolution = LogicWire)
          
        case (LogicUnresolved, d: Direction, Some(c)) => 
          critical(p, s"H'ld a sec... ${d.serialize} reg? Are you trying to assign an input? (Resolved as wire)")
          p.copy(resolution = LogicWire)
          
        case (LogicRegister, d: Output, None) =>
          if(!reg2critical.contains(p.name)){
            warn(p, s"Inconsistent declaration as register of ${p.name} while never assigned in a clocked region (always @<pos|neg>edge <clock>). (resolved as wire declaration)")
          } else {
            warn(p, s"Due to previous error ${p.name} is resolved as wire declaration which is reportedly inaccurate at least for some part of the description.")
          }
            
          p.copy(resolution = LogicWire)
        
        case (LogicWire, d: Output, Some(c)) =>
          if(!reg2critical.contains(p.name)){
            warn(p, s"Inconsistent declaration as wire of ${p.name} while assigned in a clocked region (always @<pos|neg>edge $c). (Resolved creating proper register declaration and connect for this implicit output reg)")
          } else {
            warn(p, s"Due to previous error ${p.name} is resolved creating proper register declaration and connect for this implicit output reg which is reportedly inaccurate at least for some part of the description.")
          }
          createPortRegConnect(c)
          p.copy(clock = Reference(ui, c, Seq()), resolution = LogicWire)
          
        case (LogicWire, d: Direction, Some(c)) =>
          critical(p, s"Found clocked assignment for declared ${d.serialize} wire ${p.name}: cannot create register for input ports it does not make sense !")
          p.copy(clock = Reference(UndefinedInterval, c, Seq()), resolution = LogicRegister)
        
        case (LogicRegister, d: Output, Some(c)) => 
          info(p, s"Creating proper register declaration and connect for output reg ${p.name}")
          createPortRegConnect(c)
          p.copy(clock = Reference(ui, c, Seq()), resolution = LogicWire)
          
        case (LogicRegister, d: Direction, _) =>
          critical(p, s"H'ld a sec... ${d.serialize} register? Cannot create register for input port '${p.name}' it does not make sense! (resolved to wire)")
          p.copy(resolution = LogicWire)
          
        case _ => p // nothing to do 
      }
    }
    
    def processLogic(l: DefLogic): DefLogic = {
      val clock = reg2clocks.getOrElse(l.name, None)
      (l.resolution, clock) match {
        case (LogicUnresolved, None) => l.copy(resolution = LogicWire)
        case (LogicUnresolved, Some(c)) => 
          l.copy(clock = Reference(UndefinedInterval, c, Seq()), resolution = LogicRegister)
          
        case (LogicRegister, None) =>
          if(!reg2critical.contains(l.name)){
            warn(l, s"Inconsistent declaration as register of ${l.name} while never assigned in a clocked region (always @<pos|neg>edge <clock>). (resolved as wire declaration)")
          } else {
            warn(l, s"Due to previous error ${l.name} is resolved as wire declaration which is reportedly inaccurate at least for some part of the description.")
          }
          l.copy(resolution = LogicWire)
        
        case (LogicWire, Some(c)) =>
          if(!reg2critical.contains(l.name)){
            warn(l, s"Inconsistent declaration as wire of ${l.name} while assigned in a clocked region (always @<pos|neg>edge $c). (resolved as register declaration) ")
          } else {
            warn(l, s"Due to previous error ${l.name} is resolved as register declaration which is reportedly inaccurate at least for some part of the description.")
          }
          l.copy(clock = Reference(UndefinedInterval, c, Seq()), resolution = LogicRegister)
          
        case (LogicRegister, Some(c)) => 
          l.copy(clock = Reference(UndefinedInterval, c, Seq()))
          
        case _ => l // nothing to do 
      }
    }
    
    def processStatement(s: Statement): Statement = {
      s match {
        case r: DefLogic => processLogic(r)
        case _ => s.mapStmt(processStatement)
      }
    }
    
    // Note statement must be placed right after IOs because in scala 
    val comment = Seq(Comment(UndefinedInterval,s"NOTE: The following statements are auto generated based on existing output reg of the original verilog source"))
    
    val ports = updatedPorts.map(processPort)
    val body = (module.body, rectifiedOutputRegStmt.size) match {
      case (b, 0) => b.mapStmt(processStatement)
      case (b: Block, _) =>  
        b.mapStmt(processStatement).prependStmts(comment ++ rectifiedOutputRegStmt)
      case (s: Statement, _) => SimpleBlock(s.tokens, comment ++ rectifiedOutputRegStmt ++ Seq(s))
    }
    
    currentProject.get.clearDescriptionCache() // clock & reset have been modified
    val res = renameReferences(module.copy(ports = ports, body = body, clock = clock), renameMap)
    seenModules += ((m.name, res))
    res
  }
}