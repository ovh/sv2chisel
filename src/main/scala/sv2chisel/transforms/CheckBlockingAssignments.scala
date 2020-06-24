// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashMap, ArrayBuffer}

/** Resolve logic as wire or reg with appropriate clock (TODO: and reset)
  * Also legalize port reg 
  *   > input reg raise [fatal] (non-sense) (resolved as input wire port)
  *   > output reg are resolved with the generation of an intermediate reg and wire
  */
class CheckBlockingAssignments(val llOption: Option[logger.LogLevel.Value] = None) extends DefModuleBasedTransform {
  
  
  def processModule(m: DefModule): DefModule = {
    
    var inClockedRegion : Boolean = false
    
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
    
    //Single pass => fill reg2clocks
    def visitConnect(c: Connect): Unit = {
      (c.blocking, inClockedRegion) match {
        case (true, true) =>  
          critical(c, s"Blocking statement in clocked region: ${c.loc.serialize} = ${c.expr.serialize}")
          // TO DO => filter a bit more
          // getRefs(c.loc).foreach(r => {
          //   if(reg2clocks.contains(r.name)){
          //     val declClock = reg2clocks(r.name)
          //     if (declClock != clock) {
          //       Utils.throwInternalError(s"Same declared object ${r.name} driven by at least 2 different clocks: ${declClock} and ${clock}.")
          //     }
          //   } else {
          //     reg2clocks += ((r.name, clock))
          //     registerClockUsage(clock, "non blocking assignment")
          //   }
          // })
        case _ => // nothing to do
      }
    }
    
    def visitClockRegion(c: ClockRegion): Unit = {
      // no checks here: would be duplicated from InferDefLogicClocks
      inClockedRegion = true
      // Visiting Statements
      c.foreachStmt(visitStatement)
      inClockedRegion = false
    }
    
    
    def visitStatement(s: Statement): Unit = {
      s match {
        case c: ClockRegion => visitClockRegion(c)
        case c: Connect => visitConnect(c)
        case _ => s.foreachStmt(visitStatement)
      }
    }

    // module used for second pass
    m.foreachStmt(visitStatement)
    m
  }
}