// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

/** propagate infered clocks to def instance
  * 
  * 
  * 
  */
class PropagateClocks(val llOption: Option[logger.LogLevel.Value] = None) extends DefModuleBasedTransform {
  
  def processModule(module: DefModule): DefModule = {
    
    def processInstance(i: DefInstance) = {
      currentProject.get.findModule(i.module.serialize) match {
        case Some(m: DefModule) =>
          (m.clock, module.clock) match {
            case (Some(ci), Some(cm)) if (ci == cm) => 
              // remove clock from port map
              val renamedClock = m.ports.collectFirst{
                case Port(_,_,_,_,_,_,_,_,_,Some(n),_) => n
              }.getOrElse("<error>")
              val portMap = i.portMap.flatMap(p => {
                p match {
                  case NamedAssign(_, n, _, _, _, _, _) if(n == ci || n == renamedClock) => None 
                  case NamedAssign(_, _, r: Reference, _, _, _, _) if(r.serialize == ci) => None 
                  case NoNameAssign(_, r: Reference, _, _, _, _, _) if(r.serialize == ci) => None
                  case _ => Some(p)
                }
              })
              i.copy(portMap = portMap, clock = Some(ci))

            case (None, Some(cm@_)) => 
              // update port map for instance
              critical(i, "TODO: update port map for instance")
              i
              
            case (Some(ci), None) => 
              // update port map for instance
              // NOTE: updating the port map here is not enough, Module(new sub) will require withClock context
              val renamedClock = m.ports.collectFirst{
                case Port(_,_,_,_,_,_,_,_,_,Some(n),_) => n
              }.getOrElse("<error>")
              val portMap = i.portMap.map(p => {
                p match {
                  case na @NamedAssign(_, n, _, _, _, _, _) if(n == renamedClock) =>  
                    na.copy(name = ci)
                  case _ => p
                }
              })
              i.copy(portMap = portMap, clock = Some(ci))
              
            case _ => i // nothing to do ???
          }
          
        case None => 
          // not sure that the warning is necessary
          warn(i, s"Module ${i.module.serialize} referenced by instance ${i.name} cannot be found in current design. Clock & Reset management might be inaccurate.")
          i 
      }
    }
    
    def processStatement(s: Statement): Statement = {
      s match {
        case i: DefInstance => processInstance(i)
        case _ => s.mapStmt(processStatement)
      }
    }
    
    module.mapStmt(processStatement)
    // TO DO : if one clock was infered from sub modules please update current module
    // and then redo the transform to ensure hierarchical propagation
  }
}