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
class PropagateClocks(val options: TranslationOptions) extends DefModuleBasedTransform {
  
  def processModule(module: DefModule): DefModule = {
    
    def processInstance(i: DefInstance) = {
      currentProject.get.findModule(i.module.name) match {
        case Some(m: DefModule) =>
          
          def updatedPortMap(name: String): Seq[Assign] = {
            // remove clock from port map
            val renamedClock = m.ports.collectFirst{
                case port:Port if(!port.isDefaultClock.isEmpty) => port.isDefaultClock.get
            }.getOrElse("<error>")
            i.portMap.flatMap(p => {
              p match {
                case NamedAssign(_, n, _, _, _, _, _) if(n == name || n == renamedClock) => None 
                case NamedAssign(_, _, r: Reference, _, _, _, _) if(r.serialize == name) => None 
                case NoNameAssign(_, r: Reference, _, _, _, _, _) if(r.serialize == name) => None
                case _ => Some(p)
              }
            })
          }
        
          (m.clock, module.clock) match {
            case (Some(ci), Some(cm)) if (ci == cm) => i.copy(portMap = updatedPortMap(ci), clock = Some(ci))

            case (None, Some(cm)) => 
              // update port map for instance
              // NB: proper Clock() & Reset() types are required for instantiation of ExtModules
              safeUpdateDescription(m.name, _ => {
                i.portMap.flatMap(p => {
                  p match {
                    case NamedAssign(_, n, r: Reference, _, _, _, _) if(r.serialize == cm) => Some(n) 
                    case NoNameAssign(_, r: Reference, _, _, _, _, _) if(r.serialize == cm) => None // index ?
                    case _ => None
                  }
                }) match {
                  case Seq() => 
                    warn(i, s"Cannot find connected clock `$cm` for instance ${i.name} of module ${m.name}")
                    m // nothing to do, clock does not seem connected
                  case Seq(n) => 
                    m match {
                      case e: ExtModule => e.copy(clock = Some(n))
                      case e: Module => e.copy(clock = Some(n))
                    }
                  case _ =>
                    warn(i, s"Found multiple clock `$cm` for instance ${i.name} of module ${m.name}: aborting update")
                    m
                }
              })
              m match {
                case _: ExtModule => i // nothing to update in portmap
                case _: Module => i.copy(portMap = updatedPortMap(cm), clock = Some(cm))
              }
              
            case (Some(ci), None) => 
              // update port map for instance
              // NOTE: updating the port map here is not enough, Module(new sub) will require withClock context
              val renamedClock = m.ports.collectFirst{
                case port:Port if(!port.isDefaultClock.isEmpty) => port.isDefaultClock.get
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