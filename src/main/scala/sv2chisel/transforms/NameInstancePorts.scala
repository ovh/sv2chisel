// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

/**
  * Ensure that instances use Only Named assigns 
  * Named assigns are required in chisel and can be inferred only if the 
  * declaration of instatiated module is available within the current project 
  */
class NameInstancePorts(
  val options: TranslationOptions
) extends DefModuleBasedTransform {
  
  def processModule(m: DefModule): DefModule = {
    
    // single pass 
    
    def processInstance(i: DefInstance): DefInstance = {
      val mName = i.module.serialize
      val noName = i.portMap.collect({case na: NoNameAssign => na})
      (noName.length, i.portMap.length) match {
        case (0, _) => i
        case (l, ll) if (l != ll) => 
          critical(i, s"Unsupported portmap for instance ${i.name} of module ${i.module} with both named and unnamed port assignations")
          i
        case (l, _) => 
          currentProject.get.findModule(mName) match {
            case None => 
              critical(i, s"Unable to find declaration for module ${mName} in current project. Sequential port-map of instance ${i.name} of this module ${mName} cannot be transformed to named port-map which will prevent successful chisel emission. To fix this issue please add the declaration of the module within project files or fix the port map manually")
              i
            case Some(d) => 
              if(d.ports.length != l) {
                critical(i, s"Instance ${i.name} of module ${mName} has mismatching number of ports ($l) with declaration (${d.ports.length})")
                i
              } else {
                val portMap = noName.zip(d.ports).map(t => t._1.toNamed(t._2.name))
                i.copy(portMap = portMap)
              }
          }
      }
    }
    
    def processStatement(s: Statement): Statement = {
      s match {
        case i: DefInstance => processInstance(i)
        case _ => s.mapStmt(processStatement)
      }
    }
    m.mapStmt(processStatement)
  }
}