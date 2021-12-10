// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

class AddDontCare(val options: TranslationOptions) extends DescriptionBasedTransform {

  
  def processDescription(d: Description): Description = {
    d match {
      case m: Module => m.mapStmt(processStatement)
      case d => d
    }
  }

  
  def processPortMap(assign: Seq[Assign], ref: Seq[(Port, Boolean)], ctx: String): Seq[Assign] = {
    // process ports
    val nna = assign.collect({case nna:NoNameAssign => nna})
    val na = assign.collect({case na:NamedAssign => na})
    val aa = assign.collect({case aa:AutoAssign => aa})
    (aa, nna, na) match {
      case (Seq(), Seq(), Seq()) => assign
      case (aa, Seq(), Seq()) => critical(aa.head, s"Unsupported auto-assign $ctx"); assign
      case (Seq(), nna, Seq()) =>
        if(nna.length < ref.length){
          assign ++ ref.drop(nna.length).flatMap { case (p, isClock) => p.direction match {
            case _:Input if(!isClock) => 
              warn(p, s"Adding DontCare for port ${p.name} $ctx")
              Some(NoNameAssign(UndefinedInterval, DontCare(UndefinedInterval), SourceFlow))
            case _ => None 
          }}
        } else {
          assign
        }
      case (Seq(), Seq(), na) =>
        val mapped = na.groupBy(_.name) 
        na ++ ref.flatMap { case (p, isClock) => p.direction match {
          case _: Input if(!mapped.contains(p.name) && !isClock) => 
            warn(p, s"Adding DontCare for port ${p.name} $ctx")
            Some(NamedAssign(UndefinedInterval, p.name, DontCare(UndefinedInterval), SourceFlow))
          case _ => None
        }}
        
      case _ => critical(assign.head, s"Unsupported mixed assign methods $ctx"); assign
    }
  }
  
  def processStatement(s: Statement): Statement = {
    trace(s, s"Entering processStatement for ${s.getClass.getName}")
    s.mapStmt(processStatement) match {
      case i: DefInstance =>
        // let's fetch remote module if known
        currentProject.get.findModule(i.module.serialize) match {
          case Some(m) => 
            val ctx = s"for instance `${i.name}` of module `${i.module.serialize}`"
            val portsNoClock = m.ports.map(p => p -> m.clock.map(_ == p.name).getOrElse(false))
            i.copy(portMap = processPortMap(i.portMap, portsNoClock, ctx))

          case _ => i
        }

      case st => st
    }
  }
  
}