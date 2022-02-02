// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import logger._

import org.antlr.v4.runtime.{CommonTokenStream}
import collection.mutable.{HashMap, ArrayBuffer}

abstract class Transform extends LazyLogging {
  def options: TranslationOptions
  
  // To be implemented depending on needs 
  def execute(project: Project): Unit
  
  case class Rename(from: String, to: String, includePorts: Boolean = false)
  class RenameMap(){
    private val rn = HashMap[String,Rename]()
    def add(r: Rename): Unit = rn += ((r.from, r))
    def add(s: Seq[Rename]): Unit = s.foreach(r => rn += ((r.from, r)))
    def get: HashMap[String, Rename] = rn
    def contains(s: String, port: Boolean = false): Boolean = rn.contains(s) && (!port || rn(s).includePorts)
    def update(r: Reference): Reference = if(!contains(r.name)) r else r.copy(name = rn(r.name).to)
    def update(p: Port): Port = if(!contains(p.name, true)) p else p.copy(name = rn(p.name).to)
  }
  
  // private stuff
  private[transforms] def renameLocalReferences[T <: Description](des: T, renameMap: RenameMap): des.T = {
    
    def processType(t: Type): Type = {
      t.mapWidth(_.mapExpr(processExpression))
    }
    
    def processExpression(e: Expression): Expression = {
      val expr = e match {
        case r: Reference => renameMap.update(r)
        case _ => e
      }
      expr.mapExpr(processExpression).mapType(processType)
    }
    
    def processStatement(s: Statement): Statement = {
      val updated = s match {
        case p: Port => renameMap.update(p)
        case st => st
      }
      updated.mapExpr(processExpression).mapType(processType).mapStmt(processStatement)
    }
    
    des.mapStmt(processStatement)
  }
}

abstract class SourceBasedTransform extends Transform with InfoLogger {
  // To be implemented depending on needs
  def processSourceFile(s: SourceFile): SourceFile
  
  // helpers 
  var currentSourceFile : Option[SourceFile] = None 
  var currentStream : Option[CommonTokenStream] = None
  
  def execute(project: Project): Unit = {
    project.mapEntry(e => {
      currentSourceFile = Some(e.src)
      currentStream = Some(e.stream)
      e.copy(src = processSourceFile(e.src))
    })
  }
}

abstract class DescriptionBasedTransform extends Transform with InfoLogger {
  // To be implemented depending on needs
  def processDescription(d: Description): Description
  // Optional
  val preprocessDescription : Option[Description => Description] = None
  
  // helpers 
  implicit def reference2WRef(r: Reference): WRef = WRef(r.name, r.path)
  implicit def userRefType2WRef(r: UserRefType): WRef = WRef(r.name, r.path)
  type RefStore = HashMap[WRef, FullType]
  
  var currentProject : Option[Project] = None
  var currentDescription : Option[Description] = None
  var currentSourceFile : Option[SourceFile] = None
  var currentStream : Option[CommonTokenStream] = None
  
  var localPackages = new HashMap[String, DefPackage]() // packages defined within the current project entry
  val importedPackages = new ArrayBuffer[PackageRef]() // package explicitly imported (members accessibles directly)
  val scopedPackages = new ArrayBuffer[String]() // packages in scope whose members can be refered explicitly
  
  private var refOutdated = true
  private val remoteRefsStore : RefStore = new HashMap[WRef, FullType]()
  
  protected def forceRefsRefresh(): Unit = {
    trace("forceRefresh")
    refOutdated = true
    currentProject.get.clearDescriptionCache()
    remoteRefsStore.clear()
  }
  
  private def findDescription(name: String): Option[Description] = {
    localPackages.get(name) match {
      case None => currentProject.get.findDescription(name)
      case s => s // return Some(p: DefPackage)
    }
  }
  
  protected def processImportStatement[T <: Statement](s: T, localRefStore: RefStore): T = {
    s match {
      case ImportPackages(_,sp) => sp.foreach(p => {
        debug(s, s"locally imported package: ${p.path}")
        findDescription(p.path) match {
          case Some(d: DefPackage) =>
            d.refs match {
              case Some(h) => 
                // all items
                if(p.allItems) { 
                  localRefStore ++= h
                } else {
                  if(h.contains(WRef(p.item))){
                    localRefStore += ((WRef(p.item), h(WRef(p.item))))
                  } else {
                    critical(s, s"No reference found for ${p.item} within package ${p.path}")
                  }
                }
              case _ => critical(s, s"No references found for package ${p.path}, did you properly set-ups the package refs before using processImportStatement inside a transform?")
            }
          case _ => critical(s, s"Unable to retrieve package declaration for package ref ${p.path}")
        }
      })
      case _ => 
    }
    s // return s for easy chaining
  }
  
  protected def remoteRefs: RefStore = {
    if (refOutdated) {
      refOutdated = false
      // in all cases all references with explicit paths should be available
      scopedPackages.foreach(name => {
        debug(currentDescription.get, s"scoped package: $name")
        findDescription(name) match {
          case Some(d: DefPackage) =>
            d.refs match {
              case Some(h) => 
                for ((k, v) <- h) {
                  remoteRefsStore += ((k.in(name), v)) // prepending current package name to get access
                }
              case _ => critical(currentDescription.get, s"No references found for package ${name}, did you properly set-ups the package refs before using remoteRefs inside a transform?")
            }
          case _ => critical(currentDescription.get, s"Unable to retrieve package declaration for package ref ${name}")
        }
      })
      
      for (p <- importedPackages) {
        debug(s"importedPackage: ${p.path}")
        findDescription(p.path) match {
          case Some(d: DefPackage) =>
            d.refs match {
              case Some(h) => 
                // all items
                if(p.allItems) { 
                  remoteRefsStore ++= h
                } else {
                  if(h.contains(WRef(p.item))){
                    remoteRefsStore += ((WRef(p.item), h(WRef(p.item))))
                  } else {
                    critical(currentDescription.get, s"No reference found for ${p.item} within package ${p.path}")
                  }
                }
              case _ => critical(currentDescription.get, s"No references found for package ${p.path}, did you properly set-ups the package refs before using remoteRefs inside a transform?")
            }
          case _ => critical(currentDescription.get, s"Unable to retrieve package declaration for package ref ${p.path}")
        }
        
      }
    }
    remoteRefsStore
  }
  
  protected def updateContext(desc: String): Unit = {
    currentProject.get.getEntries.map(e => e.src.descriptions.collect {
      case n: HasName => (n, e)
    }).flatten.foreach {( t => {
        if (desc == t._1.name) {
          currentSourceFile = Some(t._2.src)
          currentStream = Some(t._2.stream)
        }
      }
    )}
  }
  
  private def processDescriptionWrapper(d: Description, f: Description => Description): Description = {
    currentDescription = Some(d)
    d match {
      case IsolatedStatement(_, i: ImportPackages) => importedPackages ++= i.packages; refOutdated = true
      case _ =>
    }
    f(d) match {
      case p: DefPackage => 
        scopedPackages += p.name // shall not be considered as Scoped for himself !
        localPackages += ((p.name, p)) // for local usage (within the same Project entry -- edge case)
        p
      case des => des
    }
  }
  
  private val currentEntryDescriptions = HashMap[String, Description]()
  
  /** safely update description by first looking among current project entry descriptions to avoid conflicts */
  def safeUpdateDescription(name: String, f: Description => Description): Unit = {
    // Prevent the most basic modification access conflict  
    if(currentDescription.collect({case h: HasName => h}).map(_.name == name).getOrElse(false)){
      // get is safe here
      fatal(currentDescription.get, s"Abort unexpected concurrent modification attempt of $name description") 
    } else if(currentEntryDescriptions.contains(name)) {
      currentEntryDescriptions(name) = f(currentEntryDescriptions(name))
    } else {
      currentProject.map(_.updateDescription(name, f))
    }
  }
  
  def execute(project: Project): Unit = {
    currentProject = Some(project)
    def processEntry(e: ProjectEntry, f: Description => Description): ProjectEntry = {
      importedPackages.clear()
      remoteRefsStore.clear()
      localPackages.clear()
      currentEntryDescriptions.clear()
      refOutdated = true
      currentSourceFile = Some(e.src)
      currentStream = Some(e.stream)
      // all this side-effet logic is to safely enable side-effect transforms (update a remote description)
      currentEntryDescriptions ++= e.src.descriptions.collect({case h: HasName => h}).groupBy(_.name).mapValues(v => {
        v match {
          case Seq(i) => i
          case s => 
            fatal(s.head, s"Found multiple descriptions with name ${s.head.name}")
            s.head // NB: cannot be empty
        }
      })
      val desc = e.src.descriptions.map(d => d match {
        case named: HasName => 
          currentEntryDescriptions(named.name) = f(currentEntryDescriptions(named.name))
          Left(named.name)
        case _ => Right(f(d))
      })
      //NOTE: project only updated once per entry (not on description basis)
      e.copy(src = e.src.copy(descriptions = desc.map(d => d match {
        case Left(name) => currentEntryDescriptions(name)
        case Right(d) => d
      })))
    }
    
    // Note : project is mutable
    preprocessDescription match {
      case Some(f) => 
        project.mapEntry(e => processEntry(e, d => processDescriptionWrapper(d, f)))
        scopedPackages.clear()
        localPackages.clear()
        remoteRefsStore.clear()
        refOutdated = true
      case None =>
    }
    // descriptions updated only updated once per entry
    project.mapEntry(e => processEntry(e, d => processDescriptionWrapper(d, processDescription))) 
  }
  
}

abstract class DefModuleBasedTransform extends DescriptionBasedTransform {
  // To be implemented depending on needs
  def processModule(d: DefModule): DefModule
  
  def processDescription(d: Description): Description = {
    d match {
      case m: DefModule => processModule(m)
      case d => d
    }
  }
}
