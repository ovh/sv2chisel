// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import logger._

import org.antlr.v4.runtime.{CommonTokenStream}
import scala.collection.JavaConverters._
import collection.mutable.{HashMap, ArrayBuffer}

abstract class Transform extends LazyLogging {
  // To be implemented depending on needs 
  protected def execute(project: Project): Unit
  
  val llOption : Option[LogLevel.Value]
  
  def run(project: Project): Unit = {
    llOption match {
      case Some(ll) if ll != Logger.getGlobalLevel => 
        val prev = Logger.getGlobalLevel
        Logger.setLevel(ll)
        execute(project)
        Logger.setLevel(prev)
      
      case _ => execute(project) 
    }
  }
  
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
  private[transforms] def renameReferences(module: DefModule, renameMap: RenameMap): DefModule = {
    
    def processExpression(e: Expression): Expression = {
      val expr = e match {
        case r: Reference => renameMap.update(r)
        case _ => e
      }
      expr.mapExpr(processExpression)
    }
    
    def processStatement(s: Statement): Statement = {
      val updated = s match {
        case p: Port => renameMap.update(p)
        case st => st
      }
      updated.mapExpr(processExpression).mapStmt(processStatement)
    }
    
    module match {
      case m: Module => m.copy(body = processStatement(m.body))
      case m => m
    }
  }
}

trait InfoLogger extends EasyLogger {
  def currentSourceFile : Option[SourceFile]
  def currentStream : Option[CommonTokenStream]
  
  def getInfo(n: SVNode): String = getInfo(n.tokens)
  def getInfo(i: Interval): String = {
    val path = currentSourceFile match {
      case Some(src) => src.path
      case None => "???" 
    }
    (currentStream, i) match {
      case (_, UndefinedInterval) => s"${path}:???"
      case (Some(stream), _) => 
        val tokens = stream.getTokens(i.a, i.b)
        val sl = tokens.asScala.head.getLine()
        val stl = tokens.asScala.last.getLine()
        // not working as expected : useless absolute char index
        // val sc = tokens.asScala.head.getStartIndex()
        // val stc = tokens.asScala.last.getStopIndex()
        if (sl == stl) {
          s"${path}:$sl"
        } else {
          s"${path}:$sl>>$stl"
        }
      case (None, _) => s"${path}:???"
    }
  }
  
  /**
    * Log msg at Error level
    * @param msg msg generator to be invoked if level is right
    */
  def fatal(n: SVNode, msg: => String): Unit = {
    fatal(s"$msg at ${getInfo(n)}")
  }
  /**
    * Log msg at Error level
    * @param msg msg generator to be invoked if level is right
    */
  def critical(n: SVNode, msg: => String): Unit = {
    critical(s"$msg at ${getInfo(n)}")
  }
  /**
    * Log msg at Warn level
    * @param msg msg generator to be invoked if level is right
    */
  def warn(n: SVNode, msg: => String): Unit = {
    warn(s"$msg at ${getInfo(n)}")
  }
  /**
    * Log msg at Info level
    * @param msg msg generator to be invoked if level is right
    */
  def info(n: SVNode, msg: => String): Unit = {
    info(s"$msg at ${getInfo(n)}")
  }
  /**
    * Log msg at Debug level
    * @param msg msg generator to be invoked if level is right
    */
  def debug(n: SVNode, msg: => String): Unit = {
    debug(s"$msg at ${getInfo(n)}")
  }
  /**
    * Log msg at Trace level
    * @param msg msg generator to be invoked if level is right
    */
  def trace(n: SVNode, msg: => String): Unit = {
    trace(s"$msg at ${getInfo(n)}")
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
  type RefStore = HashMap[String, FullType]
  
  var currentProject : Option[Project] = None
  var currentSourceFile : Option[SourceFile] = None
  var currentStream : Option[CommonTokenStream] = None
  val importedPackages = new ArrayBuffer[PackageRef]()
  private var refOutdated = true
  private val remoteRefsStore : RefStore = new HashMap[String, FullType]()
  
  def forceRefsRefresh(): Unit = {
    trace("forceRefresh")
    refOutdated = true
    currentProject.get.clearDescriptionCache()
    remoteRefsStore.clear()
  }
  
  def remoteRefs: RefStore = {
    if (refOutdated) {
      refOutdated = false
      for (p <- importedPackages) {
        debug(s"importedPackage: ${p.path}")
        currentProject.get.findDescription(p.path) match {
          case Some(d: DefPackage) =>
            d.refs match {
              case Some(h) => 
                if(p.allItems) { 
                  remoteRefsStore ++= h
                } else {
                  for ((k, v) <- h) {
                    remoteRefsStore += ((s"${p.path}.$k", v))
                  }
                }
              case _ => critical(s"No references found for package ${p.path}, did you properly set-ups the package refs before using remoteRefs inside a transform?")
            }
          case _ => critical(s"Unable to retrieve package declaration for package ref ${p.path}")
        }
        
      }
    }
    remoteRefsStore
  }
  
  def updateContext(desc: String): Unit = {
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
  
  private def processDescriptionWrapper(d: Description): Description = {
    d match {
      case IsolatedStatement(_, i: ImportPackages) => importedPackages ++= i.packages; refOutdated = true
      case _ =>
    }
    processDescription(d)
  }
  
  protected def execute(project: Project): Unit = {
    currentProject = Some(project)
    def processEntry(e: ProjectEntry, f: Description => Description): ProjectEntry = {
      importedPackages.clear()
      remoteRefsStore.clear()
      refOutdated = true
      currentSourceFile = Some(e.src)
      currentStream = Some(e.stream)
      e.copy(src = e.src.mapDescription(f))
    }
    
    // Note : project is mutable
    preprocessDescription match {
      case Some(f) => project.mapEntry(processEntry(_, f))
      case None =>
    }
    project.mapEntry(processEntry(_, processDescriptionWrapper))
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
