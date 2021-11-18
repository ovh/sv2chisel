// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import sv2chisel.ir._
import sv2chisel.transforms.{Transform}
import sv2chisel.Utils.{time}

import logger.EasyLogging

import org.antlr.v4.runtime.{CommonTokenStream}
import collection.mutable.{HashMap, ArrayBuffer}
import scala.annotation.tailrec

case class ProjectEntry(
  basePath: String,
  src: SourceFile,
  stream: CommonTokenStream,
  blackboxes: Boolean
)

class Project(name: String) extends EasyLogging {
  
  /** 
    * SOURCES MANAGEMENT PART
    */ 
  private val sources = ArrayBuffer[ProjectEntry]()
  
  def getEntries: Seq[ProjectEntry] = sources.toSeq 
  
  def addEntry(e: ProjectEntry): Unit = sources += e
  
  def addFiles(files: Seq[String], basePath: String = "", blackboxes: Boolean = false): Unit = {
    files.foreach(f => {
      val (src, tokens) = Parser.parseFile(f, basePath, blackboxes)
      src.foreachDescription(d => {
        d match {
          case IsolatedStatement(_, IncludeHeader(_, s)) => addFiles(Seq(s))
          // to do : hierarchy mode 
          // add submodules files found in path & not already in project
          // same for packages 
          // ....
          case _ => 
        }
      })
      sources += ProjectEntry(basePath, src, tokens, blackboxes = blackboxes)
    })
  }
  
  /** 
    * Processing part
    */ 
  
  def foreachDescription(f: Description => Unit): Unit = sources.foreach(_.src.foreachDescription(f))
  def foreachEntry(f: ProjectEntry => Unit): Unit = sources.foreach(f)
  
  // NB: always refer to latest sources values (do not use zipWithIndex for example)
  def mapDescription(f: Description => Description): Unit =  
    (0 until sources.length).foreach(i => sources(i) = sources(i).copy(src = sources(i).src.mapDescription(f)))
  
  def mapEntry(f: ProjectEntry => ProjectEntry): Unit = 
    (0 until sources.length).foreach(i => sources(i) = f(sources(i)))
  
  /** 
    * RUN TRANSFORMS 
    */
  
  def runTimed(t: Transform): Unit = {
    struct(s"   ####### ${t.getClass.getName} #######")
    val (timeT, _) = time {
      t.execute(this)
    }
    trace("Transform result: " + this.serialize)
    struct(s"   # Elapsed time : $timeT ms")
  }
  
  @tailrec
  final def run(t: Seq[Transform]): Unit = {
    t match {
      case Seq() => 
      case s => runTimed(s.head); run(s.tail) 
    }
  }
  
  
  /** 
    * CACHE PART
    */ 
  private val descriptionCache = HashMap[String, Description]()
  private var cached = false
  
  def findDescription(des: String): Option[Description] = {
    if(cached){
      debug(s"Keys in description cache: ${descriptionCache.keys}")
      if(descriptionCache.contains(des)){
        Some(descriptionCache(des))
      } else {
        None
      }
    } else {
      cached = true
      sources.map(_.src.descriptions.collect {
        case n: HasName => n
      }).flatten.foreach {( d => {
          if (descriptionCache.contains(d.name)) {
            Utils.throwInternalError(s"Multiple descriptions with Name ${d.name} within project $name")
          }
          descriptionCache += ((d.name, d))
        }
      )}
      findDescription(des)
    }
  }
  
  def findModule(des: String): Option[DefModule] = {
    findDescription(des) match {
      case None => None
      case Some(d: DefModule) => Some(d)
      case _ => None
    }
  }
  
  /** NB: might conflict with current transform if affecting the same ProjectEntry */
  def updateDescription(name: String, f: Description => Description): Unit = {
    var count = 0
    sources.zipWithIndex.map(t => {
      val e = t._1.copy(src = t._1.src.mapDescription(d => {
        d match {
          case des: HasName if (des.name == name) =>
            val updated = f(des)
            count += 1
            if(cached)
              descriptionCache(name) = updated
            updated
          case _ => d 
        }
      }))
      sources(t._2) = e
    })
    count match {
      case 0 => critical(s"Nothing updated as description $name was not found.") 
      case 1 => // OK
      case _ => critical(s"More than one description ($count) was affected by the update of description $name.") 
    }
  }
  
  def isProjectDescription(des: String): Boolean = {
    findDescription(des) match {
      case None => false 
      case _ => true
    }
  }
  
  def isProjectModule(module: String): Boolean = {
    findDescription(module) match {
      case None => false 
      case Some(_: DefModule) => true
      case _ => false
    }
  }
  
  def clearDescriptionCache(): Unit = {
    cached = false
    descriptionCache.clear()
  }
  
  def serialize : String = sources.map(_.src.serialize).mkString("\n")
  
}

object Project {
  def apply(name: String, basePath: String, files: Seq[String], blackboxes: Seq[String] = Seq()): Project = {
    val p = new Project(name)
    p.addFiles(files, basePath, blackboxes = false)
    p.addFiles(blackboxes, basePath, blackboxes = true)
    p
  }
  // Minimal version for single source tests
  def apply(name: String, rawVerilog: String, path: Option[String]) = {
    val p = new Project(name)
    val (src, stream) = Parser.parseString(rawVerilog, path)
    p.addEntry(ProjectEntry("raw", src, stream, blackboxes = false))
    p
  }
  def apply(name: String, rawBlackboxes: String, rawVerilog: String, path: Option[String]) = {
    val p = new Project(name)
    val (srcB, streamB) = Parser.parseString(rawBlackboxes, path, blackboxes = true)
    p.addEntry(ProjectEntry("raw", srcB, streamB, blackboxes = true))
    val (src, stream) = Parser.parseString(rawVerilog, path)
    p.addEntry(ProjectEntry("raw", src, stream, blackboxes = false))
    p
  }
}
