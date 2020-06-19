// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import sv2chisel.Utils.time
import sv2chisel.chiselize._
import sv2chisel.ir.{UndefinedInterval, Interval, SourceFile, SVNode}
import sv2chisel.antlr.{sv2017Lexer}

import logger.EasyLogging

import org.antlr.v4.runtime.{CommonTokenStream}
import scala.collection.JavaConverters._
import collection.mutable.{ArrayBuffer}
import java.io.{File, FileWriter}

trait ChiselEmissionContext {
  val project: Project
  val stream: CommonTokenStream
  val src: SourceFile
  val srcBasePath: String
  val emissionBasePath: String
  val isHardware : Boolean 
  val indentLevel : Int
  val indent : String
  val toCamelCase : Boolean
  def incr(n: Int = 1): ChiselEmissionContext 
  def decr(n: Int = 1): ChiselEmissionContext
  def hw(): ChiselEmissionContext
  def sw(): ChiselEmissionContext
  def setStream(s: CommonTokenStream): ChiselEmissionContext
  def setSrc(s: SourceFile): ChiselEmissionContext
  def setFile(src: SourceFile, stream: CommonTokenStream): ChiselEmissionContext
  
  def getInfo(i: Interval): String
  def getInfo(n: SVNode): String
}

case class ScalaStyleEmission(
  project: Project, 
  emissionBasePath: String,
  indent: String,
  indentLevel: Int,
  isHardware: Boolean,
  stream: CommonTokenStream,
  src: SourceFile,
  srcBasePath: String
) extends ChiselEmissionContext {
  val toCamelCase = true
  def incr(n: Int = 1): ChiselEmissionContext = {
    this.copy(indentLevel = indentLevel + n)
  }
  def decr(n: Int = 1): ChiselEmissionContext = {
    this.copy(indentLevel = indentLevel - n)
  }
  def hw(): ChiselEmissionContext = this.copy(isHardware = true)
  def sw(): ChiselEmissionContext = this.copy(isHardware = false)
  def setStream(s: CommonTokenStream) = this.copy(stream = s)
  def setSrc(s: SourceFile) = this.copy(src = s)
  def setFile(src: SourceFile, stream: CommonTokenStream) = this.copy(src = src, stream = stream)
  
  def getInfo(n: SVNode): String = getInfo(n.tokens)
  def getInfo(i: Interval): String = {
    i match {
      case UndefinedInterval => s"${src.path}:???"
      case _ => 
        val tokens = stream.getTokens(i.a, i.b)
        val sl = tokens.asScala.head.getLine()
        val stl = tokens.asScala.last.getLine()
        // not working as expected : useless absolute char index
        // val sc = tokens.asScala.head.getStartIndex()
        // val stc = tokens.asScala.last.getStopIndex()
        if (sl == stl) {
          s"${src.path}:$sl"
        } else {
          s"${src.path}:$sl>>$stl"
        }
    }
  }
}

object ScalaStyleEmission {
  def apply(project: Project, emissionBasePath: String): ScalaStyleEmission = {
    val pe = project.getEntries.head
    new ScalaStyleEmission(
      project = project,
      emissionBasePath = emissionBasePath,
      indent = "  ", 
      indentLevel = 0,
      isHardware = false,
      stream = pe.stream,
      src = pe.src,
      srcBasePath = pe.basePath
    )
  }
}

object Emitter extends EasyLogging {

  def emitChisel(ctx: ChiselEmissionContext): String = {
    ctx.project.getEntries.map(e => {
      struct(s"######### CHISELIZING ${e.src.path} #########")
      val (timeChiselize, chisel) = time {
        e.src.chiselize(ctx.setFile(e.src, e.stream))
      }
      struct(s"# Elapsed time : $timeChiselize ms")
      
      val emissionPath = ctx.emissionBasePath + "/" + e.src.path.split('.').dropRight(1).mkString("",".",".scala")
      
      struct(s"######### EMITTING to ${emissionPath} #########")
      val (timeEmit, chiselTxt) = time {
        val chiselTxt = emit(ctx, chisel, e.stream)
        trace(chiselTxt)
        emissionPath.split("/").dropRight(1) match {
          case Array() => 
          case a => 
            val dirs = new File(a.mkString("","/",""))
            dirs.mkdirs()
        }
        
        val writer = new FileWriter(emissionPath)
        writer.write(chiselTxt)
        writer.close()
        chiselTxt
      }
      struct(s"# Elapsed time : $timeEmit ms")
      chiselTxt
    }).mkString("/////////////\n","\n\n/////////////\n", "\n")
  }
  
  
  private def emit(ctx: ChiselEmissionContext, chisel: Seq[ChiselTxt], stream: CommonTokenStream): String = {
    
    def emitRaw(t: ChiselTxt): String = {
      if(t.newLine) {
        "\n" + ctx.indent * t.indentLevel + t.txt 
      } else {
        t.txt
      }
    }
    
    def retrieveHidden(low: Int, high: Int, expIndent: Int): (Int, String) = {
      val str = ArrayBuffer[String]()
      val ws = ArrayBuffer[String]()
      
      var emittedUntil = low
      for (i <- (low until high)) {
        val tok = stream.get(i)
        tok.getChannel() match {
          case sv2017Lexer.COMMENTS => 
            // remove trailling new line
            val txt = tok.getText().split("\n").mkString("","\n","")
            val spaces = ws.mkString
            val newlineCount = spaces.count(_=='\n')
            str += ((spaces.split("\n"), newlineCount) match {
              case (Array(), n) => "\n"*n + tok.getText()
              case (Array(s), 0) => " " + tok.getText()
              case (Array(s), 1) => " " + tok.getText().split("\n").mkString("","\n","\n")
              case (Array(a, b), 1) => 
                if (b.size > (ctx.indent * 6 * expIndent).size) {
                  // remove emitter glitch (many ws retrieved due to poor interval management)
                  " " + tok.getText()
                } else {
                  "\n" + b.replace("    ", "  ") + txt
                }
              case (a, n) => "\n"*n + a.last.replace("    ", "  ") + txt
            })
            ws.clear()
            emittedUntil = i
            
          case sv2017Lexer.WHITESPACES => 
            ws += tok.getText()
            emittedUntil = i
            
          case _ => // ignore
        }
      }
      (emittedUntil, str.mkString)
    }
    
    
    val emitted = ArrayBuffer[String]()
    var emittedUntil = 0
    
    for (cToken <- chisel) {
      
      cToken.tokens match {
        case UndefinedInterval => 
        case i: Interval => 
          // first collect hidden comments & \n && integrate it
          val (h, txt) = retrieveHidden(emittedUntil, i.a, cToken.indentLevel)
          emitted += txt
          // second increase the emittedUntil
          emittedUntil = h
      }
      emitted += emitRaw(cToken)
    }
    emitted.mkString
  }
}
