// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import sv2chisel.Utils.time
import sv2chisel.chiselize._
import sv2chisel.ir.{UndefinedInterval, TechnicalInterval, Interval, SourceFile, SVNode}
import sv2chisel.antlr.{sv2017Lexer}

import logger._

import org.antlr.v4.runtime.{CommonTokenStream, Token}
import scala.collection.JavaConverters._
import collection.mutable.{ArrayBuffer}
import java.io.{File, FileWriter}

trait ChiselEmissionContext {
  val project: Project
  val options: TranslationOptions
  val stream: CommonTokenStream
  val src: SourceFile
  val srcBasePath: String
  val emissionBasePath: String
  val isHardware : Boolean 
  val isRawConnect : Boolean
  val indentLevel : Int
  val indent : String
  def legal(f: String => String): ChiselEmissionContext
  def safe(s: String): String
  def incr(n: Int = 1): ChiselEmissionContext 
  def decr(n: Int = 1): ChiselEmissionContext
  def raw(): ChiselEmissionContext
  def unraw(): ChiselEmissionContext
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
  options: TranslationOptions,
  emissionBasePath: String,
  indent: String,
  indentLevel: Int,
  isHardware: Boolean,
  isRawConnect: Boolean,
  stream: CommonTokenStream,
  src: SourceFile,
  srcBasePath: String,
  nameLegalizer: String => String
) extends ChiselEmissionContext {
  def legal(f: String => String): ChiselEmissionContext = this.copy(nameLegalizer = f)
  def safe(s: String): String = nameLegalizer(s)
  def incr(n: Int = 1): ChiselEmissionContext = {
    this.copy(indentLevel = indentLevel + n)
  }
  def decr(n: Int = 1): ChiselEmissionContext = {
    this.copy(indentLevel = indentLevel - n)
  }
  def hw(): ChiselEmissionContext = this.copy(isHardware = true)
  def sw(): ChiselEmissionContext = this.copy(isHardware = false)
  def raw(): ChiselEmissionContext = this.copy(isRawConnect = true)
  def unraw(): ChiselEmissionContext = this.copy(isRawConnect = false)
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
  def apply(project: Project, emissionBasePath: String, options: TranslationOptions): ScalaStyleEmission = {
    val pe = project.getEntries.head
    new ScalaStyleEmission(
      project = project,
      options = options,
      emissionBasePath = emissionBasePath,
      indent = "  ", 
      indentLevel = 0,
      isHardware = false,
      isRawConnect = false,
      stream = pe.stream,
      src = pe.src,
      srcBasePath = pe.basePath,
      nameLegalizer = s => s
    )
  }
}

object Emitter extends EasyLogging {

  def emitChisel(ctx: ChiselEmissionContext, noFileIO: Boolean = false): String = {
    ctx.project.getEntries.map(e => {
      struct(s"######### CHISELIZING ${e.src.path} #########")
      val (timeChiselize, chisel) = time {
        e.src.chiselize(ctx.setFile(e.src, e.stream))
      }
      struct(s"# Elapsed time : $timeChiselize ms")
      
      val emissionPath = ctx.emissionBasePath + "/" + e.src.path.split('.').dropRight(1).mkString("",".",".scala")
      
      val dry = if(noFileIO) s" [DRY RUN]" else ""
      
      struct(s"#########$dry EMITTING to ${emissionPath} #########")
      val (timeEmit, chiselTxt) = time {
        val chiselTxt = emit(ctx, chisel, e.stream)
        trace(chiselTxt)
        if(!noFileIO){          
          emissionPath.split("/").dropRight(1) match {
            case Array() => 
            case a => 
            val dirs = new File(a.mkString("","/",""))
            dirs.mkdirs()
          }
          
          val writer = new FileWriter(emissionPath)
          writer.write(chiselTxt)
          writer.close()
        }
        chiselTxt
      }
      struct(s"# Elapsed time : $timeEmit ms")
      chiselTxt
    }).mkString("/////////////\n","\n\n/////////////\n", "\n")
  }
  
  
  private def emit(ctx: ChiselEmissionContext, chisel: Seq[ChiselTxt], stream: CommonTokenStream): String = {
    
    def emitRaw(t: ChiselTxt, lastWasSingleLineComment: Boolean): String = {
      if(t.newLine || lastWasSingleLineComment) {
        "\n" + ctx.indent * t.indentLevel + t.txt 
      } else {
        t.txt
      }
    }
    
    def retrieveHidden(low: Int, high: Int, expIndent: Int, newLine: Boolean, stopAt1stMain: Boolean): (Int, String) = {
      val str = ArrayBuffer[String]()
      val ws = ArrayBuffer[String]()
      var startFromNewLineNext = low == 0 // first token always start from new line
      var startFromNewLine = false // won't be used

      def getSpace: String = if(startFromNewLine) ctx.indent * expIndent else " "
      
      def processToken(tok: Token, last: Boolean = false): (Boolean, Boolean) = {
        startFromNewLine = startFromNewLineNext
        tok.getChannel() match {
          case sv2017Lexer.COMMENTS => 
            // in-line comment always integrate a EOL within the text
            val eol = tok.getType() match {
              case sv2017Lexer.ONE_LINE_COMMENT => startFromNewLineNext = true; "\n"
              case sv2017Lexer.BLOCK_COMMENT => ""
              case _ => Utils.throwInternalError("Lexing error")
            }
            // remove trailling new line
            val txt = tok.getText().split("\n").mkString("","\n","")
            val spaces = ws.mkString
            val newlineCount = spaces.count(_=='\n')
            str += ((spaces.split("\n"), newlineCount) match {
              case (Array(), 0) => 
                trace(s"case 0")
                getSpace + tok.getText()
              
              case (Array(), n) => 
                trace(s"case 1 n=$n");
                val updatedTxt = tok.getText().split("\n").map(l => {
                  ctx.indent * expIndent + l
                }).mkString("\n")
                val trailling = "\n"*(tok.getText().count(_=='\n') - updatedTxt.count(_=='\n'))
                "\n"*n + updatedTxt + trailling
                
              case (Array(s@_), 0) => // most common case for single-line comments
                trace(s"case 2")
                getSpace + tok.getText()
                
              case (Array(s@_), 1) => 
                trace(s"case 3")
                " " + tok.getText().split("\n").mkString("","\n","\n")
                
              case (Array(a@_, b), 1) => 
                trace(s"case 4")
                val updatedTxt = txt.split("\n").map(l => {
                  l.replaceAll("^[ ]{"+ b.size +"}", ctx.indent * expIndent)
                }).mkString("\n")
                "\n" + ctx.indent * expIndent + updatedTxt
                
              case (a, n) => 
                trace(s"case 5")
                "\n"*n + a.last.replace("    ", "  ") + txt + eol
            })
            ws.clear()
            (true, false)
            
          case sv2017Lexer.WHITESPACES => 
            ws += tok.getText()
            if(last && newLine){
              val spaces = ws.mkString
              // compensate the one which will get suppressed upon newLine
              val newlineCount = spaces.count(_=='\n') + (if(newLine) 1 else 0)
              trace(s"Reintegrating $newlineCount `\\n`")
              str += "\n"*newlineCount
              ws.clear()
              (true, false)
            } else {
              (false, false) // not emitted
            }
            
          case _ => 
            ws.clear() // prevent carrying unwanted whitespaces between tokens
            val stop = tok.getType match {
              case sv2017Lexer.SEMI => false // ignore
              case _ => stopAt1stMain 
            }
            
            if(stop) debug(s"Stoping at `${tok.getText}`")
            (false, stop)
        }
      }
      
      var emittedUntil = low
      if(high < low){ // process to end
        stream.getHiddenTokensToRight(low) match {
          case null =>
          case s => s.asScala.foreach(t => processToken(t))
        }
      } else {
        var i = low
        var stop = false
        while (i < high && !stop) {
          val (emitted, stp) = processToken(stream.get(i), high-1 == i)
          emittedUntil = if(emitted) i else emittedUntil
          stop = stp
          i += 1
        }
      }
      
      // remove last newline if necessary
      val result = newLine match {
        case true => str.mkString.replaceAll("\\n[ ]*$", "")
        case _ => str.mkString
      }
      
      (emittedUntil, result)
    }
    
    
    val emitted = ArrayBuffer[String]()
    var emittedUntil = -1
    val maxToken = stream.size()
    // Logger.setLevel(LogLevel.Trace)
    
    for (cToken <- chisel) {
      trace(cToken.toString)
      
      val lastWasSingleLineComment = cToken.tokens match {
        case UndefinedInterval => false
        // case i: Interval => 
        case i: Interval if(i.b > emittedUntil && emittedUntil+1 < maxToken) => 
          // first collect hidden comments & \n && integrate it
          val (low, high, stopAt1stMain) = i match {
            case TechnicalInterval(max) => (emittedUntil + 1, max, true)
            case _ => (emittedUntil + 1, i.a, false)
          }
          
          // TODO: address root cause & remove this check
          // only one example of this behaviour on the current corpus
          if(i.a > maxToken){
            critical(s" Unexpected interval !! maxToken: $maxToken i.a: ${i.a}, low: ${low}")
            emitted += "???"
            critical(cToken.toString)
            false
          } else {            
            val (h, txt) = retrieveHidden(low, high, cToken.indentLevel, cToken.newLine, stopAt1stMain)
            trace(s" |-> low = $low ; high = $high ==> h= $h (${stream.get(h).getType}: `${stream.get(h).getText}`)")
            trace(s" |-> hidden retrieved: `${txt}`")
            emitted += txt
            // second increase the emittedUntil
            emittedUntil = Seq(h, i.a).max
            stream.get(h).getType match {
              case sv2017Lexer.ONE_LINE_COMMENT => true
              case _ => false
            }
          }
        case _ => false
      }
      emitted += emitRaw(cToken, lastWasSingleLineComment)
    }
    // last call to retrieve hidden til EOF (if not already reached)
    if(emittedUntil+1 < maxToken){      
      val (_, txt) = retrieveHidden(emittedUntil+1, -1, 0, false, false)
      emitted += txt
    }
    emitted.mkString
  }
}
