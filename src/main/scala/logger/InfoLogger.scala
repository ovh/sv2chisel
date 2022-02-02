// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package logger

import org.antlr.v4.runtime.misc.Interval
import org.antlr.v4.runtime.{CommonTokenStream}
import scala.collection.JavaConverters._

trait HasPath {
  def path: String
}

trait InfoLogging extends InfoLogger with LazyLogging

trait InfoLogger extends EasyLogger {
  def currentSourceFile : Option[HasPath]
  def currentStream : Option[CommonTokenStream]
  
  def getAtInfo(i: Interval): String = {
    val path = currentSourceFile match {
      case Some(src) => s"at ${src.path}"
      case None => ""
    }
    (currentStream, i) match {
      case (_, i) if(i.a < 0) => path
      case (Some(stream), _) => 
        val tokens = stream.getTokens(i.a, i.b)
        val sl = tokens.asScala.head.getLine()
        val stl = tokens.asScala.last.getLine()
        val sc = tokens.asScala.head.getCharPositionInLine()
        val stc = tokens.asScala.last.getCharPositionInLine()
        if (sl == stl) {
          if (sc == stc) {
            s"${path}:$sl:$sc"
          } else {
            s"${path}:$sl:$sc->$stc"
          }
        } else {
          s"${path}:$sl:$sc>>$stl:$stc"
        }
      case (None, _) => path
    }
  }
  
  /**
    * Log msg at Error level
    * @param msg msg generator to be invoked if level is right
    */
  def fatal(n: Interval, msg: => String): Unit = {
    fatal(s"$msg ${getAtInfo(n)}")
  }
  /**
    * Log msg at Error level
    * @param msg msg generator to be invoked if level is right
    */
  def critical(n: Interval, msg: => String): Unit = {
    critical(s"$msg ${getAtInfo(n)}")
  }
  /**
    * Log msg at Warn level
    * @param msg msg generator to be invoked if level is right
    */
  def warn(n: Interval, msg: => String): Unit = {
    warn(s"$msg ${getAtInfo(n)}")
  }
  /**
    * Log msg at Info level
    * @param msg msg generator to be invoked if level is right
    */
  def info(n: Interval, msg: => String): Unit = {
    info(s"$msg ${getAtInfo(n)}")
  }
  /**
    * Log msg at Debug level
    * @param msg msg generator to be invoked if level is right
    */
  def debug(n: Interval, msg: => String): Unit = {
    debug(s"$msg ${getAtInfo(n)}")
  }
  /**
    * Log msg at Trace level
    * @param msg msg generator to be invoked if level is right
    */
  def trace(n: Interval, msg: => String): Unit = {
    trace(s"$msg ${getAtInfo(n)}")
  }
}
