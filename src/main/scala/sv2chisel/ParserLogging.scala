// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import org.antlr.v4.runtime.{ParserRuleContext,CommonTokenStream}
import org.antlr.v4.runtime.tree.{TerminalNode}
import scala.collection.JavaConverters._

import logger.EasyLogging

import sv2chisel.ir.{UndefinedInterval, Interval}

trait ParserLogging extends EasyLogging {
  def tokenStream: CommonTokenStream
  def pathInfo: String
  
  def getInfo(ctx: ParserRuleContext): String = {
    getInfo(ctx.getSourceInterval())
  }
  
  def getInfo(n: TerminalNode): String = {
    getInfo(n.getSourceInterval())
  }
  
  def getInfo(i: Interval): String = {
    i match {
      case UndefinedInterval => s"${pathInfo}:???"
      case _ => 
      val tokens = tokenStream.getTokens(i.a, i.b)
      val sl = tokens.asScala.head.getLine()
      val stl = tokens.asScala.last.getLine()
      // not working as expected : useless absolute char index
      // val sc = tokens.asScala.head.getStartIndex()
      // val stc = tokens.asScala.last.getStopIndex()
      if (sl == stl) {
        s"${pathInfo}:$sl"
      } else {
        s"${pathInfo}:$sl>>$stl"
      }
    }
  }
  
  def fatal(ctx: ParserRuleContext, msg: => String): Unit = fatal(s"At ${getInfo(ctx)}: $msg")
  def fatal(n: TerminalNode, msg: => String): Unit = fatal(s"At ${getInfo(n)}: $msg")
  def fatal(i: Interval, msg: => String): Unit = fatal(s"At ${getInfo(i)}: $msg")
  
  def critical(ctx: ParserRuleContext, msg: => String): Unit = critical(s"At ${getInfo(ctx)}: $msg")
  def critical(n: TerminalNode, msg: => String): Unit = critical(s"At ${getInfo(n)}: $msg")
  def critical(i: Interval, msg: => String): Unit = critical(s"At ${getInfo(i)}: $msg")
  
  def warn(ctx: ParserRuleContext, msg: => String): Unit = warn(s"At ${getInfo(ctx)}: $msg")
  def warn(n: TerminalNode, msg: => String): Unit = warn(s"At ${getInfo(n)}: $msg")
  def warn(i: Interval, msg: => String): Unit = warn(s"At ${getInfo(i)}: $msg")
  
  def struct(ctx: ParserRuleContext, msg: => String): Unit = struct(s"At ${getInfo(ctx)}: $msg")
  def struct(n: TerminalNode, msg: => String): Unit = struct(s"At ${getInfo(n)}: $msg")
  def struct(i: Interval, msg: => String): Unit = struct(s"At ${getInfo(i)}: $msg")
  
  def info(ctx: ParserRuleContext, msg: => String): Unit = info(s"At ${getInfo(ctx)}: $msg")
  def info(n: TerminalNode, msg: => String): Unit = info(s"At ${getInfo(n)}: $msg")
  def info(i: Interval, msg: => String): Unit = info(s"At ${getInfo(i)}: $msg")
  
  def debug(ctx: ParserRuleContext, msg: => String): Unit = debug(s"At ${getInfo(ctx)}: $msg")
  def debug(n: TerminalNode, msg: => String): Unit = debug(s"At ${getInfo(n)}: $msg")
  def debug(i: Interval, msg: => String): Unit = debug(s"At ${getInfo(i)}: $msg")
  
  def trace(ctx: ParserRuleContext, msg: => String): Unit = trace(s"At ${getInfo(ctx)}: $msg")
  def trace(n: TerminalNode, msg: => String): Unit = trace(s"At ${getInfo(n)}: $msg")
  def trace(i: Interval, msg: => String): Unit = trace(s"At ${getInfo(i)}: $msg")
}