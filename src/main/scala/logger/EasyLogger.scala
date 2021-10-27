// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package logger

trait EasyLogging extends LazyLogging with EasyLogger

trait EasyLogger {
  protected def logger : Logger
  
  // note: octal literals such as \033 are deprecated in scala
  private val red = "\u001b[31m"
  // private val green = "\u001b[32m"
  private val orange = "\u001b[33m"
  private val blue = "\u001b[34m"
  private val pink = "\u001b[35m"
  private val lightblue = "\u001b[36m"
  private val white = "\u001b[37m"
  private val end = "\u001b[0m"
  
  /**
    * Log msg at Error level
    * @param msg msg generator to be invoked if level is right
    */
  def fatal(msg: => String): Unit = {
    logger.error(s"[${red}fatal${end}] $msg")
  }
  /**
    * Log msg at Error level
    * @param msg msg generator to be invoked if level is right
    */
  def critical(msg: => String): Unit = {
    logger.error(s"[${pink}critical${end}] $msg")
  }
  /**
    * Log msg at Warn level
    * @param msg msg generator to be invoked if level is right
    */
  def warn(msg: => String): Unit = {
    logger.warn(s"[${orange}warn${end}] $msg")
  }
  /**
    * Log msg at Info level
    * @param msg msg generator to be invoked if level is right
    */
  def struct(msg: => String): Unit = {
    logger.info(s"[log] $msg")
  }
  /**
    * Log msg at Info level
    * @param msg msg generator to be invoked if level is right
    */
  def info(msg: => String): Unit = {
    logger.info(s"[${blue}info${end}] $msg")
  }
  /**
    * Log msg at Debug level
    * @param msg msg generator to be invoked if level is right
    */
  def debug(msg: => String): Unit = {
    logger.debug(s"[${lightblue}debug${end}] $msg")
  }
  /**
    * Log msg at Trace level
    * @param msg msg generator to be invoked if level is right
    */
  def trace(msg: => String): Unit = {
    logger.trace(s"[${white}trace${end}] $msg")
  }
  
  
}