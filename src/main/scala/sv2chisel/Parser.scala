// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import org.antlr.v4.runtime._
import org.antlr.v4.runtime.atn._
import logger.EasyLogging
import sv2chisel.ir._
import sv2chisel.Utils.time
import sv2chisel.antlr._

class ParserException(message: String) extends SV2ChiselUserException(message)

case class ParameterNotSpecifiedException(message: String) extends ParserException(message)
case class ParameterRedefinedException(message: String) extends ParserException(message)
case class InvalidStringLitException(message: String) extends ParserException(message)
case class InvalidEscapeCharException(message: String) extends ParserException(message)
case class SyntaxErrorsException(message: String) extends ParserException(message)


object Parser extends EasyLogging {

  /** Parses a file in a given filename and returns a parsed [[sv2chisel.ir.SourceFile SourceFile]] */
  def parseFile(filename: String, basePath: String=""): (SourceFile, CommonTokenStream) = {
    val path = if (basePath != "") basePath + "/" + filename else filename
    parseCharStream(CharStreams.fromFileName(path), filename)
  }

  /** Parses a String and returns a parsed [[sv2chisel.ir.SourceFile SourceFile]] */
  def parseString(text: String, path: String = ""): (SourceFile, CommonTokenStream) =
    parseCharStream(CharStreams.fromString(text), path)

  /** Parses a org.antlr.v4.runtime.CharStream and returns a parsed [[sv2chisel.ir.SourceFile SourceFile]] */
  def parseCharStream(charStream: CharStream, path: String = ""): (SourceFile, CommonTokenStream) = {
    struct(s"############# Parsing $path #############")
    val (parseTimeMillis, (cst, tokens)) = time {
      val lexer = new sv2017Lexer(charStream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new sv2017Parser(tokens)

      parser.getInterpreter.setPredictionMode(PredictionMode.SLL) // seems a bit faster ??????

      // Concrete Syntax Tree
      val cst = parser.source_text

      val numSyntaxErrors = parser.getNumberOfSyntaxErrors
      if (numSyntaxErrors > 0) throw new SyntaxErrorsException(s"$numSyntaxErrors syntax error(s) detected")
      (cst, tokens)
    }
    
    val visitor = new Visitor(DelayedWarning, tokens, path)
    val (visitTimeMillis, visit) = time {
      visitor.visit(cst)
    }
    val ast = visit match {
      case c: SourceFile => c
      case x => throw new ClassCastException("Error! AST not rooted with SourceFile node!")
    }
    struct(s"######### Elapsed time for $path #########")
    struct(s"# Lexing+Parsing Time : $parseTimeMillis ms")
    struct(s"# Mapping to IR Time  : $visitTimeMillis ms")
    (ast, tokens)
  }
  /** Takes Iterator over lines of SV2Chisel, returns SVNode (root node is SourceFile) */
  def parse(lines: Iterator[String]): (SourceFile, CommonTokenStream) =
    parseString(lines.mkString("\n"))

  def parse(lines: Seq[String]): (SourceFile, CommonTokenStream) = parseString(lines.mkString("\n"))

  def parse(text: String): (SourceFile, CommonTokenStream) = parseString(text)

}
