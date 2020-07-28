// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

// Freely inspired by firrtl Utils.scala
// Originally retrieved from https://github.com/freechipsproject/firrtl/ March 2020

package sv2chisel

import sv2chisel.ir._
import sv2chisel.transforms.InfoLogger
import logger.{LazyLogging}
import org.antlr.v4.runtime.{CommonTokenStream}

import scala.collection.mutable
import scala.util.matching.Regex

object Utils extends LazyLogging with InfoLogger {
  var currentSourceFile : Option[SourceFile] = None
  var currentStream : Option[CommonTokenStream] = None
  
  def getStackHere(filter: String): String = {
    val stack = new Exception(s"Stack at $filter").getStackTrace()
    stack.collect { case e if (e.toString.contains(filter)) => e.toString }
      .mkString("\n    ", "\n    ", "")
  }
  
  def printStackHere(filter: String): Unit = {
    println(getStackHere(filter))
  }
  
  def isSimple(e: Expression): Boolean = {
    e match {
      case r: StringLit => true
      case r: Reference => true
      case d: DoCast => true
      case r: Number => true
      case s: SubIndex => isSimple(s.expr)
      case s: SubRange => isSimple(s.expr)
      case DoPrim(_,PrimOps.InlineIf(_), _, HwExpressionKind, _) => true // Mux
      case _ => false
    }
  }
  
  def safeOperand(e: Expression): Expression = {
    isSimple(e) match {
      case true => e 
      case false => DoPrim(UndefinedInterval, PrimOps.Par(UndefinedInterval), Seq(e))
    }
  }
  
  def cleanTok[T <: SVNode](node: T): T = {
    (node.mapInterval(i => {
      i match {
        case UndefinedInterval =>
        case _ => trace(node, s"Cleaning interval $i for node ${node.serialize}") 
      }
      UndefinedInterval
    }) match {
      case n: SourceFile => n.mapDescription(cleanTok)
      case n: Description => n.mapStmt(cleanTok).mapVerilogAttributes(cleanTok)
      case n: PackageRef => n
      case n: VerilogAttributes => n
      case n: PrimOp => n
      case n: Expression => n.mapExpr(cleanTok).mapType(cleanTok) match {
          case d: DoPrim => d.copy(op = cleanTok(d.op))
          case e => e 
        }
      
      case n: Statement => n.mapExpr(cleanTok).mapType(cleanTok).mapStmt(cleanTok)
      case n: AlwaysComb => n.copy(trig = cleanTok(n.trig))
      case n: AlwaysFF => n.copy(trig = cleanTok(n.trig))
      case n: UndefinedEventControl => n
      case n: Width => n.mapExpr(cleanTok)
      case n: Orientation => n
      case n: Field => n.copy(flip = cleanTok(n.flip), tpe = cleanTok(n.tpe))
      case n: Type => n.mapType(cleanTok).mapWidth(cleanTok)
      case n: Direction => n
      case n: Port => n.copy(
        attributes = cleanTok(n.attributes),
        direction = cleanTok(n.direction),
        tpe = cleanTok(n.tpe),
        init = cleanTok(n.init),
        clock = cleanTok(n.clock),
        reset = cleanTok(n.reset)
      )
      case n: StringParam => n.copy(value = cleanTok(n.value))
      case n: Param => n
    }).asInstanceOf[T]
  }
  
  def cleanTokens[T <: SVNode](entry: T)(implicit src : Option[SourceFile], stream : Option[CommonTokenStream]): T = {
    this.currentSourceFile = src
    this.currentStream = stream
    
    cleanTok(entry)
  }
  
  def eq[T <: SVNode](n1: T, n2: T): Boolean = {
    cleanTok(n1) == cleanTok(n2)
  }
  
  
  /** Unwind the causal chain until we hit the initial exception (which may be the first).
    *
    * @param maybeException - possible exception triggering the error,
    * @param first - true if we want the first (eldest) exception in the chain,
    * @return first or last Throwable in the chain.
    */
  def getThrowable(maybeException: Option[Throwable], first: Boolean): Throwable = {
    maybeException match {
      case Some(e: Throwable) => {
        val t = e.getCause
        if (t != null) {
          if (first) {
            getThrowable(Some(t), first)
          } else {
            t
          }
        } else {
          e
        }
      }
      case None | null => null
    }
  }

  /** Throw an internal error, possibly due to an exception.
    *
    * @param message - possible string to emit,
    * @param exception - possible exception triggering the error.
   */
  def throwInternalError(message: String = "", exception: Option[Exception] = None) = {
    // We'll get the first exception in the chain, keeping it intact.
    val first = true
    val throwable = getThrowable(exception, true)
    val string = if (message.nonEmpty) message + "\n" else message
    error("Internal Error! %sPlease file an issue at ...".format(string), throwable)
  }

  def time[R](block: => R): (Double, R) = {
    val t0 = System.nanoTime()
    val result = block
    val t1 = System.nanoTime()
    val timeMillis = (t1 - t0) / 1000000.0
    (timeMillis, result)
  }


  def error(str: String, cause: Throwable = null) = throw new SV2ChiselInternalException(str, cause)

  /** Indent the results of [[ir.FirrtlNode.serialize]] */
  def indent(str: String) = str replaceAllLiterally ("\n", "\n  ")
  
  def getSIntWidth(s: BigInt): Int = s.bitLength + 1
  def getUIntWidth(u: BigInt): Int = u.bitLength
  def dec2string(v: BigDecimal): String = v.underlying().stripTrailingZeros().toPlainString
  def trim(v: BigDecimal): BigDecimal = BigDecimal(dec2string(v))
  def max(a: BigInt, b: BigInt): BigInt = if (a >= b) a else b
  def min(a: BigInt, b: BigInt): BigInt = if (a >= b) b else a
  def pow_minus_one(a: BigInt, b: BigInt): BigInt = a.pow(b.toInt) - 1
  val one  = UIntLiteral(UndefinedInterval, 1)
  val zero = UIntLiteral(UndefinedInterval, 0)
  
  
  // TO DO : adapat this logic for scala keywords
  val v_keywords = Set(
    "alias", "always", "always_comb", "always_ff", "always_latch",
    "and", "assert", "assign", "assume", "attribute", "automatic",

    "before", "begin", "bind", "bins", "binsof", "bit", "break",
    "buf", "bufif0", "bufif1", "byte",

    "case", "casex", "casez", "cell", "chandle", "checker", "class", "clocking",
    "cmos", "config", "const", "constraint", "context", "continue",
    "cover", "covergroup", "coverpoint", "cross",

    "deassign", "default", "defparam", "design", "disable", "dist", "do",

    "edge", "else", "end", "endattribute", "endcase", "endclass",
    "endclocking", "endconfig", "endfunction", "endgenerate",
    "endgroup", "endinterface", "endmodule", "endpackage",
    "endprimitive", "endprogram", "endproperty", "endspecify",
    "endsequence", "endtable", "endtask",
    "enum", "event", "expect", "export", "extends", "extern",

    "final", "first_match", "for", "force", "foreach", "forever",
    "fork", "forkjoin", "function",
    "generate", "genvar",
    "highz0", "highz1",
    "if", "iff", "ifnone", "ignore_bins", "illegal_bins", "import",
    "incdir", "include", "initial", "initvar", "inout", "input",
    "inside", "instance", "int", "integer", "interconnect",
    "interface", "intersect",

    "join", "join_any", "join_none", "large", "liblist", "library",
    "local", "localparam", "logic", "longint",

    "macromodule", "matches", "medium", "modport", "module",

    "nand", "negedge", "new", "nmos", "nor", "noshowcancelled",
    "not", "notif0", "notif1", "null",

    "or", "output",

    "package", "packed", "parameter", "pmos", "posedge",
    "primitive", "priority", "program", "property", "protected",
    "pull0", "pull1", "pulldown", "pullup",
    "pulsestyle_onevent", "pulsestyle_ondetect", "pure",

    "rand", "randc", "randcase", "randsequence", "rcmos",
    "real", "realtime", "ref", "reg", "release", "repeat",
    "return", "rnmos", "rpmos", "rtran", "rtranif0", "rtranif1",

    "scalared", "sequence", "shortint", "shortreal", "showcancelled",
    "signed", "small", "solve", "specify", "specparam", "static",
    "strength", "string", "strong0", "strong1", "struct", "super",
    "supply0", "supply1",

    "table", "tagged", "task", "this", "throughout", "time", "timeprecision",
    "timeunit", "tran", "tranif0", "tranif1", "tri", "tri0", "tri1", "triand",
    "trior", "trireg", "type","typedef",

    "union", "unique", "unsigned", "use",

    "var", "vectored", "virtual", "void",

    "wait", "wait_order", "wand", "weak0", "weak1", "while",
    "wildcard", "wire", "with", "within", "wor",

    "xnor", "xor",

    "SYNTHESIS",
    "PRINTF_COND",
    "VCS")

  /** Expand a name into its prefixes, e.g., 'foo_bar__baz' becomes 'Seq[foo_, foo_bar__, foo_bar__baz]'. This can be used
    * to produce better names when generating prefix unique names.
    * @param name a signal name
    * @param prefixDelim a prefix delimiter (default is "_")
    * @return the signal name and any prefixes
    */
  def expandPrefixes(name: String, prefixDelim: String = "_"): Seq[String] = {
    val regex = ("(" + Regex.quote(prefixDelim) + ")+[A-Za-z0-9$]").r

    name +: regex
      .findAllMatchIn(name)
      .map(_.end - 1)
      .toSeq
      .foldLeft(Seq[String]()){ case (seq, id) => seq :+ name.splitAt(id)._1 }
  }
}

/**
  * Maintains a one to many graph of each modules instantiated child module.
  * This graph can be searched for a path from a child module back to one of
  * it's parents.  If one is found a recursive loop has happened
  * The graph is a map between the name of a node to set of names of that nodes children
  */
class ModuleGraph {
  val nodes = mutable.HashMap[String, mutable.HashSet[String]]()

  /**
    * Add a child to a parent node
    * A parent node is created if it does not already exist
    *
    * @param parent module that instantiates another module
    * @param child  module instantiated by parent
    * @return a list indicating a path from child to parent, empty if no such path
    */
  def add(parent: String, child: String): List[String] = {
    val childSet = nodes.getOrElseUpdate(parent, new mutable.HashSet[String])
    childSet += child
    pathExists(child, parent, List(child, parent))
  }

  /**
    * Starting at the name of a given child explore the tree of all children in depth first manner.
    * Return the first path (a list of strings) that goes from child to parent,
    * or an empty list of no such path is found.
    *
    * @param child  starting name
    * @param parent name to find in children (recursively)
    * @param path   path being investigated as possible route
    * @return
    */
  def pathExists(child: String, parent: String, path: List[String] = Nil): List[String] = {
    nodes.get(child) match {
      case Some(children) =>
        if(children(parent)) {
          parent :: path
        }
        else {
          children.foreach { grandchild =>
            val newPath = pathExists(grandchild, parent, grandchild :: path)
            if(newPath.nonEmpty) {
              return newPath
            }
          }
          Nil
        }
      case _ => Nil
    }
  }
}
