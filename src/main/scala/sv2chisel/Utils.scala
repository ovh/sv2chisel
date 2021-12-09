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
      case _: BoolLiteral => true
      case _: UIntLiteral => true
      case _: SIntLiteral => true
      case _: StringLit => true
      case _: Reference => true
      case _: DoCast => true
      case _: Concat => true
      case _: Number => true
      case _: DoCall => true
      case _: SubField => true
      case s: SubIndex => isSimple(s.expr)
      case s: SubRange => isSimple(s.expr)
      case DoPrim(_,PrimOps.InlineIf(_), _, HwExpressionKind, _) => true // Mux
      case DoPrim(_,PrimOps.Par(_), _, _, _) => true // Parenthesis
      case DoPrim(_,PrimOps.CeilLog2(_), _, _, _) => true
      case DoPrim(_,PrimOps.GetWidth(_), _, _, _) => true
      case _ => false
    }
  }
  
  def safeOperand(e: Expression): Expression = {
    isSimple(e) match {
      case true => e 
      case false => DoPrim(UndefinedInterval, PrimOps.Par(UndefinedInterval), Seq(e))
    }
  }
  
  def eqRawExpr(e1: Expression, e2: Expression): Boolean = {
    def cleanExpr(e: Expression): Expression = {
      e.mapType(_ => UnknownType()).mapInterval(_ => UndefinedInterval).mapKind(_ => UnknownExpressionKind)
    }
    (cleanExpr(e1), cleanExpr(e2)) match {
      case (d1: DoPrim, d2: DoPrim) if(d1.args.length != d2.args.length) => false
      case (d1: DoPrim, d2: DoPrim) => 
        Utils.eq(d1.op, d2.op) && 
          d1.args.zip(d2.args).map(t => eqRawExpr(t._1, t._2)).reduceOption(_ && _).getOrElse(false)

      case (e1Raw, e2Raw) => 
        val e1clean = e1Raw.mapExpr(cleanExpr)
        val e2clean = e2Raw.mapExpr(cleanExpr)
        val res = e1clean == e2clean
        if(!res) {
          trace(e1, s"Match Failed on")
          trace(e1, s"$e1clean")
          trace(e1, s"$e2clean")
        }
        res
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
          case r: Reference => r.copy(flow = UnknownFlow)
          case e => e 
        }
      
      case n: Statement => 
        n.mapExpr(cleanTok).mapType(cleanTok).mapStmt(cleanTok).mapVerilogAttributes(cleanTok) match {
          case p: Port => p.copy(direction = cleanTok(p.direction))
          case s => s
        }
        
      case n: AlwaysComb => n.copy(trig = cleanTok(n.trig))
      case n: AlwaysFF => n.copy(trig = cleanTok(n.trig))
      case n: UndefinedEventControl => n
      case n: Width => n.mapExpr(cleanTok)
      case n: Orientation => n
      case n: Field => n.copy(flip = cleanTok(n.flip), tpe = cleanTok(n.tpe))
      case n: Type => n.mapType(cleanTok).mapWidth(cleanTok)
      case n: Direction => n
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
    val clean1 = cleanTok(n1)
    val clean2 = cleanTok(n2)
    val res = clean1 == clean2
    trace(s"$res: ${n1.serialize} == ${n2.serialize}")
    trace(s" |-> Before cleaning ${n1} & ${n2}")
    trace(s" |-> After cleaning ($res) of ${clean1} & ${clean2}")
    res
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
    val throwable = getThrowable(exception, true)
    val string = if (message.nonEmpty) message + "\n" else message
    error("Internal Error! %sPlease file an issue at https://github.com/ovh/sv2chisel/".format(string), throwable)
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
  val bundleFieldsReservedNames = Seq(
    // public reserved names
    ":=", "<>", "asTypeOf", "asUInt", "autoSeed", "className", "cloneType", 
    "computeName", "equals", "getElements", "getWidth", "hasSeed", "hashCode", 
    "ignoreSeq", "instanceName", "isLit", "isWidthKnown", "litOption", "litValue", 
    "parentModName", "parentPathName", "pathName", "suggestName", "toAbsoluteTarget", 
    "toNamed", "toPrintable", "toString", "toTarget", "widthOption", "litArg",
    // private reserved names from Record
    "setElementRefs", "_makeLit", "toPrintableHelper",
    // private reserved name from Bundle
    "getBundleField", "_usingPlugin", "_outerInst", "_containingModule", "_containingBundles",
    "checkClone", "_cloneTypeImpl", 
    // private reserved name from Data
    "flatten", "_specifiedDirection", "specifiedDirection", "specifiedDirection_=",
    "_assignCompatibilityExplicitDirection", "_binding", "binding", "binding_=",
    "isSynthesizable", "topBindingOpt", "topBinding", "bind", "_direction", "direction", "direction_=",
    "bindingToString", "badConnect", "connect", "bulkConnect", "typeEquivalent", "requireVisible",
    "lref", "ref", "width", "legacyConnect", "allElements", "cloneTypeFull", "connectFromBits"
  )
  
  def legalBundleField(name: String): String = {
    if(bundleFieldsReservedNames.contains(name)) s"${name}_" else name
  }
}



