// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

// Freely inspired by firrtl IR.scala
// Originally retrieved from https://github.com/freechipsproject/firrtl/ March 2020

package sv2chisel
package ir

import sv2chisel.ir.evalExpression._
import Utils.{indent}

import collection.mutable.{HashMap, HashSet, ArrayBuffer}

case object UndefinedInterval extends Interval(-1,-1) {
  override def toString : String = "UndefinedInterval"
}
case class TechnicalInterval(r: Int) extends Interval(-1,r) {
  override def toString : String = s"TechnicalInterval($b)"
}

/** Intermediate Representation for SourceFile */
sealed abstract class SVNode {
  type T <: SVNode
  def serialize: String
  val tokens: Interval
  def mapInterval(f: Interval => Interval): T
}

case class SourceFile(tokens: Interval, path: String, descriptions: Seq[Description]) extends SVNode {
  type T = SourceFile
  
  private val dep = HashSet[PackageRef]()
  private var closed = false
  def addDep(p: PackageRef) = {
    if(closed) Utils.throwInternalError("Cannot add dependancy after dependancy read")
    dep += p
  }
  def getDep: Seq[PackageRef] = {
    closed = true
    dep.toSeq
  }
  
  def serialize : String = s"SourceFile at $path:${indent(descriptions.map("\n" + _.serialize).mkString)}\n"
  def foreachDescription(f: Description => Unit): Unit = descriptions.foreach(f)
  def mapDescription(f: Description => Description): SourceFile = this.copy(descriptions = descriptions.map(f))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
}

abstract class Description extends SVNode {
  type T <: Description
  def mapStmt(f: Statement => Statement): T
  def mapString(f: String => String): T
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes): T
  
  def foreachStmt(f: Statement => Unit): Unit
  def foreachString(f: String => Unit): Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit
}

case class UnrecognizedDescription(tokens: Interval) extends Description {
  type T = UnrecognizedDescription
  def serialize: String = "Unrecognized Description"
  def mapStmt(f: Statement => Statement) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = Unit
}

case class IsolatedStatement(tokens: Interval, body: Statement) extends Description {
  type T = IsolatedStatement
  def serialize: String = body.serialize
  def mapStmt(f: Statement => Statement) = this.copy(body = f(body))
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachStmt(f: Statement => Unit): Unit = f(body)
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = Unit
}

abstract class Header extends Statement {
  type T <: Header
  def mapExpr(f: Expression => Expression) = this.asInstanceOf[T]
  def mapType(f: Type => Type) = this.asInstanceOf[T]
  def mapStmt(f: Statement => Statement) = this.asInstanceOf[T]
  def mapString(f: String => String) = this.asInstanceOf[T]
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.asInstanceOf[T]
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = Unit
}

abstract class CompilerDirective extends Header {
  type T <: CompilerDirective
  val text : String 
  def serialize : String = s"$tokens: [IGNORED COMPILER DIRECTIVE] $text"
}
case class TimescaleDirective(tokens: Interval, text: String) extends CompilerDirective {
  type T = TimescaleDirective
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
}
case class DefaultNettypeStatement(tokens: Interval, text: String) extends CompilerDirective {
  type T = DefaultNettypeStatement
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
}

case class PackageRef(tokens: Interval, path: String, item: String="_") extends SVNode {
  type T = PackageRef
  def allItems : Boolean = item == "_"
  def serialize : String = s"'$item' from package '$path'"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
}

case class ImportPackages(tokens: Interval, packages: Seq[PackageRef]) extends Header {
  type T = ImportPackages
  def serialize : String = tokens + ": " + packages.map(_.serialize).mkString("import ","; ","")
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
}

case class IncludeHeader(tokens: Interval, path: String) extends Header {
  def serialize : String = s"include $path"
  type T = IncludeHeader
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
}


abstract class VerilogAttributes extends SVNode {
  type T <: VerilogAttributes
  // default implementation
  def serialize: String = this.toString
  def ++(that: VerilogAttributes): VerilogAttributes
}
case object NoVerilogAttribute extends VerilogAttributes {
  type T = VerilogAttributes
  def mapInterval(f: Interval => Interval) = this
  val tokens : Interval = UndefinedInterval
  override def toString: String = ""
  def ++(that: VerilogAttributes): VerilogAttributes = that
}

// TODO : it should be Expression and not String but ... not so sure it gets used in the end ...
case class VerilogAttribute(tokens: Interval, name: String, expression: Option[String]=None) extends VerilogAttributes {
  type T = VerilogAttribute
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  
  override def toString: String = expression match {
    case None => s"$tokens: (* $name *)"
    case Some(s) => s"$tokens: (* $name = $s *)"
  }
    
  //scalastyle:off method.name
  def ++(that: VerilogAttributes): VerilogAttributes = if (that == NoVerilogAttribute) this else MultiVerilogAttributes(Seq(this, that))
}
case class MultiVerilogAttributes(attributess: Seq[VerilogAttributes]) extends VerilogAttributes {
  type T = MultiVerilogAttributes
  def mapInterval(f: Interval => Interval) = this.copy(attributess.map(va => va.mapInterval(f)))
  
  private def collectStrings(attributes: VerilogAttributes): Seq[String] = attributes match {
    case VerilogAttribute(_, name, expr) => expr match {
      case None => Seq(name)
      case Some(s) => Seq(s"$name = $s")
    }
    case MultiVerilogAttributes(seq) => seq flatMap collectStrings
    case NoVerilogAttribute => Seq.empty
  }
  val tokens = attributess.map(_.tokens).reduce((a,b) => a.union(b))
  override def toString: String = {
    val parts = collectStrings(this)
    if (parts.nonEmpty) tokens + ": " + parts.mkString(" (*", ",", "*)")
    else ""
  }
  //scalastyle:off method.name
  def ++(that: VerilogAttributes): VerilogAttributes = if (that == NoVerilogAttribute) this else MultiVerilogAttributes(attributess :+ that)
}
object MultiVerilogAttributes {
  def apply(attributes: VerilogAttributes*) = {
    val attributesx = attributes.filterNot(_ == NoVerilogAttribute)
    attributesx.size match {
      case 0 => NoVerilogAttribute
      case 1 => attributesx.head
      case _ => new MultiVerilogAttributes(attributesx)
    }
  }
}

trait HasName {
  val name: String
}
trait HasVerilogAttributes {
  val attributes: VerilogAttributes
}
trait IsDeclaration extends HasName with HasVerilogAttributes

case class DesignAttribute(tokens: Interval, attributes: VerilogAttributes) extends Header with HasVerilogAttributes {
  type T = DesignAttribute
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def serialize : String = s"$tokens: [DESIGN ATTRIBUTES] " + attributes.serialize
}

/** Primitive Operation
  *
  * See [[PrimOps]]
  */
abstract class PrimOp extends SVNode {
  this: PrimOps.OpKind with PrimOps.ArgsKind => // ensure OpKind & ArgsKind are fully covering
  type T <: PrimOp
  
  def expectedArgs: Int
  
  def serialize: String = this.toString

  def apply(tokens: Interval, exprs: Expression*): DoPrim = {
    DoPrim(tokens, this, exprs)
  }
}


sealed trait ExpressionKind { def serialize : String }
case object UnknownExpressionKind extends ExpressionKind { def serialize : String = "??" }
case object HwExpressionKind extends ExpressionKind { def serialize : String = "Hw" }
case object SwExpressionKind extends ExpressionKind { def serialize : String = "Sw" }

// IMPORTANT NOTICE: although flows can help to infer remote reference flows 
// !! They are only valid locally !! 
sealed trait Flow // only intended to specify HwExpressionKind
case object UnknownFlow extends Flow
// input port or driven by submodule
case object SourceFlow extends Flow 
// driving submodule or output port
case object SinkFlow extends Flow 

/**
  * EXPRESSION 
  *
  */
sealed abstract class Expression extends SVNode {
  type T <: Expression
  def kind: ExpressionKind
  def tpe: Type
  def flow: Flow
  def mapExpr(f: Expression => Expression): T
  def mapType(f: Type => Type): T
  def mapWidth(f: Width => Width): T
  def mapKind(f: ExpressionKind => ExpressionKind): T
  def foreachExpr(f: Expression => Unit): Unit
  def foreachType(f: Type => Unit): Unit
  def foreachWidth(f: Width => Unit): Unit
  
  final def ftpe : String = s"${tpe.serialize}@${kind.serialize}"
}
case class RawScalaExpression(str: String, kind: ExpressionKind) extends Expression {
  type T = RawScalaExpression
  def serialize: String = str
  val tokens: Interval = UndefinedInterval
  def tpe: Type = UnknownType()
  def flow : Flow = UnknownFlow
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapInterval(f: Interval => Interval) = this
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
}
case class RawScalaExprWrapper(
  tokens: Interval, 
  fmt: String, 
  exprs: Seq[Expression], 
  kind: ExpressionKind,
  tpe: Type
) extends Expression {
  type T = RawScalaExprWrapper
  def serialize: String = {
    var str = fmt
    exprs.foreach(e => str = str.replaceFirst("%e", e.serialize))
    str
  }
  def flow : Flow = UnknownFlow
  def mapExpr(f: Expression => Expression) = this.copy(exprs=exprs.map(f))
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapKind(f: ExpressionKind => ExpressionKind) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = exprs.foreach(f)
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
}

case class UndefinedExpression(tokens: Interval, flow: Flow = UnknownFlow) extends Expression {
  type T = UndefinedExpression
  def tpe : Type = UnknownType()
  def kind : ExpressionKind = UnknownExpressionKind
  def serialize: String = "Undefined"
  def mapExpr(f: Expression => Expression): UndefinedExpression = this
  def mapType(f: Type => Type): UndefinedExpression = this
  def mapWidth(f: Width => Width): UndefinedExpression = this 
  def mapKind(f: ExpressionKind => ExpressionKind): UndefinedExpression = this 
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit 
  def foreachWidth(f: Width => Unit): Unit = Unit
}
object UndefinedExpression {
  def apply() : UndefinedExpression = UndefinedExpression(UndefinedInterval)
}

case class DontCare(tokens: Interval) extends Expression {
  type T = DontCare
  def tpe : Type = UnknownType()
  def flow : Flow = SourceFlow
  def kind : ExpressionKind = HwExpressionKind
  def serialize: String = "DontCare"
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this 
  def mapKind(f: ExpressionKind => ExpressionKind) = this 
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit 
  def foreachWidth(f: Width => Unit): Unit = Unit
}
object DontCare {
  def apply() : DontCare = DontCare(UndefinedInterval)
}

case class StringLit(tokens: Interval, string: String, kind : ExpressionKind, width: Width) extends Expression {
  type T = StringLit
  def flow : Flow = SourceFlow
  def tpe: Type = {
    kind match {
      case HwExpressionKind => 
        val uint8 = UIntType(UndefinedInterval, Width(8), NumberDecimal)
        PackedVecType(UndefinedInterval, Seq(uint8), width.getBoundToZero, downto = true)
        
      case _ => StringType(UndefinedInterval, width)
    }
    
  }
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
  
  /** Returns an escaped and quoted String */
  def escape: String = {
    "\"" + string + "\""
  }
  def serialize: String = escape

  /** Format the string for Verilog */
  def verilogFormat: StringLit = {
    StringLit(tokens, string.replaceAll("%x", "%h"))
  }
  /** Returns an escaped and quoted String */
  def verilogEscape: String = {
    // normalize to turn things like ö into o
    import java.text.Normalizer
    val normalized = Normalizer.normalize(string, Normalizer.Form.NFD)
    val ascii = normalized flatMap StringLit.toASCII
    ascii.mkString("\"", "", "\"")
  }
}
object StringLit {
  import org.apache.commons.text.StringEscapeUtils
  /** Maps characters to ASCII for Verilog emission */
  private def toASCII(char: Char): List[Char] = char match {
    case nonASCII if !nonASCII.isValidByte => List('*')
    case '"' => List('\\', '"')
    case '\\' => List('\\', '\\')
    case c if c >= ' ' && c <= '~' => List(c)
    case '\n' => List('\\', 'n')
    case '\t' => List('\\', 't')
    case _ => List('*')
  }

  /** Create a StringLit from a raw parsed String */
  def unescape(tokens: Interval, raw: String): StringLit = {
    StringLit(tokens, StringEscapeUtils.unescapeJava(raw))
  }
  
  def apply(tokens: Interval, string: String) : StringLit = {
    StringLit(tokens, string, SwExpressionKind, Width(string.length))
  }
} 

case class Reference(
    tokens: Interval, 
    name: String, 
    path: Seq[String], 
    tpe: Type = UnknownType(), 
    kind: ExpressionKind = UnknownExpressionKind, 
    flow: Flow = UnknownFlow
  ) extends Expression with HasName {
  type T = Reference
  def serialize: String = (path :+ name).mkString(".")
  def mapExpr(f: Expression => Expression): Reference = this
  def mapType(f: Type => Type): Reference = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width): Reference = this
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
  def asUserRefType(): UserRefType = UserRefType(tokens, name, path, tpe)
}
sealed abstract class Assign extends Expression {
  type T <: Assign
  def mapKind(f: ExpressionKind => ExpressionKind) = this.asInstanceOf[T]
  def mapType(f: Type => Type) = this.asInstanceOf[T]
  def mapWidth(f: Width => Width) = this.asInstanceOf[T]
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
}

sealed trait RemoteLinked {
  this: Expression =>
  def expr: Expression // local expression
  
  def remoteKind : Option[ExpressionKind]
  def remoteType : Option[Type]
  def remoteName : Option[String]
  def getName: String = remoteName.getOrElse("<?>")
  def assignExpr : Option[Expression]
  
  def remoteFullType : Option[FullType] = {
    (remoteKind, remoteType) match {
      case (Some(k), Some(t)) => Some(FullType(t, k))
      case _ => None
    }
  }
  def remoteFullTypeOrUnknown : FullType = {
    FullType(remoteType.getOrElse(UnknownType()), remoteKind.getOrElse(UnknownExpressionKind))
  }
  
  def remoteTypeStr: String = 
    s"${remoteType.getOrElse(UnknownType()).serialize}@${remoteKind.getOrElse(UnknownExpressionKind).serialize}"
}

case class AutoAssign(tokens: Interval) extends Assign {
  type T = AutoAssign
  def tpe : Type = UnknownType()
  def flow: Flow = UnknownFlow
  def kind: ExpressionKind = UnknownExpressionKind
  def serialize: String = s"AutoAssign"
  def mapExpr(f: Expression => Expression) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
}
case class NamedAssign(
  tokens: Interval, 
  name: String, 
  expr: Expression, 
  flow: Flow, 
  remoteKind : Option[ExpressionKind] = None,
  remoteType : Option[Type] = None,
  assignExpr : Option[Expression] = None
) extends Assign with HasName with RemoteLinked {
  type T = NamedAssign
  def remoteName = Some(name)
  def tpe : Type = expr.tpe
  def kind: ExpressionKind = expr.kind
  def serialize: String = flow match {
    case SourceFlow => s"$name: <$remoteTypeStr> <- " + expr.serialize
    case SinkFlow => s"$name: <$remoteTypeStr> -> " + expr.serialize
    case _ => s"$name: <$remoteTypeStr> <-?-> " + expr.serialize
  }
  
  def mapExpr(f: Expression => Expression) = this.copy(expr = f(expr))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = f(expr)
}
case class NoNameAssign(
  tokens: Interval, 
  expr: Expression, 
  flow: Flow,
  remoteName : Option[String] = None,
  remoteKind : Option[ExpressionKind] = None,
  remoteType : Option[Type] = None,
  assignExpr : Option[Expression] = None
) extends Assign with RemoteLinked {
  type T = NoNameAssign
  def tpe : Type = expr.tpe
  def kind: ExpressionKind = expr.kind
  def serialize: String = s"NoNameAssign(" + expr.serialize + s"): <$remoteTypeStr>"
  def mapExpr(f: Expression => Expression) = this.copy(expr = f(expr))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = f(expr)
  def toNamed(name: String): NamedAssign = NamedAssign(tokens, name, expr, flow, remoteKind, remoteType, assignExpr)
}

case class TypeInst(
    tokens: Interval,
    tpe: Type,
    name: Option[String],
    path: Seq[String],
    kind: ExpressionKind,
    flow : Flow = UnknownFlow
  ) extends Expression {
  type T = TypeInst
  def serialize: String = s"InstOf[${(path :+ name.getOrElse("<unamed>")).mkString(".")}]<$ftpe>"  
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachWidth(f: Width => Unit): Unit = Unit
}
case class SubField(
    tokens: Interval, 
    expr: Expression, 
    name: String, 
    kind: ExpressionKind,
    tpe: Type,
    flow : Flow = UnknownFlow
  ) extends Expression with HasName {
  type T = SubField
  def serialize: String = s"${expr.serialize}.$name"  
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) = this.copy(expr = f(expr))
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = f(expr)
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachWidth(f: Width => Unit): Unit = Unit
}

case class SubIndex(
    tokens: Interval, 
    expr: Expression, 
    index: Expression, 
    kind: ExpressionKind,
    tpe: Type,
    flow : Flow = UnknownFlow
  ) extends Expression {
  type T = SubIndex
  def serialize: String = s"${expr.serialize}[${index.serialize}]"
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) =
    this.copy(expr = f(expr), index = f(index))
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = { f(expr); f(index) }
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachWidth(f: Width => Unit): Unit = Unit
}
case class SubRange(
    tokens: Interval, 
    expr: Expression, 
    left: Expression, 
    right: Expression, 
    kind: ExpressionKind,
    tpe: Type,
    flow : Flow = UnknownFlow
  ) extends Expression {
  type T = SubRange
  def serialize: String = s"${expr.serialize}[${left.serialize}:${right.serialize}]"
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) =
    this.copy(expr = f(expr), left = f(left), right = f(right))
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = { f(expr); f(left); f(right) }
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachWidth(f: Width => Unit): Unit = Unit
}

sealed abstract class NumberBase(val value: Int) 
case object NumberDecimal extends NumberBase(10)
case object NumberBinary extends NumberBase(2)
case object NumberOctal extends NumberBase(8)
case object NumberHexa extends NumberBase(16)

case class Number(tokens: Interval, value: String, width: Width, base: NumberBase, kind: ExpressionKind) extends Expression {
  type T = Number
  def serialize: String = {(base match {
      case NumberDecimal => s"$value"
      case NumberOctal => s"0o$value"
      case NumberHexa => s"0x$value"
      case NumberBinary => s"0b$value"
    }) + (width.expr match {
      case _:UndefinedExpression => ""
      case e => s"(${e.serialize})"
    })
  }
  def tpe = {
    (width.expr, kind) match {
      case (_:UndefinedExpression, SwExpressionKind) => IntType(tokens, base)
      case (_, SwExpressionKind) => Utils.throwInternalError(s"Unexpected specified width $width for SwExpressionKind Number ${this.serialize}")
      case (_, HwExpressionKind) => UIntType(tokens, width, base)
      case _ => UnknownType()
    }
  }

  def getInt: Option[Int] = {
    try { 
      Some(Integer.parseInt(value, base.value))
    } catch {
      case _:java.lang.NumberFormatException => None
    }
  }
  def getBigInt: Option[BigInt] = {
    try { 
      Some(BigInt(value, base.value))
    } catch {
      case _:java.lang.NumberFormatException => None
    }
  }
  
  def flow: Flow = SourceFlow
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
}

object Number {
  // minimally specified HW or SW
  def apply(tokens: Interval, value: String, kind: ExpressionKind): Number = {
    Number(tokens, value, UnknownWidth(), NumberDecimal, kind)
  }
  
  // minimally specified SW
  def apply(tokens: Interval, value: String): Number = {
    Number(tokens, value, UnknownWidth(), NumberDecimal, SwExpressionKind)
  }
  def apply(tokens: Interval, value: String, base: NumberBase): Number = {
    Number(tokens, value, UnknownWidth(), base, SwExpressionKind)
  }
  
  // minimally specified HW
  def apply(tokens: Interval, value: String, width: Width): Number = {
    Number(tokens, value, width, NumberDecimal, HwExpressionKind)
  }
  def apply(tokens: Interval, value: String, width: Width, base: NumberBase): Number = {
    Number(tokens, value, width, base, HwExpressionKind)
  }
}

case class MaskedNumber(
    tokens: Interval, 
    number: Number, 
    mask: Number
  ) extends Expression {
  type T = MaskedNumber
  def flow: Flow = SourceFlow
  def kind: ExpressionKind = number.kind
  def tpe: Type = number.tpe
  def serialize: String = s"${number.serialize}/mask:${mask.serialize}/"
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(number = number.mapKind(f))
  def mapExpr(f: Expression => Expression) =
    this.copy(number = number.mapExpr(f), mask = mask.mapExpr(f))
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = { f(number); f(mask) }
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
} 

sealed abstract class ComplexValues extends Expression {
  type T <: ComplexValues
  def flow: Flow = SourceFlow
  def mapWidth(f: Width => Width) = this.asInstanceOf[T]
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachWidth(f: Width => Unit): Unit = Unit
}


case class FillingBitPattern(tokens: Interval, bit: String, kind: ExpressionKind, tpe: Type) extends ComplexValues {
  type T = FillingBitPattern
  def serialize: String = s"FillingBitPattern($bit)"
  def mapType(f: Type => Type) = this.copy(tpe=f(tpe))
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
}

case class ReplicatePattern(tokens: Interval, scaler: Expression, pattern: Expression, kind: ExpressionKind, tpe: Type) extends ComplexValues {
  type T = ReplicatePattern
  def serialize: String = s"ReplicatePattern(${scaler.serialize} => ${pattern.serialize})"
  def mapType(f: Type => Type) = this.copy(tpe=f(tpe))
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) = this.copy(scaler = f(scaler), pattern = f(pattern))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = {f(scaler); f(pattern)}
}

case class AssignPattern(tokens: Interval, assign: Seq[(Expression, Expression)], kind: ExpressionKind, tpe: Type) extends ComplexValues {
  type T = AssignPattern
  def mapType(f: Type => Type) = this.copy(tpe=f(tpe))
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def serialize: String = s"AssignPattern${assign.map(t => s"${t._1.serialize} -> ${t._2.serialize}").mkString("(", ",", ")")}:<${tpe.serialize}@${kind.serialize}>"
  def mapExpr(f: Expression => Expression) = this.copy(assign = assign.map(t => (f(t._1), f(t._2))))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = assign.foreach(t => {
    f(t._1) ; f(t._2)
  })
}

case class MappedValues(tokens: Interval, map: HashMap[Expression, Expression], default: Option[Expression], kind: ExpressionKind, tpe: Type) extends ComplexValues {
  // for emission Map[Int, List[String]]().withDefaultValue(List())
  type T = MappedValues
  def mapType(f: Type => Type) = this.copy(tpe=f(tpe))
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def serialize: String = s"MappedValues${map.map(t => s"${t._1.serialize} -> ${t._2.serialize}").mkString("(", ",", ")")}"
  def mapExpr(f: Expression => Expression) = this.copy(map = map.map(t => (f(t._1), f(t._2))), default = default.map(f))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = map.foreach(t => {
    f(t._1) ; f(t._2) ; default.foreach(f)
  })
}

case class SeqValues(tokens: Interval, values: Seq[Expression], kind: ExpressionKind, tpe: Type) extends ComplexValues {
  type T = SeqValues
  def mapType(f: Type => Type) = this.copy(tpe=f(tpe))
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def serialize: String = s"SeqValues${values.map(_.serialize).mkString("(", ",", ")")}:<${tpe.serialize}@${kind.serialize}>"
  def mapExpr(f: Expression => Expression) = this.copy(values = values.map(f))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = values.foreach(f)
}

case class DefaultAssignPattern(tokens: Interval) extends Expression with HasName {
  type T = DefaultAssignPattern
  def flow: Flow = UnknownFlow
  def tpe : Type = UnknownType()
  def kind : ExpressionKind = UnknownExpressionKind
  val name : String = "default"
  def serialize: String = s"DefaultAssignPattern"
  def mapKind(f: ExpressionKind => ExpressionKind) = this
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
}

sealed abstract class Literal extends Expression {
  type T <: Literal
  def flow: Flow = SourceFlow
  val value: BigInt
  val width: Width
}
case class BoolLiteral(tokens: Interval, v: Boolean, kind: ExpressionKind) extends Literal {
  type T = BoolLiteral
  val value = if(v) 1 else 0
  val width: Width = Width(1)
  def tpe = BoolType(tokens)
  def serialize = kind match {
    case HwExpressionKind => v.toString + ".B" 
    case _ => v.toString 
  }
  
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
}
case class UIntLiteral(tokens: Interval, value: BigInt, width: Width, base: NumberBase) extends Literal {
  type T = UIntLiteral
  def tpe = UIntType(tokens, width, base)
  def kind: ExpressionKind = HwExpressionKind
  def serialize = {
    val base = s"$value.U"
    width.expr match {
      case _: UndefinedExpression => base
      case w => base + s"(${w.serialize}.W)"
    }
  }
  def mapKind(f: ExpressionKind => ExpressionKind) = this
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this.copy(width = f(width))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = f(width)
}
object UIntLiteral {
  def minWidth(value: BigInt): Width = Width(math.max(value.bitLength, 1))
  def apply(tokens: Interval, value: BigInt): UIntLiteral = new UIntLiteral(tokens, value, minWidth(value), NumberDecimal)
}
case class SIntLiteral(tokens: Interval, value: BigInt, width: Width) extends Literal {
  type T = SIntLiteral
  def tpe = SIntType(tokens, width)
  def kind: ExpressionKind = HwExpressionKind
  def serialize = s"""SInt${width.serialize}("h""" + value.toString(16)+ """")"""
  def mapKind(f: ExpressionKind => ExpressionKind) = this
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = SIntLiteral(tokens, value, f(width))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = f(width)
}
object SIntLiteral {
  def minWidth(value: BigInt): Width = Width(value.bitLength + 1)
  def apply(tokens: Interval, value: BigInt): SIntLiteral = new SIntLiteral(tokens, value, minWidth(value))
}
case class FixedLiteral(tokens: Interval, value: BigInt, width: Width, point: Width) extends Literal {
  type T = FixedLiteral
  def tpe = FixedType(tokens, width, point)
  def kind: ExpressionKind = HwExpressionKind
  def serialize = {
    val pstring = if(point == UnknownWidth()) "" else s"<${point.serialize}>"
    s"""Fixed${width.serialize}$pstring("h${value.toString(16)}")"""
  }
  def mapKind(f: ExpressionKind => ExpressionKind) = this
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = FixedLiteral(tokens, value, f(width), f(point))
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = { f(width); f(point) }
}
case class DoPrim(
    tokens: Interval, 
    op: PrimOp, 
    args: Seq[Expression],
    kind: ExpressionKind = UnknownExpressionKind,
    tpe: Type = UnknownType()
  ) extends Expression {
  type T = DoPrim
  def flow: Flow = SourceFlow
  def serialize: String =  (op, args)match {
    case(_:PrimOps.BinaryOp, Seq(a,b)) => s"(${a.serialize}) ${op.serialize} (${b.serialize})"
    case _ => s"${op.serialize}${args.map(_.serialize).mkString("(",", ",")")}"
  }
  
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) = this.copy(args = args map f)
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = args.foreach(f)
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
}

case class Concat(
    tokens: Interval, 
    args: Seq[Expression],
    kind: ExpressionKind = UnknownExpressionKind,
    tpe: Type = UnknownType(),
    flow : Flow = UnknownFlow
  ) extends Expression {
  type T = Concat
  def serialize: String = s"Concat(${indent(args.map(a => s"\n${a.serialize}: <${a.ftpe}>").mkString)}): <${ftpe}>\n"
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) = this.copy(args = args map f)
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = args.foreach(f)
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
}

case class DoCast(
    tokens: Interval, 
    expr: Expression, 
    kind: ExpressionKind,
    tpe: Type
  ) extends Expression {
  type T = DoCast
  def flow: Flow = SourceFlow
  def serialize: String = s"(${expr.serialize}).asTypeOf(${ftpe}):${ftpe}"
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  def mapExpr(f: Expression => Expression) = this.copy(expr = f(expr))
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = f(expr)
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachWidth(f: Width => Unit): Unit = Unit
}
case class DoCall(
    tokens: Interval, 
    fun: Reference, 
    args: Seq[Assign],
    tpe: Type,
    kind: ExpressionKind
  ) extends Expression {
  type T = DoCall
  def flow: Flow = SourceFlow
  def serialize: String =  s"${fun.serialize}${args.map(_.serialize).mkString("(",", ",")")}"
  def mapKind(f: ExpressionKind => ExpressionKind) = this.copy(kind = f(kind))
  // hacky: assign are transparent here
  def mapExpr(f: Expression => Expression) = this.copy(args = args.map(_.mapExpr(f))) 
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def foreachExpr(f: Expression => Unit): Unit = args.foreach(f)
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachWidth(f: Width => Unit): Unit = Unit
}

/**
  * STATEMENTS 
  *
  */
sealed abstract class Statement extends SVNode {
  type T <: Statement
  def mapInterval(f: Interval => Interval): T
  def mapStmt(f: Statement => Statement): T
  def mapExpr(f: Expression => Expression): T
  def mapType(f: Type => Type): T
  def mapString(f: String => String): T
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes): T
  def foreachStmt(f: Statement => Unit): Unit
  def foreachExpr(f: Expression => Unit): Unit
  def foreachType(f: Type => Unit): Unit
  def foreachString(f: String => Unit): Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit
}

case class DefInstance(
  tokens: Interval,
  attributes: VerilogAttributes,
  name: String,
  module: Reference,
  portMap: Seq[Assign],
  paramMap: Seq[Assign],
  clock : Option[String] = None,
  reset: Option[String] = None,
  ioBundleConnect: Boolean = false
) extends Statement with IsDeclaration {
  type T = DefInstance
  def serialize: String = s"inst $name of ${module.serialize} : ${attributes.serialize}${indent(paramMap.map("\n" + _.serialize).mkString)}${indent(portMap.map("\n" + _.serialize).mkString)}\n"  
  
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this.copy(portMap = portMap.map(p => p.mapExpr(f)), paramMap = paramMap.map(_.mapExpr(f)))
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = DefInstance(tokens, attributes, f(name), module, portMap, paramMap)
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = {portMap.map(f) ; paramMap.map(f)}
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = f(name)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}

case class DefFunction(
  tokens: Interval,
  name: String,
  body: Statement,
  tpe: Type
) extends Statement with IsDeclaration with FetchPort {
  val attributes: VerilogAttributes = NoVerilogAttribute
  val kind : ExpressionKind = HwExpressionKind // TODO: deal with various function kinds
  type T = DefFunction
  def serialize: String = s"function $name : ${indent(body.serialize)}\n"  
  
  def mapPort(f: Port => Statement): T = this.copy(body = mapPortStatement(body, f))
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this.copy(body = f(body))
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def mapString(f: String => String) = this.copy(name = f(name))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this
  
  def foreachPort(f: Port => Unit): Unit = ports.map(f)
  def foreachStmt(f: Statement => Unit): Unit = f(body)
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachString(f: String => Unit): Unit = f(name)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = Unit
}

case class DefType(
  tokens: Interval,
  attributes: VerilogAttributes,
  name: String,
  tpe: Type
) extends Statement with IsDeclaration {
  type T = DefType
  def serialize: String = s"type $name: <${tpe.serialize}>"  
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def mapString(f: String => String) = this.copy(name = f(name))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachString(f: String => Unit): Unit = f(name)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}

object ReadUnderWrite extends Enumeration {
  val Undefined = Value("undefined")
  val Old = Value("old")
  val New = Value("new")
}

case class DefMemory(tokens: Interval, 
    attributes: VerilogAttributes,
    name: String,
    dataType: Type,
    depth: BigInt,
    writeLatency: Int,
    readLatency: Int,
    readers: Seq[String],
    writers: Seq[String],
    readwriters: Seq[String],
    // TODO: handle read-under-write
    readUnderWrite: ReadUnderWrite.Value = ReadUnderWrite.Undefined) extends Statement with IsDeclaration {
  type T = DefMemory
  def serialize: String =
    s"mem $name :" + attributes.serialize +
    indent(
      (Seq("\ndata-type => " + dataType.serialize,
          "depth => " + depth,
          "read-latency => " + readLatency,
          "write-latency => " + writeLatency) ++
          (readers map ("reader => " + _)) ++
          (writers map ("writer => " + _)) ++
          (readwriters map ("readwriter => " + _)) ++
       Seq(s"read-under-write => ${readUnderWrite}")) mkString "\n")
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this.copy(dataType = f(dataType))
  def mapString(f: String => String) = this.copy(name = f(name))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = f(dataType)
  def foreachString(f: String => Unit): Unit = f(name)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}



// State Used to record SVerilog's Logic resolution as reg or wire depdending on usage
sealed trait LogicResolution {
  def serialize : String
  def mapExpr(f: Expression => Expression): LogicResolution = this match {
    case s:SimpleLogicResolution => s.mapExpr(f) 
    case LogicConditional(bindings) => LogicConditional(bindings.map(b => b.copy(f(b.ctx), b.res.mapExpr(f))))
    case _ => this
  }
  def foreachExpr(f: Expression => Unit): Unit = this match {
    case s:SimpleLogicResolution => s.foreachExpr(f) 
    case LogicConditional(bindings) => bindings.foreach(b => {f(b.ctx) ; b.res.foreachExpr(f)})
    case _ => 
  }
}
sealed trait SimpleLogicResolution extends LogicResolution {
  override def mapExpr(f: Expression => Expression): SimpleLogicResolution = this match {
    case LogicRegister(clock, reset, init, preset) => LogicRegister(f(clock), f(reset), f(init), f(preset))
    case LogicWire(default) => LogicWire(f(default))
    case LogicUnresolved(default) => LogicUnresolved(f(default))
  }
  override def foreachExpr(f: Expression => Unit): Unit = this match {
    case LogicRegister(clock, reset, init, preset) => {f(clock) ; f(reset) ; f(init) ; f(preset)}
    case LogicWire(default) => f(default)
    case LogicUnresolved(default) => f(default)
  }
}

case class LogicUnresolved(value: Expression) extends SimpleLogicResolution {
  def serialize : String = "logic<unresolved>"
}
case class LogicWire(default: Expression) extends SimpleLogicResolution {
  def serialize : String = "logic<wire>"
}
case class LogicRegister(
  clock: Expression, 
  reset: Expression, 
  init: Expression, 
  preset: Expression
) extends SimpleLogicResolution {
  def serialize : String = s"logic<reg@${clock.serialize}>"
}
object LogicRegister {
  def apply(preset: Expression): LogicRegister = {
    LogicRegister(UndefinedExpression(), UndefinedExpression(), UndefinedExpression(), preset)
  }
}

case class LogicBinding(ctx: Expression, res: SimpleLogicResolution)

case class LogicConditional(bindings: Seq[LogicBinding]) extends LogicResolution {
  def serialize : String = s"logic<" + bindings.map(b => s"[${b.ctx.serialize}]->${b.res.serialize}").mkString("\n     ") + ">"
}

case class DefLogic(
    tokens: Interval, 
    attributes: VerilogAttributes, 
    name: String,
    tpe: Type,
    resolution : LogicResolution
  ) extends Statement with IsDeclaration {
  type T = DefLogic
  def serialize: String = {
    val header = s"${resolution.serialize} $name : <${tpe.serialize}>"
    resolution match {
      case LogicRegister(clock, reset, init, preset) => 
        header + s",@${clock.serialize} ; preset => ${preset.serialize} ; reset => (${reset.serialize}, ${init.serialize})" + attributes.serialize
      case LogicWire(_:UndefinedExpression) => header 
      case LogicWire(default) => s"$header = ${default.serialize}" 
      case LogicUnresolved(_:UndefinedExpression) => header 
      case LogicUnresolved(value) => s"$header = ${value.serialize}" 
      case _:LogicConditional => s"logic<cond> $name : <${tpe.serialize}>"
    }
  }
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this.copy(resolution = resolution.mapExpr(f))
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def mapString(f: String => String) = this.copy(name = f(name))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = resolution.foreachExpr(f)
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachString(f: String => Unit): Unit = f(name)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}

object DefWire {
  def apply(t: Interval, 
            a: VerilogAttributes, 
            n: String,
            tpe: Type): DefLogic = {
    DefLogic(t, a, n, tpe, LogicWire(UndefinedExpression()))
  }
  def apply(n: String, tpe: Type): DefLogic = {
    apply(UndefinedInterval, NoVerilogAttribute, n, tpe)
  }
}

case class Conditionally(tokens: Interval, 
    attributes: VerilogAttributes,
    pred: Expression,
    conseq: Statement,
    alt: Statement) extends Statement with HasVerilogAttributes {
  type T = Conditionally
  def serialize: String =
    s"when ${pred.serialize} :" + attributes.serialize +
    indent("\n" + conseq.serialize) +
    (if (alt == EmptyStmt) "" else "\nelse :" + indent("\n" + alt.serialize))
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = Conditionally(tokens, attributes, pred, f(conseq), f(alt))
  def mapExpr(f: Expression => Expression) = Conditionally(tokens, attributes, f(pred), conseq, alt)
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = { f(conseq); f(alt) }
  def foreachExpr(f: Expression => Unit): Unit = f(pred)
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}

case class Switch(tokens: Interval, 
    attributes: VerilogAttributes,
    expr: Expression, 
    cases: Seq[(Expression, Statement)],
    default: Statement) extends Statement with HasVerilogAttributes {
  type T = Switch
  def serialize: String = {
    s"switch ${expr.serialize}:" + attributes.serialize +
      indent( cases.map(t => {
         "\n" + s"${t._1.serialize}: " + indent("\n" + t._2.serialize)
      }).mkString) + 
      (if (default == EmptyStmt) "" else indent( "\n<default>:" + indent("\n" + default.serialize)))}
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this.copy(cases = cases.map(t => (t._1, f(t._2))), default = f(default))
  def mapExpr(f: Expression => Expression) = this.copy(cases = cases.map(t => (f(t._1), t._2)), expr = f(expr))
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = { cases.foreach(t => f(t._2)) ; f(default) }
  def foreachExpr(f: Expression => Unit): Unit = { cases.foreach(t => f(t._1)) ; f(expr) }
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}

case class IfGen(tokens: Interval, 
    attributes: VerilogAttributes,
    pred: Expression,
    conseq: Statement,
    alt: Statement) extends Statement with HasVerilogAttributes {
  type T = IfGen
  def serialize: String =
    s"if(${pred.serialize})" + attributes.serialize +
    indent("\n" + conseq.serialize) +
    (if (alt == EmptyStmt) "" else "\nelse :" + indent("\n" + alt.serialize))
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this.copy(conseq = f(conseq), alt = f(alt))
  def mapExpr(f: Expression => Expression) = this.copy(pred = f(pred))
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = { f(conseq); f(alt) }
  def foreachExpr(f: Expression => Unit): Unit = f(pred)
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}
case class ForGen(tokens: Interval, 
    attributes: VerilogAttributes,
    init: Expression,
    stop: Expression,
    step: Expression,
    stmt: Statement) extends Statement with HasVerilogAttributes {
  type T = ForGen
  def serialize: String =
    s"for(${init.serialize}; ${stop.serialize}; ${step.serialize}):" + attributes.serialize +
    indent("\n" + stmt.serialize)
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this.copy(stmt = f(stmt))
  def mapExpr(f: Expression => Expression) = this.copy(init = f(init), stop = f(stop), step = f(step))
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = { f(stmt) }
  def foreachExpr(f: Expression => Unit): Unit = {f(init) ; f(stop) ; f(step)}
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}


abstract class Block extends Statement with HasName {
  type T <: Block
  val stmts: Seq[Statement]
  
  def serializeStmts: String = stmts map (_.serialize) mkString "\n"
  
  def mapExpr(f: Expression => Expression) = this.asInstanceOf[T] 
  def mapType(f: Type => Type) = this.asInstanceOf[T] 
  def mapString(f: String => String) = this.asInstanceOf[T] 
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.asInstanceOf[T] 
  def foreachStmt(f: Statement => Unit): Unit = stmts.foreach(f)
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = Unit
  
  def appendStmts(s: Seq[Statement]) : T 
  def prependStmts(s: Seq[Statement]) : T 
  
  def toSimpleBlock(): SimpleBlock = {
    SimpleBlock(tokens, stmts, name)
  }
  def toClockRegion(clock: Expression, posedge: Boolean): ClockRegion = {
    ClockRegion(tokens, stmts, clock, posedge, name)
  }
  def toResetRegion(reset: Expression, synchronous: Boolean): ResetRegion = {
    ResetRegion(tokens, stmts, reset, synchronous, name)
  }  
}
object Block {
  def getInterval(stmts: Seq[Statement]): Interval = {
    stmts match {
      case Seq() => UndefinedInterval 
      case Seq(s) => s.tokens
      case s =>
      // TO DO : fix this : does not work with UndefinedInterval...
      // provide a proper function to extract biggest englobing interval from random seq of statement
        val low = s.head.tokens.a
        val high = s.last.tokens.b
        new Interval(low,high)
    }
  }
}

case class SimpleBlock(tokens: Interval, stmts: Seq[Statement], name: String = "") extends Block {
  type T = SimpleBlock
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this.copy(stmts = stmts map f)
  def serialize : String = serializeStmts
  def appendStmts(s: Seq[Statement])  = this.copy(stmts = stmts ++ s)
  def prependStmts(s: Seq[Statement])  = this.copy(stmts = s ++ stmts)
}

object SimpleBlock {
  def apply(tokens: Interval, head: Statement, tail: Statement*): SimpleBlock = SimpleBlock(tokens, head +: tail)
  def apply(stmts: Seq[Statement]): SimpleBlock = {
    SimpleBlock(Block.getInterval(stmts), stmts)
  }
}

trait EventControl {
  val clocked : Boolean // true => clock ; false => comb
  val posedge : Boolean
  val all     : Boolean
  val trig    : Expression
}

case class AlwaysComb(tokens: Interval, all: Boolean, trig: Expression) extends SVNode with EventControl {
  type T = AlwaysComb
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  val clocked : Boolean = false
  val posedge : Boolean = false
  def serialize : String = "always_comb"
}

object AlwaysComb {
  def apply(tokens: Interval): AlwaysComb = {
    AlwaysComb(tokens, true, UndefinedExpression())
  }
}

case class AlwaysFF(tokens: Interval, posedge: Boolean, trig: Expression) extends SVNode with EventControl {
  type T = AlwaysFF
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  val clocked : Boolean = true
  val all     : Boolean = false
  def serialize : String = "always_ff"
}

case class UndefinedEventControl(tokens: Interval) extends SVNode with EventControl {
  type T = UndefinedEventControl
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  val clocked : Boolean = false
  val posedge : Boolean = false 
  val all     : Boolean = false 
  val trig : Expression = UndefinedExpression()
  def serialize : String = "always_???"
}

case class ClockRegion(
    tokens: Interval, 
    stmts: Seq[Statement],
    clock: Expression,
    posedge: Boolean,
    name: String = ""
  ) extends Block {
  type T = ClockRegion
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))  
  def mapStmt(f: Statement => Statement) = this.copy(stmts = stmts map f)
  
  private def edge = if(posedge) "posedge" else "negedge"
  
  def serialize : String = s"ClockRegion($edge,${clock.serialize})" + indent("\n" + serializeStmts)
  
  def appendStmts(s: Seq[Statement])  = this.copy(stmts = stmts ++ s)
  def prependStmts(s: Seq[Statement])  = this.copy(stmts = s ++ stmts)
}
object ClockRegion {
  def apply(stmts: Seq[Statement], clock: Expression, posedge: Boolean): ClockRegion = {
    ClockRegion(Block.getInterval(stmts), stmts, clock, posedge)
  }
}
case class ResetRegion(
    tokens: Interval, 
    stmts: Seq[Statement],
    reset: Expression,
    synchronous: Boolean,
    name: String = ""
  ) extends Block {
  type T = ResetRegion
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this.copy(stmts = stmts map f)
  
  private def tpe = if(synchronous) "sync" else "async"
  
  def serialize : String = s"ResetRegion($tpe,${reset.serialize})\n" + indent(serializeStmts)
  
  def appendStmts(s: Seq[Statement])  = this.copy(stmts = stmts ++ s)
  def prependStmts(s: Seq[Statement])  = this.copy(stmts = s ++ stmts)
}
object ResetRegion {

  def apply(stmts: Seq[Statement], reset: Expression, synchronous: Boolean): ResetRegion = {
    ResetRegion(Block.getInterval(stmts), stmts, reset, synchronous)
  }
}

case class Connect(
    tokens: Interval, 
    attributes: VerilogAttributes, 
    loc: Expression, 
    expr: Expression,
    continuous: Boolean,
    blocking: Boolean
  ) extends Statement with HasVerilogAttributes {
  type T = Connect
  def serialize: String =  s"${loc.serialize}:<${loc.ftpe}> := ${expr.serialize}:<${expr.ftpe}>" + attributes.serialize
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this.copy(loc = f(loc), expr = f(expr))
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = { f(loc); f(expr) }
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}

// simple wrapper, typical use case: last function statement = return value 
case class ExpressionStatement(
    expr: Expression
  ) extends Statement {
  type T = ExpressionStatement
  val tokens = expr.tokens
  def serialize: String =  s"${expr.serialize}:<${expr.ftpe}>"
  def mapInterval(f: Interval => Interval) = this
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this.copy(expr = f(expr))
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = f(expr)
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = Unit
}

case class Stop(tokens: Interval, attributes: VerilogAttributes, ret: Expression, clk: Expression, en: Expression) extends Statement with HasVerilogAttributes {
  type T = Stop
  def serialize: String = s"stop(${clk.serialize}, ${en.serialize}, $ret)" + attributes.serialize
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = Stop(tokens, attributes, f(ret), f(clk), f(en))
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = { f(ret) ; f(clk); f(en) }
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}
case class Print(tokens: Interval, 
    attributes: VerilogAttributes,
    string: StringLit,
    args: Seq[Expression]
  ) extends Statement with HasVerilogAttributes {
  type T = Print
  def serialize: String = {
    val strs = Seq(string.escape) ++
               (args map (_.serialize))
    "printf(p" + (strs mkString ", ") + ")" + attributes.serialize
  }
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this.copy(args = args.map(f))
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = { args.foreach(f) }
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}
case object EmptyStmt extends Statement {
  type T = Statement
  val tokens : Interval = UndefinedInterval
  def serialize: String = "skip"
  def mapInterval(f: Interval => Interval) = this
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = Unit
}

case class Comment(tokens: Interval, str: String) extends Statement {
  type T = Comment
  def serialize: String = str
  def mapInterval(f: Interval => Interval) = this
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this.copy(str = f(str))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = f(str)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = Unit
}

case class RawScala(str: String) extends Statement {
  type T = RawScala
  val tokens: Interval = UndefinedInterval
  def serialize: String = str
  def mapInterval(f: Interval => Interval) = this
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = this
  def mapType(f: Type => Type) = this
  def mapString(f: String => String) = this
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = Unit
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = Unit
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = Unit
}






/**
  * WIDTH 
  * should be an expression rather than independent node kind ??
  *
  */
case class Width(tokens: Interval, expr: Expression) extends SVNode {
  type T = Width
  def serialize: String = s"(${expr.serialize}).W"
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapExpr(f: Expression => Expression): Width = this.copy(expr = f(expr))
  def foreachExpr(f: Expression => Unit): Unit = f(expr)
  
  def getBoundToZero(): Expression = {
    // need to do -1 to get bound from width
    // in some case there is a +1 in the expression, let's just strip it
    expr match {
      case DoPrim(_, _:PrimOps.Add, Seq(e, p), _, _) if(p.serialize == "1") => e 
      case DoPrim(_, _:PrimOps.Add, Seq(p, e), _, _) if(p.serialize == "1") => e 
      case n: Number => n.copy(value = s"${n.evalBigInt()-1}")
      case _ => 
        DoPrim(expr.tokens, PrimOps.Sub(UndefinedInterval), Seq(expr, Number(UndefinedInterval,"1", SwExpressionKind)), expr.kind, expr.tpe)
    }
  }
}

object Width {
  def apply(width: BigInt): Width = {
    Width(UndefinedInterval, Number(UndefinedInterval, s"$width"))
  }
  def apply(expr: Expression): Width = {
    new Width(expr.tokens, expr)
  }
}

object BoolWidth {
  def apply(): Width = {
    Width(1)
  }
  def unapply(w: Width): Boolean = {
    if(Utils.eq(w.expr, Width(1).expr)) true else false
  }
}

object UnknownWidth {
  def apply() : Width = {
    Width(UndefinedInterval, UndefinedExpression())
  }
  def unapply(w: Width): Boolean = w.expr match {
    case _: UndefinedExpression => true
    case _ => false 
  }
   
}

/** Orientation of [[Field]] */
abstract class Orientation extends SVNode {
  type T = Orientation
  def mapInterval(f: Interval => Interval): Orientation = this
  val tokens : Interval = UndefinedInterval
}
case object Default extends Orientation {
  def serialize: String = ""
}
case object Flip extends Orientation {
  def serialize: String = "flip "
}

/** Field of [[BundleType]] */
case class Field(tokens: Interval, attributes: VerilogAttributes, name: String, flip: Orientation, tpe: Type) extends SVNode with HasName {
  type T = Field
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
  def serialize: String = {
    val main = "val " + name + " = " + tpe.serialize
    flip match {
      case Flip => s"Flipped($main)"
      case _ => main
    }
  }
}

case class EnumField(tokens: Interval, name: String, value: Expression) extends SVNode with HasName {
  type T = EnumField
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def serialize: String = s"$name = ${value.serialize}"
}

/** Types of [[SVNode]] */
abstract class Type extends SVNode {
  type T <: Type
  def mapType(f: Type => Type): T
  def mapWidth(f: Width => Width): T
  def foreachType(f: Type => Unit): Unit
  def foreachWidth(f: Width => Unit): Unit
  def widthOption: Option[Width]
}

case class VoidType(tokens: Interval) extends Type {
  type T = VoidType
  def mapType(f: Type => Type): T = this
  def mapWidth(f: Width => Width): T = this
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
  def widthOption: Option[Width] = None
  def mapInterval(f: Interval => Interval): T = this.copy(tokens = f(tokens))
  def serialize: String = "void"
}

/** any hardware type (working intermediate, cannot be emitted) */
case class DataType() extends Type {
  type T = DataType
  val tokens = UndefinedInterval
  def mapType(f: Type => Type): T = this
  def mapWidth(f: Width => Width): T = this
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
  def widthOption: Option[Width] = None
  def mapInterval(f: Interval => Interval): T = this
  def serialize: String = "Data"
}

case class RawScalaType(str: String) extends Type {
  type T = RawScalaType
  def mapInterval(f: Interval => Interval) = this
  def serialize : String = str
  val tokens: Interval = UndefinedInterval
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
  def widthOption: Option[Width] = None
}

case class OptionType(tpe: Type) extends Type {
  type T = OptionType
  def mapInterval(f: Interval => Interval) = this
  def serialize : String = s"Option[${tpe.serialize}]"
  val tokens: Interval = tpe.tokens
  def mapType(f: Type => Type) = this.copy(tpe=f(tpe))
  def mapWidth(f: Width => Width) = this
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachWidth(f: Width => Unit): Unit = Unit
  def widthOption: Option[Width] = None
}


case class UserRefType(
    tokens: Interval, 
    name: String, 
    path: Seq[String], 
    tpe: Type = UnknownType()
  ) extends Type with HasName {
  type T = UserRefType
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def serialize: String = (path :+ name).mkString(".")
  def mapType(f: Type => Type) = this.copy(tpe=f(tpe))
  def mapWidth(f: Width => Width) = this.copy(tpe=tpe.mapWidth(f))
  def foreachWidth(f: Width => Unit): Unit = tpe.foreachWidth(f)
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def widthOption: Option[Width] = tpe.widthOption
}

abstract class GroundType extends Type {
  type T <: GroundType
  val width: Width
  def mapType(f: Type => Type) = this.asInstanceOf[T]
  def foreachType(f: Type => Unit): Unit = Unit
  def widthOption: Option[Width] = Some(width)
}
object GroundType {
  def unapply(ground: GroundType): Option[Width] = Some(ground.width)
}

abstract class AggregateType extends Type {
  type T <: AggregateType
}

case class StringType(tokens: Interval, width: Width) extends GroundType {
  type T = StringType
  def serialize: String = "String"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = StringType(tokens, f(width))
  def foreachWidth(f: Width => Unit): Unit = f(width)
}

case class TypeOf(tokens: Interval, expr: Expression) extends GroundType {
  type T = TypeOf
  val width = Width(expr)
  def serialize: String = s"ChiselTypeOf(${expr.serialize})"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = this.copy(expr = f(width).expr)
  def foreachWidth(f: Width => Unit): Unit = f(width)
}

case class UIntType(tokens: Interval, width: Width, base: NumberBase) extends GroundType {
  type T = UIntType
  def serialize: String = s"UInt%$base(${width.serialize})"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = UIntType(tokens, f(width), base)
  def foreachWidth(f: Width => Unit): Unit = f(width)
}

case class IntType(tokens: Interval, base: NumberBase) extends GroundType {
  type T = IntType
  val width = UnknownWidth()
  def serialize: String = s"Int%$base"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = this
  def foreachWidth(f: Width => Unit): Unit = Unit
}
object IntType {
  def apply(): IntType = {
    IntType(UndefinedInterval, NumberDecimal)
  }
}

// signed is meant for temporary purposes before aggregating an (un)packedVec of Bool into a SInt
case class BoolType(tokens: Interval, signed: Boolean = false) extends GroundType {
  type T = BoolType
  val width = BoolWidth()
  def serialize: String = "Bool"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = this
  def foreachWidth(f: Width => Unit): Unit = f(width)
  
  def toClock = ClockType(tokens)
}

case class SIntType(tokens: Interval, width: Width) extends GroundType {
  type T = SIntType
  def serialize: String = s"SInt(${width.serialize})"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = SIntType(tokens, f(width))
  def foreachWidth(f: Width => Unit): Unit = f(width)
}
case class FixedType(tokens: Interval, width: Width, point: Width) extends GroundType {
  type T = FixedType
  override def serialize: String = {
    val pstring = if(point == UnknownWidth()) "" else s"<${point.serialize}>"
    s"Fixed${width.serialize}$pstring"
  }
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = FixedType(tokens, f(width), f(point))
  def foreachWidth(f: Width => Unit): Unit = { f(width); f(point) }
}

case class BundleType(tokens: Interval, fields: Seq[Field]) extends AggregateType {
  type T = BundleType
  def serialize: String = "Bundle { " + indent(fields.map(_.serialize
  ).mkString("\n","\n", "")) + "\n}"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapType(f: Type => Type) = this.copy(fields = fields.map(x => x.copy(tpe = f(x.tpe))))
  def foreachType(f: Type => Unit): Unit = fields.foreach{ x => f(x.tpe) }
  def mapWidth(f: Width => Width) = this
  def foreachWidth(f: Width => Unit): Unit = Unit
  def widthOption: Option[Width] = {
    val fieldWidths = fields.map(_.tpe.widthOption)
    fieldWidths.collectFirst {case a@None => a} match {
      case None => // OK continue
        Some(Width(UndefinedInterval, fieldWidths.map(_.get.expr).reduce((a, b) => {
          DoPrim(UndefinedInterval, PrimOps.Add(UndefinedInterval), Seq(a, b))
        })))
      case _ => None
    }
  }
}

case class EnumType(tokens: Interval, fields: Seq[EnumField], tpe: Type, kind: ExpressionKind) extends AggregateType {
  type T = EnumType
  def serialize: String = s"Enum: <${tpe.serialize}@${kind.serialize}> { " + indent(fields.map(_.serialize
  ).mkString("\n","\n", "")) + "\n}"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def mapWidth(f: Width => Width) = this
  def foreachWidth(f: Width => Unit): Unit = Unit
  def widthOption: Option[Width] = tpe.widthOption
  
  def isGeneric: Boolean = {
    // no user-specified values in verilog
    fields.map({
      case EnumField(_,_,_:UndefinedExpression) => true
      case _ => false
    }).reduce(_ && _) ||
    // incremental values specified by user in verilog
    fields.zipWithIndex.map({
      case (EnumField(_,_,n:Number), i) => n.getInt match {
        case Some(v) => v == i
        case _ => false
      }
      case _ => false
    }).reduce(_ && _)
  }
}

abstract class VecType extends AggregateType {
  type T <: VecType
  val downto: Boolean
  val tpe: Seq[Type]
  val bound : Expression
  def widthOption: Option[Width] = Some(Width(getWidth))
  
  def downtoStr : String = if (downto) "downto" else "upto"
  
  def foreachType(f: Type => Unit): Unit = tpe.foreach(f)
  def mapType(f: Type => Type): T
  
  def foreachWidth(f: Width => Unit): Unit = f(Width(bound))
  
  def mapBound(f: Expression => Expression): T
  
  def asUnpackedVecType(): UnpackedVecType = {
    UnpackedVecType(tokens, tpe, bound, downto)
  }
  def asPackedVecType(): PackedVecType = {
    PackedVecType(tokens, tpe, bound, downto)
  }
  
  def getLen: Expression = {
    // need to do +1 to get actual width
    // in most case there is a -1 in the verilog, let's strip it
    bound match {
      case DoPrim(_, _:PrimOps.Sub, Seq(e, sub), _, _) if(sub.serialize == "1") => e 
      case n: Number => n.copy(value = s"${n.evalBigInt()+1}")
      case _ => 
        DoPrim(bound.tokens, PrimOps.Add(UndefinedInterval), Seq(bound, Number(UndefinedInterval,"1", SwExpressionKind)), bound.kind, bound.tpe)
    }
  }

  def getWidth: Expression = {
    val len = getLen
    tpe match {
      case Seq(t) => 
        t.widthOption match {
          case Some(w) if (BoolWidth.unapply(w)) => len
          case Some(w) => DoPrim(bound.tokens, PrimOps.Mul(UndefinedInterval), Seq(len, w.expr), bound.kind, bound.tpe)
          case _ => len
        }
      case _ => len
    }
  }
  
  def getWidth8: Expression = {
    // need to do +1 to get actual width
    // in most case there is a -1 in the verilog, let's strip it
    getLen match {
      case DoPrim(_, _:PrimOps.Mul, Seq(a, b), _, _) if(b.serialize == "8") => a 
      case DoPrim(_, _:PrimOps.Mul, Seq(a, b), _, _) if(a.serialize == "8") => b
      case n: Number => 
        val value = n.evalBigInt
        println(value)
        println(value/8)
        if (value % 8 == 0){
          n.copy(value = s"${(value/8)}")
        } else {
          // to do : transform println as critical ?
          println(s"Padding declaration as CharVec with ${n.evalBigInt % 8} bits to get a multiple of 8")
          n.copy(value = s"${(value/8)+1}")
        }
      case _ => 
        DoPrim(bound.tokens, PrimOps.Div(UndefinedInterval), Seq(Utils.safeOperand(bound), Number(UndefinedInterval,"8", SwExpressionKind)), bound.kind, bound.tpe)
    }
  }
  
  def asUIntType: UIntType = {
    tpe match {
      case Seq() => UIntType(tokens, Width(0), NumberDecimal)
      case Seq(_: BoolType) => UIntType(tokens, Width(UndefinedInterval, getLen), NumberDecimal)
      case _ => Utils.throwInternalError("Unable to convert to UInt")
    }
  }
  
  def asCharVecType: VecType = {
    tpe match {
      case Seq() => NoneVecType(tokens)
      case Seq(_: BoolType) => 
      val ui = UndefinedInterval
      PackedVecType(
        tokens,
        Seq(UIntType(ui, Width(ui, Number(ui,"8",NumberDecimal)), NumberDecimal)),
        getWidth8,
        downto
      )
      case _ => Utils.throwInternalError("Unable to convert to CharVec")
    }
  }
  
  def asSIntType: SIntType = {
    tpe match {
      case Seq() => SIntType(tokens, Width(0))
      case Seq(_: BoolType) => SIntType(tokens, Width(UndefinedInterval, getLen))
      case _ => Utils.throwInternalError("Unable to convert to UInt")
    }
  }
  
}

case class NoneVecType(tokens: Interval) extends VecType {
  type T = NoneVecType
  val downto: Boolean = false
  val tpe: Seq[Type] = Seq()
  val bound : Expression = UndefinedExpression()
  def serialize: String = s"NoneVecType)"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapType(f: Type => Type): NoneVecType = this
  // TO DO ? add override of cast methods as errors ? 
  def mapWidth(f: Width => Width) = this
  def mapBound(f: Expression => Expression) = this
}


case class SomeVecType(tokens: Interval, tpe: Seq[Type], bound: Expression, downto: Boolean) extends VecType {
  type T = SomeVecType
  def serialize: String = s"SomeVecType($downtoStr,${bound.serialize},${tpe.map(t => t.serialize).mkString("(",",",")")})"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapType(f: Type => Type) = this.copy(tpe = tpe.map(f))
  def mapWidth(f: Width => Width) = this.copy(bound = f(Width(bound)).expr)
  def mapBound(f: Expression => Expression)= this.copy(bound = f(bound))
}

case class PackedVecType(tokens: Interval, tpe: Seq[Type], bound: Expression, downto: Boolean) extends VecType {
  type T = PackedVecType
  def serialize: String = s"PackedVecType($downtoStr,${bound.serialize},${tpe.map(t => t.serialize).mkString("(",",",")")})"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapType(f: Type => Type) = this.copy(tpe = tpe.map(f))
  def mapWidth(f: Width => Width) = this.copy(bound = f(Width(bound)).expr)
  def mapBound(f: Expression => Expression)= this.copy(bound = f(bound))
}

case class UnpackedVecType(tokens: Interval, tpe: Seq[Type], bound: Expression, downto: Boolean) extends VecType {
  type T = UnpackedVecType
  def serialize: String = s"UnpackedVecType($downtoStr,${bound.serialize},${tpe.map(t => t.serialize).mkString("(",",",")")})"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapType(f: Type => Type) = this.copy(tpe = tpe.map(f))
  def mapWidth(f: Width => Width) = this.copy(bound = f(Width(bound)).expr)
  def mapBound(f: Expression => Expression)= this.copy(bound = f(bound))
}

case class ClockType(tokens: Interval) extends GroundType {
  type T = ClockType
  val width = Width(1)
  def serialize: String = "Clock"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = this
  def foreachWidth(f: Width => Unit): Unit = Unit
}
/* Abstract reset, will be inferred to UInt<1> or AsyncReset */
case class ResetType(tokens: Interval) extends GroundType {
  type T = ResetType
  val width = Width(1)
  def serialize: String = "Reset"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = this
  def foreachWidth(f: Width => Unit): Unit = Unit
}
case class AsyncResetType(tokens: Interval) extends GroundType {
  type T = AsyncResetType
  val width = Width(1)
  def serialize: String = "AsyncReset"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapWidth(f: Width => Width) = this
  def foreachWidth(f: Width => Unit): Unit = Unit
}

case class UnknownType(tokens: Interval) extends Type {
  type T = UnknownType
  def serialize: String = s"<?>"
  def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
  def mapType(f: Type => Type) = this
  def mapWidth(f: Width => Width) = this
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachWidth(f: Width => Unit): Unit = Unit
  def widthOption: Option[Width] = None
}
object UnknownType {
  def apply(): UnknownType = UnknownType(UndefinedInterval)
}

/** [[Port]] Direction */
sealed abstract class Direction extends SVNode {
  type T <: Direction
}
case class Input(tokens: Interval) extends Direction {
  type T = Input
  def serialize: String = "Input"
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
}
case class Output(tokens: Interval) extends Direction {
  type T = Output
  def serialize: String = "Output"
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
}
case class Inout(tokens: Interval) extends Direction {
  type T = Inout
  def serialize: String = "Inout"
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
}

/** [[DefModule]] Port */
case class Port(
    tokens: Interval, 
    attributes: VerilogAttributes,
    name: String,
    direction: Direction,
    tpe: Type,
    resolution: LogicResolution,
    // contains original port name if modified
    isDefaultClock: Option[String] = None,
    isDefaultReset: Option[String] = None
  ) extends Statement with IsDeclaration {
  type T = Port
  private def serial_init : String = resolution match {
    case LogicWire(e) => s"= ${e.serialize}"
    case _ => ""
  }
  def serialize: String = attributes.serialize + s"${direction.serialize} ${resolution.serialize} $name : <${tpe.serialize}> ${serial_init}" 

  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def mapStmt(f: Statement => Statement): T = this
  def mapExpr(f: Expression => Expression): T = this.copy(resolution=resolution.mapExpr(f))
  def mapType(f: Type => Type): T = this.copy(tpe=f(tpe))
  def mapString(f: String => String): T = this.copy(name = f(name))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes): T = this.copy(attributes = f(attributes))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = resolution.foreachExpr(f)
  def foreachType(f: Type => Unit): Unit = f(tpe)
  def foreachString(f: String => Unit): Unit = f(name)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}

/** Parameters for modules */
sealed abstract class Param extends SVNode {
  type T <: Param
  def name: String
  def serialize: String = s"parameter $name = "
}
/** Integer (of any width) Parameter */
case class IntParam(tokens: Interval, name: String, value: BigInt) extends Param {
  type T = IntParam
  override def serialize: String = super.serialize + value
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
}
/** IEEE Double Precision Parameter (for Verilog real) */
case class DoubleParam(tokens: Interval, name: String, value: Double) extends Param {
  type T = DoubleParam
  override def serialize: String = super.serialize + value
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
}
/** String Parameter */
case class StringParam(tokens: Interval, name: String, value: StringLit) extends Param {
  type T = StringParam
  override def serialize: String = super.serialize + value.escape
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
}
case class RawStringParam(tokens: Interval, name: String, value: String) extends Param {
  type T = RawStringParam
  override def serialize: String = super.serialize + s"'${value.replace("'", "\\'")}'"
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
}

/** Expression Param for all parameter to be handled  */
case class DefParam(
    tokens: Interval, 
    attributes: VerilogAttributes, 
    name: String, 
    tpe: Type,
    value: Option[Expression],
    kind: ExpressionKind
  ) extends Statement with IsDeclaration {
  type T = DefParam
  def serialize: String = {
    val header = attributes.serialize + s"param $name: <${tpe.serialize}@${kind.serialize}>" 
    value match {
      case Some(e) => header + s" = ${e.serialize}"
      case _ => header
    }
  }
  def mapStmt(f: Statement => Statement) = this
  def mapExpr(f: Expression => Expression) = value match {
    case Some(e) => this.copy(value = Some(f(e)))
    case _ => this
  }
  def mapType(f: Type => Type) = this.copy(tpe = f(tpe))
  def mapString(f: String => String) = this.copy(name = f(name))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def foreachStmt(f: Statement => Unit): Unit = Unit
  def foreachExpr(f: Expression => Unit): Unit = value match {
    case Some(e) => f(e)
    case _ => 
  }
  def foreachType(f: Type => Unit): Unit = Unit
  def foreachString(f: String => Unit): Unit = f(name)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}

case class FullType(
  tpe: Type, 
  kind: ExpressionKind, 
  tpeRef: Boolean = false,
  portRefs: Seq[Port] = Seq(), // use-case: for DefDunction args
  implicitPath: Seq[String] = Seq() // use-case: legalize flat Enum access
){
  def serialize: String = s"${tpe.serialize}@${kind.serialize}${if(tpeRef) "(type)" else ""}"
}
case class WRef(
  name: String, 
  path: Seq[String] = Seq()
){
  def in(prepend: String): WRef = this.copy(path = prepend +: path)
  def serialize: String = (path :+ name).mkString(".")
}

case class DefPackage(
  tokens: Interval,
  attributes : VerilogAttributes,
  name : String,
  body : Statement,
  refs: Option[HashMap[WRef, FullType]] = None
) extends Description with IsDeclaration {
  type T = DefPackage
  def serialize: String = s"package $name:" + indent("\n" + body.serialize)
  def mapStmt(f: Statement => Statement) = this.copy(body = f(body))
  def mapString(f: String => String) = this.copy(name = f(name))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes) = this.copy(attributes = f(attributes))
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  def foreachStmt(f: Statement => Unit): Unit = {f(body)}
  def foreachString(f: String => Unit): Unit = f(name)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
}

trait FetchPort {
  def body: Statement
  
  // dynamically retrieve ports
  private def fetchPorts(stmt: Statement): Seq[Port] = {
    stmt match {
      case p: Port => Seq(p)
      case st => 
        // ugly imperative style, missing a real map option on the IR 
        val ports = ArrayBuffer[Seq[Port]]()
        st.foreachStmt(s => ports += fetchPorts(s))
        ports.toSeq.flatten
    }
  }
  lazy val ports : Seq[Port] = fetchPorts(body)
  
  
  // map need the copy
  protected def mapPortStatement(s: Statement, f: Port => Statement): Statement = {
    s match {
      case p: Port => f(p)
      case st => st.mapStmt(s => mapPortStatement(s, f))
    }
  }
  
  protected def insertAfterPortsInner(toInsert: Statement): Statement = mapPortStatement(body, p => {
    if(p.name == ports.last.name){
      toInsert match {
        case b: Block => b.prependStmts(Seq(p))
        case s => SimpleBlock(p.tokens, Seq(p, s))
      }
    } else {
      p
    }
  })
}

/** Base class for modules */
sealed abstract class DefModule extends Description with IsDeclaration with FetchPort {
  val attributes : VerilogAttributes
  val name : String
  val params : Seq[DefParam]
  val clock : Option[String]
  val reset: Option[String]
  val body: Statement
  protected def serializeHeader(tpe: String): String =
    s"$tokens: $tpe $name :${attributes.serialize}${indent(params.map("\n" + _.serialize).mkString)}${indent(ports.map("\n" + _.serialize).mkString)}\n"
  type T <: DefModule

  // foreach can be implemented right away (no need for copy)
  def foreachPort(f: Port => Unit): Unit = ports.foreach(f)
  def foreachParam(f: DefParam => Unit): Unit = params.foreach(f)
  def foreachStmt(f: Statement => Unit): Unit = f(body)
  def foreachString(f: String => Unit): Unit = f(name)
  def foreachVerilogAttributes(f: VerilogAttributes => Unit): Unit = f(attributes)
  
  def mapPort(f: Port => Statement): T
  def mapParam(f: DefParam => DefParam): T
}
/** Internal Module
  *
  * An instantiable hardware block
  */
case class Module(
    tokens: Interval,
    attributes: VerilogAttributes,
    name: String,
    params: Seq[DefParam],
    body: Statement,
    clock : Option[String] = None,
    reset: Option[String] = None
  ) extends DefModule {
  type T = Module
  def serialize: String = serializeHeader("module") + indent("\n" + body.serialize)

  def mapPort(f: Port => Statement): T = this.copy(body = mapPortStatement(body, f))
  def insertAfterPorts(toInsert: Statement): T = this.copy(body = insertAfterPortsInner(toInsert))
  
  def mapParam(f: DefParam => DefParam): T = this.copy(params = params map f)
  def mapStmt(f: Statement => Statement): T = this.copy(body = f(body))
  def mapString(f: String => String): T = this.copy(name = f(name))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes): T = this.copy(attributes = f(attributes))
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
  
}

/** External Module
  *
  * Used for Verilog black boxes
  */
case class ExtModule(
    tokens: Interval, 
    attributes: VerilogAttributes,
    name: String,
    params: Seq[DefParam],
    body: Statement, // contain only port statements
    resourcePath: Option[String],
    clock : Option[String] = None,
    reset: Option[String] = None,
    paramMap: Seq[NamedAssign] = Seq()
  ) extends DefModule {
  type T = ExtModule
  
  def serialize: String = serializeHeader("extmodule")
  
  def mapStmt(f: Statement => Statement): T = this.copy(body = f(body))
  def mapPort(f: Port => Statement): T = this.copy(body = mapPortStatement(body, f))
  def mapParam(f: DefParam => DefParam): T = this.copy(params = params map f)
  def mapString(f: String => String): T = this.copy(name = f(name))
  def mapVerilogAttributes(f: VerilogAttributes => VerilogAttributes): T = this.copy(attributes = f(attributes))
  def mapInterval(f: Interval => Interval) = this.copy(tokens=f(tokens))
}

