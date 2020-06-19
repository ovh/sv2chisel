// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package ir

import logger.LazyLogging

/** Definitions and Utility functions for [[ir.PrimOp]]s */
object PrimOps extends LazyLogging {
  abstract class UnaryOp extends PrimOp {
    def expectedArgs: Int = 1
  }
  abstract class BinaryOp extends PrimOp {
    def expectedArgs: Int = 2
  }
  // distinguish operators that are returning bool
  sealed trait OpKind
  sealed trait BoolOp extends OpKind
  sealed trait NumOp extends OpKind
  sealed trait ShiftOp extends OpKind
  sealed trait BitOp extends OpKind
  sealed trait RedOp extends OpKind
  sealed trait UncharOp extends OpKind

  case class Par(tokens: Interval) extends UnaryOp with UncharOp {
    type T = Par
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "par"
  }
  
  case class Add(tokens: Interval) extends BinaryOp with NumOp {
    type T = Add
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "+"
  }
  case class Sub(tokens: Interval) extends BinaryOp with NumOp {
    type T = Sub
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "-"
  }
  case class Mul(tokens: Interval) extends BinaryOp with NumOp {
    type T = Mul
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "*"
  }
  case class Pow(tokens: Interval) extends BinaryOp with NumOp {
    type T = Pow
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "**" // not supported by default in scala, use math.pow() or 1 << n for 2**n
  }
  case class Div(tokens: Interval) extends BinaryOp with NumOp {
    type T = Div
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "/"
  }
  case class Rem(tokens: Interval) extends BinaryOp with NumOp {
    type T = Rem
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "%"
  }
  case class Lt(tokens: Interval) extends BinaryOp with BoolOp {
    type T = Lt
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "<"
  }
  case class Leq(tokens: Interval) extends BinaryOp with BoolOp {
    type T = Leq
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "<="
  }
  case class Gt(tokens: Interval) extends BinaryOp with BoolOp {
    type T = Gt
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = ">"
  }
  case class Geq(tokens: Interval) extends BinaryOp with BoolOp {
    type T = Geq
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = ">="
  }
  
  case class Eq(tokens: Interval) extends BinaryOp with BoolOp {
    type T = Eq
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "eq" // emission depends on context scala vs chisel
  }
  
  case class Neq(tokens: Interval) extends BinaryOp with BoolOp {
    type T = Neq
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "!=" // emission depends on context scala vs chisel?
  }
  
  // LOGIC
  case class Not(tokens: Interval) extends UnaryOp with BoolOp {
    type T = Not
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "!"
  }
  case class And(tokens: Interval) extends BinaryOp with BoolOp {
    type T = And
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "&&"
  }
  case class Or(tokens: Interval) extends BinaryOp with BoolOp {
    type T = Or
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "||"
  }
  
  // Bitwise operator
  case class BitNeg(tokens: Interval) extends UnaryOp with BitOp {
    type T = BitNeg
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "~"
  }
  case class BitAnd(tokens: Interval) extends BinaryOp with BitOp {
    type T = BitAnd
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "&"
  }
  case class BitOr(tokens: Interval) extends BinaryOp with BitOp {
    type T = BitOr
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "|"
  }
  case class BitXor(tokens: Interval) extends BinaryOp with BitOp {
    type T = BitXor
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "^"
  }
  case class BitXnor(tokens: Interval) extends BinaryOp with BitOp {
    type T = BitXnor
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "~^"
  }
  
  // Reduction operator
  case class OrRed(tokens: Interval) extends UnaryOp with RedOp {
    type T = OrRed
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "|"
  }
  case class AndRed(tokens: Interval) extends UnaryOp with RedOp {
    type T = AndRed
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "&"
  }
  case class XorRed(tokens: Interval) extends UnaryOp with RedOp {
    type T = XorRed
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "^"
  }
  case class NorRed(tokens: Interval) extends UnaryOp with RedOp {
    type T = NorRed
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "~|"
  }
  case class NandRed(tokens: Interval) extends UnaryOp with RedOp {
    type T = NandRed
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "~&"
  }
  case class XnorRed(tokens: Interval) extends UnaryOp with RedOp {
    type T = XnorRed
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "~^"
  }

  
  // Shift operators
  // MAJOR WARNING:
  // Scala >> is arithmetic shift while in verilog >> is logic shift
  // Scala >>> is logic shift
  // to string is intended for scala emission
  case class Shl(tokens: Interval) extends BinaryOp with ShiftOp {
    type T = Shl
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "<<"
  }
  case class Shr(tokens: Interval) extends BinaryOp with ShiftOp {
    type T = Shr
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = ">>"
  }
  case class LogShr(tokens: Interval) extends BinaryOp with ShiftOp {
    type T = LogShr
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "LogicalShiftRight"
  }
  
  case class Incr(tokens: Interval, prefix: Boolean = false) extends UnaryOp with NumOp {
    type T = Incr
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "++"
  }
  case class Decr(tokens: Interval, prefix: Boolean = false) extends UnaryOp with NumOp {
    type T = Decr
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "--"
  }
  case class Plus(tokens: Interval) extends UnaryOp with NumOp {
    type T = Plus
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "+"
  }
  case class Minus(tokens: Interval) extends UnaryOp with NumOp {
    type T = Minus
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "-"
  }
  
  case class CeilLog2(tokens: Interval) extends UnaryOp with NumOp {
    type T = CeilLog2
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "CeilLog2"
  }
  case class GetWidth(tokens: Interval) extends UnaryOp with NumOp {
    type T = GetWidth
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    override def toString = "GetWidth"
  }
  
  case class InlineIf(tokens: Interval) extends PrimOp with UncharOp {
    type T = InlineIf
    def mapInterval(f: Interval => Interval) = this.copy(tokens = f(tokens))
    def expectedArgs: Int = 3
    override def toString = "InlineIf"
  }

}
