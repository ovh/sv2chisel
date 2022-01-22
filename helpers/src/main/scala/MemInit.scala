// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel.helpers

import chisel3.{Mem, Vec, Data}

import firrtl.annotations.{MemoryArrayInitAnnotation, MemoryScalarInitAnnotation}
import chisel3.experimental.{ChiselAnnotation, annotate, DataMirror}


object MemInit {
  /*** INIT FROM DATA (expecting literals) **/
  
  // providing similar syntax to VecInit
  def apply[T <: Data](initLiteral: T, initLiterals: T*): Mem[T] = {
    apply(initLiteral +: initLiterals)
  }
  // providing similar syntax to VecInit
  def apply[T <: Data](initLiterals: Seq[T]): Mem[T] = {
    doApply(initLiterals)
  }
  
  private def doApply[T <: Data](initLiterals: Seq[T]): Mem[T] = {
    require(!initLiterals.isEmpty, "MemInit requires at least element: cannot create zero-sized memories")
    val tpe = initLiterals.head
    initLiterals.foreach(t => {
      require(DataMirror.checkTypeEquivalence(t, tpe), s"All MemInit elements must share a common type ($tpe selected)")
      require(t.isLit, s"All MemInit elements must be literals. Failing at: $t")
    })
    apply(initLiterals.length, tpe, initLiterals.map(_.litValue))
  }
  
  // allow syntax similar to RegInit(VecInit(...)) => MemInit(VecInit())
  def apply[T <: Data](v: Vec[T]): Mem[T] = {
    doApply(v.toSeq) // unexpected hanging with scala 2.13.+ requires to use this do_apply intermediate
  }

  /*** INIT FROM BigInt **/

  def apply[T <: Data](size: Int, tpe: T, inits: Seq[BigInt]): Mem[T] = {
    require(size == inits.length, "Init vector shall provide a value for each element of the memory")
    val m = Mem(size, tpe.cloneType)
    annotate(new ChiselAnnotation {
      override def toFirrtl = MemoryArrayInitAnnotation(m.toTarget, values = inits)
    })
    m
  }
  
  def fill[T <: Data](size: Int, tpe: T)(eltInit: => BigInt): Mem[T] = {
    val m = Mem(size, tpe.cloneType)
    annotate(new ChiselAnnotation {
      override def toFirrtl = MemoryScalarInitAnnotation(m.toTarget, value = eltInit)
    })
    m
  }
    
  
  def tabulate[T <: Data](size: Int, tpe: T)(f: Int => BigInt): Mem[T] = {
    apply(size, tpe, (0 until size).map(f))
  }
  
}

