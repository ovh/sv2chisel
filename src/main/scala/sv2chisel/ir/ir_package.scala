// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel 

package object ir {
  type Interval = org.antlr.v4.runtime.misc.Interval // avoid import in all files
  implicit def svnode2Interval[T <: SVNode](n:T): Interval = n.tokens
}

