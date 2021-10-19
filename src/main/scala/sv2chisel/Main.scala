// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import logger._

// see README for another template example
object SV2ChiselApp extends App {
  Logger.setLevel(LogLevel.Info)
  
  val basePath = "tree_filters"
  val files = Seq(
    "tf_p.sv",
  )
  val project = Project("tree_filters", basePath, files)
  
  Driver.emitChisel(project, "chisel_gen")

}
