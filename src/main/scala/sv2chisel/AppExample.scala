// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import logger._

// see README for another template example
/** Simple application example using example files provided in resource folder */
object SV2ChiselAppExample extends App {
  Logger.setLevel(LogLevel.Info)
  
  val basePath = "src/main/resources/project/hdl"
  val files = Seq(
    "my_package.sv",
    "my_module.sv"
  )
  val project = Project("project", basePath, files)
  
  Driver.emitChisel(project, "chisel_gen")

}
