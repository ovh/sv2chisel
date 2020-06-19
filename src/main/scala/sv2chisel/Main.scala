// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import sv2chisel.ir._
import sv2chisel.Utils.{time}
import sv2chisel.transforms._

import logger._


object SV2ChiselApp extends App { // TODO: CLI options
  // Loglevel should definitely be controlled through CLI options 
  Logger.setLevel(LogLevel.Info)
  
  val basePath = "../github/picorv32"
  val files = Seq(
    "picorv32.v",
  )
  val project = Project("picorv32", basePath, files)
  
  Driver.emitChisel(project, "chisel_gen")

}
