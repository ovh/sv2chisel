// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import sv2chisel.Utils.{time}
import sv2chisel.transforms._

import logger._

object Driver extends EasyLogging {
  
  def lintVerilog(project: Project): Unit = {
    val transforms = Seq(
      // to do : unused signals, ... ?
      new CheckUseBeforeDecl,
      new CheckScopes,
      new InferDefLogicClocks,
      new TypeReferences, // warn about undeclared references
      new LegalizeParamDefaults // To adapt just to warn about usage ??? "time to switch to chisel "
    )
    struct(s"######### Executing ${transforms.size} transforms #########")
    val (timeTransforms, _) = time { project.run(transforms) }
    struct(s"# Total Elapsed time running transforms : $timeTransforms ms")
  }
  
  def emitChisel(project: Project, emissionBasePath: String = "chisel_gen"): String = {
    val transforms = Seq(
      // Core transforms 
      new CheckUseBeforeDecl,
      new CheckScopes,
      new CheckBlockingAssignments,
      new InferDefLogicClocks,
      new PropagateClocks,
      new FlowReferences,
      new InferUInts, //Some(LogLevel.Trace)), // requires flows
      new TypeReferences, // should run after InferUInts for infered UInt type propagation
      new LegalizeExpressions, // TO DO - requires TypedReferences; no more concats
      
      // Emission Oriented Transforms
      new FixFunctionImplicitReturns,
      new NameInstancePorts,
      new RemovePatterns,
      new RemoveConcats,
      new InferParamTypes,
      new LegalizeParamDefaults // needs param types
    )
    struct(s"######### Executing ${transforms.size} transforms #########")
    val (timeTransforms, _) = time { project.run(transforms) }
    struct(s"# Total Elapsed time running transforms : $timeTransforms ms")
    
    struct(s"######### EMISSION #########")
    Emitter.emitChisel(ScalaStyleEmission(project, emissionBasePath))
  }

}

