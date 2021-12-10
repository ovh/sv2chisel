// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import sv2chisel.Utils.{time}
import sv2chisel.transforms._

import logger._

object Driver extends EasyLogging {
  
  def lintVerilog(project: Project, options: TranslationOptions): Unit = {
    val transforms = Seq(
      // to do : unused signals, ... ?
      new CheckUseBeforeDecl(options),
      new CheckScopes(options),
      new InferDefLogicClocks(options),
      new TypeReferences(options), // warn about undeclared references
      new LegalizeParamDefaults(options) // To adapt just to warn about usage ??? "time to switch to chisel "
    )
    struct(s"######### Executing ${transforms.size} transforms #########")
    val (timeTransforms, _) = time { project.run(transforms) }
    struct(s"# Total Elapsed time running transforms : $timeTransforms ms")
  }
  
  def emitChisel(
    project: Project, 
    options: TranslationOptions, 
    emissionBasePath: String = "chisel_gen", 
    noFileIO: Boolean = false
  ): String = {
    val transforms = Seq(
      // Core transforms 
      new CheckUseBeforeDecl(options),
      new CheckScopes(options),
      new CheckBlockingAssignments(options),
      new InferDefLogicClocks(options),
      new PropagateClocks(options),
      new FlowReferences(options),
      new InferUInts(options), // requires flows
      new InferParamTypes(options),
      new TypeReferences(options), // should run after InferUInts & InferParamTypes for proper type propagation to refs
      new LegalizeExpressions(options), // Requires TypedReferences
      
      // Emission Oriented Transforms
      new FixFunctionImplicitReturns(options),
      new NameInstancePorts(options),
      new RemovePatterns(options),
      new RemoveConcats(options),
      new AddDontCare(options),
      new LegalizeParamDefaults(options) // needs typed parameters
    )
    struct(s"######### Executing ${transforms.size} transforms #########")
    val (timeTransforms, _) = time { project.run(transforms) }
    struct(s"# Total Elapsed time running transforms : $timeTransforms ms")
    
    struct(s"######### EMISSION #########")
    Emitter.emitChisel(ScalaStyleEmission(project, emissionBasePath, options), noFileIO)
  }

}

