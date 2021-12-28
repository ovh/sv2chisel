package sv2chisel.helpers.tools

import chisel3.experimental.{RunFirrtlTransform}

import firrtl._
import firrtl.PrimOps._
import firrtl.ir._
import firrtl.ir.{AsyncResetType}
import firrtl.annotations._

import logger.LazyLogging

import scala.collection.mutable.{ArrayBuffer, HashSet}

case class ModulePresetAnnotation(target: ModuleTarget) extends SingleTargetAnnotation[ModuleTarget] {
  override def duplicate(n: ModuleTarget): ModulePresetAnnotation = this.copy(target = n)
}

case class ModulePresetChiselAnnotation(target: ModuleTarget) extends RunFirrtlTransform {
  def transformClass = classOf[ModulePreset]          // scalastyle:off public.methods.have.type
  def toFirrtl       = ModulePresetAnnotation(target) // scalastyle:off public.methods.have.type
}

/** Convert Top Reset Type of Annotated Modules For each module with expected annotation `ModulePresetAnnotation`
 *    - convert reset IO of any Type into AsyncResetType
 *    - add `PresetAnnotation` on this IO
 *
 *  Caveats:
 *    - Expected result is seen only at the emission : no more reset signal
 *    - Firrtl emission will only reflect modification of the Reset into AsyncResetType
 */
class ModulePreset extends Transform with DependencyAPIMigration with LazyLogging {

  override val prerequisites                    = firrtl.stage.Forms.HighForm
  override val dependents                       = Seq.empty
  override def invalidates(a: firrtl.Transform) = false

  /** Recursively update all annotated modules
   *    - annotated modules are removed from the moduleSet
   *    - instance declared inside an annotated modules are added to the moduleSet
   *    - run again until moduleSet is empty
   *
   *  @param circuit
   *    the circuit
   *  @param annotations
   *    all the annotations
   *  @return
   *    updated annotations
   */
  def update(
      cs: CircuitState,
      presetModules: Set[ModuleTarget],
      topPresetModules: Set[ModuleTarget]
  ): CircuitState = {

    // Annotations to be appended and returned as result of the transform
    val annos         = cs.annotations.to[ArrayBuffer]
    val moduleSet     = presetModules.to[HashSet]
    val circuitTarget = CircuitTarget(cs.circuit.main)

    /** Update annotated module
     *    - convert reset into AsyncResetType
     *    - add preset annotation for top level port only (useless to add for all)
     */
    def processModule(m: DefModule): DefModule = {
      val moduleTarget = circuitTarget.module(m.name)

      def processPorts(port: Port): Port = {
        if (port.name == "reset") {
          logger.debug(s"[debug] Update reset of ${m.name}")
          val target = moduleTarget.ref(port.name)
          if (topPresetModules.contains(moduleTarget)) annos += PresetAnnotation(target)
          port.copy(tpe = AsyncResetType)
        } else {
          port
        }
      }

      def processStatements(stmt: Statement): Statement = {
        stmt match {
          case i: WDefInstance =>
            logger.debug(
                s"[debug] Registering instance ${i.name} of ${i.module} for AsyncReset Propagation"
            )
            moduleSet += circuitTarget.module(i.module)
            i
          case _ => stmt.mapStmt(processStatements)
        }
      }

      if (moduleSet.contains(moduleTarget)) {
        moduleSet -= moduleTarget
        m.mapPort(processPorts).mapStmt(processStatements)
      } else {
        m
      }
    }
    // ensure that modules are processed in hierarchy order (avoid multiple runs)
    val modules = cs.circuit.modules.reverse.map(processModule)
    val circuit = cs.circuit.copy(modules = modules.reverse)
    val result  = cs.copy(circuit = circuit, annotations = annos)
    if (moduleSet.isEmpty) {
      result
    } else {
      logger.warn("[info] Re-running ModulePreset Propagation")
      update(result, moduleSet.toSet, topPresetModules)
    }
  }

  def execute(state: CircuitState): CircuitState = {
    // Collect all user-defined PresetAnnotation
    val presets = state.annotations
      .collect { case m: ModulePresetAnnotation => m }
      .groupBy(_.target)
      .keySet
    // No ModulePresetAnnotation => no need to walk the IR
    if (presets.size == 0) {
      state
    } else {
      update(state, presets, presets)
    }
  }
}
