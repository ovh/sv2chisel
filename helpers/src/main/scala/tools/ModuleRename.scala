package sv2chisel.helpers.tools

import chisel3.experimental.RunFirrtlTransform

import firrtl._
import firrtl.renamemap.MutableRenameMap
import firrtl.ir._
import firrtl.annotations._
import firrtl.options.Dependency

import logger.LazyLogging

/** Common trait for ModuleRename annotations
 *
 *  These annotations shall carry the following informations:
 *    - suffix to be applied to all module & instances within the circuit
 *    - prefix to be applied to all submodules (and the top if doPrefixTop)
 */
trait ModuleRenameAnnotation {
  def prefix: String
  def suffix: String
  def doPrefixTop: Boolean
  def topOnly: Boolean

  /** Both prefix & suffix are always applied for submodules
   *
   *  Note that "suffix" is applied on prefix to match hierarchical naming with suffixing of top name
   */
  final def newSubModuleName(oldName: String): String = s"${prefix}_${suffix}_$oldName"

  /** for top module suffix is always applied but prefix is applied only when doPrefixTop is true */
  final def newTopName(oldName: String) = if (doPrefixTop) s"${prefix}_${oldName}_$suffix" else s"${oldName}_$suffix"
}

/** ModuleRename firrtl annotation simply implementing the common trait as val of a case class */
case class ModuleRenameFirrtlAnnotation(
    prefix: String,
    suffix: String,
    doPrefixTop: Boolean,
    topOnly: Boolean
) extends NoTargetAnnotation
    with ModuleRenameAnnotation

/** ModuleRename chisel annotation implemnting the ModuleRenameAnnotation trait
 *
 *  This chisel annotation has multiple roles:
 *    - it records prefix and suffix to be applied to all module & instances within the circuit
 *    - it adds the transform `ModuleRename` to the firrtl compilation flow
 *    - it is converted into the firrtl's ModuleRenameAnnotation
 */
case class ModuleRenameChiselAnnotation(
    prefix: String,
    suffix: String,
    doPrefixTop: Boolean = false,
    topOnly: Boolean = false
) extends RunFirrtlTransform
    with ModuleRenameAnnotation {
  def transformClass = classOf[ModuleRename]
  def toFirrtl       = ModuleRenameFirrtlAnnotation(prefix, suffix, doPrefixTop, topOnly)
}

/** Transform adding a suffix to all module/instances of the circuit
 *
 *  The suffix applies globally (without any filter) and is provided by the firrtl's `ModuleRenameAnnotation`
 */
class ModuleRename extends Transform with DependencyAPIMigration with LazyLogging {
  // Wiring is not a prerequisite but when mixed-in (by BoringUtils), it must be run before the renaming
  override val prerequisites             = firrtl.stage.Forms.LowForm
  override val optionalPrerequisites     = Seq(Dependency[firrtl.passes.wiring.WiringTransform])
  override val dependents                = Seq.empty
  override def invalidates(a: Transform) = false

  /** Actual IR-modifying function, renaming all modules & instances
   *
   *  @param circuit
   *    the circuit
   *  @param anno
   *    ModuleRenameAnnotation providing the renaming functions
   *  @return
   *    updated circuit with the associated RenameMap
   */
  def update(circuit: Circuit, anno: ModuleRenameAnnotation): (Circuit, RenameMap) = {
    // This RenameMap is required such that the firrtl compiler is able to track external references to elements of this
    // circuit which includes (most notably) TargetAnnotations, such as the ones used for preset annotation
    // Not providing this RenameMap leads to immediate crash in the following transforms (where last is the emitter)
    val renames = MutableRenameMap()

    val prevTarget = CircuitTarget(circuit.main)

    // circuit main must be renamed as well to match the actual name of top module
    val newMain   = anno.newTopName(circuit.main)
    val newTarget = CircuitTarget(newMain)
    renames.record(prevTarget, newTarget)

    /** Update module instances references, still required despite the rename map
     *
     *  Renaming both blackboxes and vanilla instances because it would be harder to filter blackbox out. This is an
     *  internal reference name only and does not impact the actual user "desiredName" actually emitted.
     */
    def processStatements(stmt: Statement): Statement = {
      stmt match {
        case i: DefInstance if(!anno.topOnly) =>
          val newRef = anno.newSubModuleName(i.module) // there cannot be any instances of main
          logger.debug(s"[debug] Updating reference for instance ${i.name} from ${i.module} to ${newRef}")
          i.copy(module = newRef)
        case _ => stmt.mapStmt(processStatements)
      }
    }

    /** Update module name, look for instances within modules' statements */
    def processModule(d: DefModule): DefModule = {
      val newName = if (d.name == circuit.main) newMain else anno.newSubModuleName(d.name)
      d.mapStmt(processStatements) match {
        case m: Module if(!anno.topOnly || d.name == circuit.main) =>
          logger.debug(s"[debug] Renaming module ${d.name} into ${newName}")
          renames.record(prevTarget.module(d.name), newTarget.module(newName))
          m.copy(name = newName)
        case m: ExtModule if(!anno.topOnly) =>
          // this rename does not override the desiredName (m.defname)
          logger.debug(s"[debug] Renaming external module (blackbox) ${d.name} into ${newName}")
          renames.record(prevTarget.module(d.name), newTarget.module(newName))
          m.copy(name = newName)
        case _ => d
      }
    }

    // modules are naturally processed in reverse hierarchical order
    // here we have no dependency requirement thanks to the static suffix
    // Do not forget to rename the main module (pointer to the top module)
    (circuit.copy(main = newMain, modules = circuit.modules.map(processModule)), renames)
  }

  /** Main method: entry point for the transform
   *    - triggers only if a ModuleRenameAnnotation is found
   *    - if multiple ModuleRenameAnnotation are found, only the first one is considered
   */
  def execute(state: CircuitState): CircuitState = {
    state.annotations.collectFirst { case a: ModuleRenameFirrtlAnnotation => a } match {
      case Some(anno) =>
        val (circuit, renames) = update(state.circuit, anno)
        state.copy(circuit = circuit, renames = Some(renames))

      case _ => state // No ModuleRenameFirrtlAnnotation => no need to walk the IR
    }
  }
}
