package sv2chisel.helpers.tools

import chisel3.{RawModule}
import chisel3.stage._
import chisel3.internal.firrtl.Circuit
import chisel3.experimental.{ChiselAnnotation}

import firrtl.{AnnotationSeq, EmittedVerilogCircuitAnnotation}
import firrtl.annotations.CircuitTarget
import firrtl.options.{Dependency, Phase}

import java.io.File

/** Custom Wrapper enforcing the default directory to be chisel_gen for all generated files */
class ChiselGenMain() extends App {
  val ar = args.isEmpty match {
    case false =>
      if (args.contains("--target-dir")) {
        args
      } else {
        args ++ Array("--target-dir", "chisel_gen")
      }
    case true => Array("--target-dir", "chisel_gen")
  }

  /** Usual chisel emission of the given module
   *  @param m
   *    chisel module (pass-by-name)
   *  @return
   *    verilog as string (verilog is also written at chisel_gen/ )
   */
  def emit(m: => RawModule): String = {
    ChiselGen.emit(m, ar)
  }

  /** Usual chisel emission of the given module towards FIRRTL
   *  @param m
   *    chisel module (pass-by-name)
   *  @return
   *    Firrtl as file (verilog is also written at chisel_gen/ )
   */
  def emitFirrtl(m: => RawModule): String = {
    (new ChiselStage).emitFirrtl(m, ar)
  }

  /** Emission of the given module with when deeply applied
   *  @param m
   *    chisel module (pass-by-name)
   *  @return
   *    verilog as string (verilog is also written at chisel_gen/ )
   */
  def emitDeepWhen(m: => RawModule): String = {
    val annos = Seq(firrtl.stage.RunFirrtlTransformAnnotation(new DeepWhen))
    (new ChiselStage).emitVerilog(m, ar, annos)
  }

  /** Emission of the given module with reset converted to preset
   *  @param m
   *    chisel module (pass-by-name)
   *  @return
   *    verilog as string (verilog is also written at chisel_gen/ )
   */
  def emitPreset(m: => RawModule): String = {
    ChiselGen.emitPreset(m, ar)
  }
}

/** Wrapper for AnnotationSeq corresponding to elaboration result
 *
 *  Provides helpers for underlying circuit update
 */
case class ElaboratedDesign(raw: AnnotationSeq) {
  // Circuit declaration is in chisel3 core => outside main src folder => not registered in API DOC
  // For reference, here is the declaration:
  // case class Circuit(name: String, components: Seq[Component], annotations: Seq[ChiselAnnotation] = Seq.empty)

  lazy val circuit                            = raw.collectFirst { case ChiselCircuitAnnotation(c) => c }.get
  lazy val chiselAnnos: Seq[ChiselAnnotation] = circuit.annotations
  lazy val name                               = circuit.name
  lazy val designOption                       = raw.collectFirst { case chisel3.stage.DesignAnnotation(d) => d }

  def mapCircuit(f: Circuit => Circuit): ElaboratedDesign = {
    this.copy(raw = raw.map(a => {
      a match {
        case ChiselCircuitAnnotation(c) => ChiselCircuitAnnotation(f(c))
        case _                          => a
      }
    }))
  }

  def mapName(f: String => String): ElaboratedDesign = mapCircuit(c => c.copy(name = f(c.name)))
  def mapChiselAnnos(f: Seq[ChiselAnnotation] => Seq[ChiselAnnotation]): ElaboratedDesign = {
    mapCircuit(c => c.copy(annotations = f(c.annotations)))
  }
}

/** Helpers for easy flow split between elaboration and firrtl compilation */
object ChiselGen {

  /** Usual chisel emission of the given module
   *  @param m
   *    chisel module (pass-by-name)
   *  @param args
   *    Array of string with arguments for elaboration.
   *  @param annotations
   *    Sequence of annotations to be added to unavoidable ones
   *  @return
   *    verilog as string (verilog is also written at target_dir)
   */
  def emit(
      m: => RawModule,
      args: Array[String] = Array.empty,
      annotations: AnnotationSeq = Seq.empty
  ): String = {
    // using standard chisel flow
    (new ChiselStage).emitVerilog(m, args, annotations)
  }

  /** Emission of the given module with reset converted to preset
   *  @param m
   *    chisel module (pass-by-name)
   *  @param args
   *    Array of string with arguments for elaboration.
   *  @param annotations
   *    Sequence of annotations to be added to unavoidable ones
   *  @return
   *    verilog as string (verilog is also written at target_dir)
   */
  def emitPreset(
      m: => RawModule,
      args: Array[String] = Array.empty,
      annotations: AnnotationSeq = Seq.empty
  ): String = {
    val ed = elaborate(m, args)
    emitPreset(ed, annotations)
  }

  /** Emission of the given module with reset converted to preset from an elaborated design
   *  @param ed
   *    an elaborated design (can be generated using elaborate)
   *  @param args
   *    Array of string with arguments for elaboration.
   *  @param annotations
   *    Sequence of annotations to be added to unavoidable ones
   *  @return
   *    verilog as string (verilog is also written at target_dir)
   */
  def emitPreset(
      ed: ElaboratedDesign,
      annotations: AnnotationSeq
  ): String = {
    val annos = Seq(ModulePresetChiselAnnotation(CircuitTarget(ed.name).module(ed.name)))
    emitVerilog(ed.mapChiselAnnos(s => annos ++ s), Array(), annotations)
  }

  /** Return an ElaboratedDesign for a Chisel module
   *  @param gen
   *    a call-by-name Chisel module
   */
  def elaborate(
      gen: => RawModule,
      args: Array[String] = Array.empty,
      annotations: AnnotationSeq = Seq.empty
  ): ElaboratedDesign = {

    val stage = new ChiselStage {
      override val targets =
        Seq(Dependency[chisel3.stage.phases.Checks], Dependency[chisel3.stage.phases.Elaborate])
    }

    val res = stage
      .execute(
          Array("--no-run-firrtl") ++ args,
          ChiselGeneratorAnnotation(() => gen) +: annotations
      )
      .flatMap(a =>
        a match {
          case NoRunFirrtlCompilerAnnotation => None // prevent leaking this annotation
          case _                             => Some(a)
        }
      )
    ElaboratedDesign(res)
  }

  /** Complete compilation of an elaborated circuit toward verilog
   *
   *  FEATURES:
   *    - not redoing elaboration & checks
   *    - not adding implicit output files path (as they won't be used anyway)
   *    - not emitting firrtl (chisel3.stage.phases.Emitter means write firrtl result to file)
   *    - not emitting annotation (no AddImplicitOutputAnnotationFile)
   *    - no longuer able to avoid verilog emission but emitted in chisel_gen/ and deleted afterwards
   *
   *  NOTE: if you need the result files (verilog, firrtl, anno.json), please use emitVerilog
   *
   *  @param ed
   *    an elaborated design (can be generated using elaborate)
   *  @param args
   *    Array of string with arguments for elaboration.
   *  @param annotations
   *    Sequence of annotations to be added to unavoidable ones
   *  @return
   *    verilog string
   */
  def getVerilog(
      ed: ElaboratedDesign,
      args: Array[String] = Array.empty,
      annotations: AnnotationSeq = Seq.empty
  ): String = {
    val tmpDir = "./chisel_gen"
    val clean = ed.raw.flatMap(a =>
      a match {
        case a: firrtl.options.TargetDirAnnotation       => Some(a.copy(directory = tmpDir))
        case _: chisel3.stage.ChiselOutputFileAnnotation => None
        case other                                       => Some(other)
      }
    )
    val stage = new ChiselStage {
      override val targets: Seq[Dependency[Phase]] =
        Seq(
            Dependency[chisel3.stage.phases.MaybeAspectPhase], // aspect phase (not used by us for now)
            Dependency[chisel3.stage.phases.Convert],          // Chisel ir to HighFir
            Dependency[chisel3.stage.phases.MaybeFirrtlStage]  // Actual Firrtl compilation
        )
    }

    val res = stage
      .execute(Array("-X", "verilog") ++ args, clean ++ annotations)
      .collectFirst {
        // not deleted because Emitter has not run
        case EmittedVerilogCircuitAnnotation(a) => a
      }
      .get
      .value

    new File(s"$tmpDir/${ed.name}.v").delete() // safe (won't throw Exception if not present)
    res
  }

  /** Finish compilation toward verilog FEATURES:
   *    - not redoing elaboration & checks
   *
   *  @param ed
   *    an elaborated design (can be generated using elaborate)
   *  @param args
   *    Array of string with arguments for elaboration.
   *  @param annotations
   *    Sequence of annotations to be added to unavoidable ones
   *  @return
   *    verilog string
   */
  def emitVerilog(
      ed: ElaboratedDesign,
      args: Array[String] = Array.empty,
      annotations: AnnotationSeq = Seq.empty
  ): String = {
    val stage = new ChiselStage {
      override val targets: Seq[Dependency[Phase]] =
        Seq(
            Dependency[chisel3.stage.phases.AddImplicitOutputFile],
            Dependency[chisel3.stage.phases.AddImplicitOutputAnnotationFile],
            Dependency[chisel3.stage.phases.MaybeAspectPhase], // aspect phase (not used by us for now)
            Dependency[chisel3.stage.phases.Emitter],          // emit fir result
            Dependency[chisel3.stage.phases.Convert],          // Chisel ir to HighFir
            Dependency[chisel3.stage.phases.MaybeFirrtlStage]  // Actual Firrtl compilation
        )
    }

    stage
      .execute(Array("-X", "verilog") ++ args, ed.raw ++ annotations)
      .collectFirst {
        case EmittedVerilogCircuitAnnotation(a) => a
      }
      .get
      .value
  }
}
