package sv2chisel.helpers.tools

import chisel3._
import chisel3.ActualDirection
import chisel3.experimental.{DataMirror, Param, IntParam, StringParam, RawParam}

import firrtl.annotations.{CircuitTarget}

import collection.mutable.{LinkedHashMap, ArrayBuffer}
import collection.immutable.{ListMap}

// Develepment note:
// potential evolution : take a verilog module as argument, parse it for ports & params, and provide a mock to it
// => would allow to provide automated casts & conversions ?

/** Type Alias-like for Seq[(String, Param)] trait to be mixed in any object intended to provide parameterization to the
 *  wrapper generator
 */
trait ParamSetLike {
  val params: Seq[(String, Param)]
}

case class WrapperException(msg: String = "", stack: Throwable = None.orNull) extends Exception(msg, stack)

/** Most basic implementation of ParamSetLike trait
 *  @param params
 *    : Sequence of (param name ; param value)
 */
case class ParamSet(params: Seq[(String, Param)]) extends ParamSetLike

/** Generator Internal Representation of IOs Ports
 *  @groupdesc InternalWrapperIR
 *    Internal IR class for the ParamWrapperGenerator
 *  @param name
 *    : name of this port
 *  @param dir
 *    : direction of this port (input|output|inout)
 *  @param tpe
 *    : chisel type of this port (Data)
 *  @param flatPorts
 *    : flattened ports associated to this port (ex Vec in => in_1, in_2, ...)
 */
private final case class VerilogPort(
    val name: String,
    val dir: String,
    val tpe: Data,
    val flatPorts: Seq[VerilogPort] = Seq()
) {

  private def updateNameRec(newName: String): VerilogPort = {
    this.copy(name = newName, flatPorts = flatPorts.map(p => p.updateNameRec(newName + p.name.drop(name.length))))
  }

  /** recursively verilog equivalent type from given Chisel type */
  private def getVerilogTypeRec(tpe: Data): String = {
    tpe match {
      case _: Clock                     => s""
      case _: Reset                     => s""
      case u: UInt if (u.getWidth == 1) => s""
      case u: UInt                      => s" [${u.getWidth - 1}:0]"

      case v: Vec[_] => s" [${v.length - 1}:0]${v.headOption.map(getVerilogTypeRec(_)).getOrElse("")}"

      case b: Record => s" ${b.className.split('$').head}"

      case _ => throw WrapperException(s"Unsupported type for verilog port inference: $tpe -- ${tpe.getClass.getName}")
    }
  }

  /** emit the port for wrapper IOs */
  def emit(): String = {
    s"$dir${getVerilogTypeRec(tpe)} $name"
  }

  private def unflat(flatPrefix: String, prefix: String, ports: Seq[VerilogPort]): Seq[VerilogPort] = {
    val pattern = s"^${flatPrefix}(.*)".r
    ports.collect {
      case p if (p.name.matches(pattern.regex)) =>
        p.copy(name = s"${prefix}${pattern.findFirstMatchIn(p.name).get.group(1)}")
    }
  }

  private def collect(name: String, ports: Seq[VerilogPort]): Option[VerilogPort] = {
    ports.collect { case p if (p.name == name) => p } match {
      case Seq(p) => Some(p)
      case _ =>
        println(s"Unexpectedly found multiple ports with name $name ($ports)")
        None
    }
  }

  /** emit the port for IOs map after instance declaration */
  def emitInstance(ref: VerilogPort): (Seq[String], Seq[String]) = {
    // NB: ref is the wrapper
    lazy val stdMap = s".$name(${ref.name})"
    (tpe, ref.tpe) match {
      case (_: Clock, _) => (Seq(stdMap), Seq())
      case (_: Reset, _) => (Seq(stdMap), Seq())

      case (u: UInt, v: UInt) if (u.getWidth < v.getWidth) =>
        val portMap = s".$name(${ref.name}[${u.getWidth - 1}:0])"
        if (dir == "input") {
          println(s"[NOTE] Using only ${u.getWidth} bits out of ${v.getWidth} for port $name")
          (Seq(portMap), Seq())
        } else {
          println(s"[NOTE] Padding unused bits of port ${ref.name} (${v.getWidth - 1} downto ${u.getWidth}) with '0")
          (Seq(portMap), Seq(s"assign ${ref.name}[${v.getWidth - 1}:${u.getWidth}] = '0;"))
        }

      case (_: UInt, _) => (Seq(stdMap), Seq())

      case (u: Vec[_], v: Vec[_]) => // here comes the fun
        // let's discriminate on length
        // NB: Asumming that no top top level ports get removed at any time (no sparse naming)
        if (u.length > v.length) throw WrapperException(s"Badly formed vec port ${this.emit} for remote ${ref.emit}")
        val (portMap, assignMap) = flatPorts
          .zip(ref.flatPorts)
          .zipWithIndex
          .map {
            case ((l, r), i) => l.emitInstance(r.copy(name = s"${ref.name}[$i]"))
          }
          .reduceOption((t, r) => (t._1 ++ r._1, t._2 ++ r._2))
          .getOrElse((Seq(), Seq()))

        val assignIndexes = ref.flatPorts.zipWithIndex.drop(flatPorts.length).flatMap {
          case (p, i) => {
            if (p.dir != "input") {
              println(s"[NOTE] Assigning port ${ref.name} to 0")
              Some(s"assign ${ref.name}[$i] = '0;")
            } else {
              None
            }
          }
        }
        (portMap, assignMap ++ assignIndexes)

      case (l: Record, r: Record) =>
        val fpMapL = this.flatPorts.map(p => p.name -> p).toMap
        val fpMapR = ref.flatPorts.map(p => p.name -> p).toMap
        r.elements
          .map {
            case (field, dr) => {
              (fpMapL.get(s"${name}_${field}"), l.elements.get(field), fpMapR.get(s"${name}_${field}")) match {
                case (Some(pl), Some(_), Some(pr)) =>
                  pl.emitInstance(pr.copy(name = s"${ref.name}.${field}"))

                case (None, None, Some(pr)) if (pr.dir == "input") => (Seq(), Seq())
                case (None, None, Some(_)) =>
                  println(s"[NOTE] assigning field ${ref.name}.${field} to '0")
                  (Seq(), Seq(s"assign ${ref.name}.${field} = '0;"))

                case (None, Some(dl), None) => // field is a bundle or vec which also gets flattened
                  (dl, dr) match {
                    case ((_: Vec[_], _: Vec[_]) | (_: Record, _: Record)) =>
                      val prefix = s"${ref.name}.${field}"
                      val lfp    = unflat(s"${name}_${field}", prefix, flatPorts)
                      val rfp    = unflat(s"${name}_${field}", prefix, ref.flatPorts)

                      this
                        .copy(name = prefix, flatPorts = lfp, tpe = dl)
                        .emitInstance(ref.copy(name = prefix, flatPorts = rfp, tpe = dr))

                    case _ => throw WrapperException(s"Malformed ports: $this->$dl $ref->$dr")
                  }

                case _ =>
                  throw WrapperException(
                      s"Unsupported mismatching bundle keys: ${l.elements.keySet} vs ${r.elements.keySet}"
                  )
              }

            }
          }
          .reduceOption((t, r) => (t._1 ++ r._1, t._2 ++ r._2))
          .getOrElse((Seq(), Seq())) // empty record case

      case _ => throw WrapperException(s"Mismatching type for verilog port emission: $tpe -- ${ref.tpe}")
    }

  }

  private def backPropagate(tpe: Data, port: VerilogPort): VerilogPort = {
    tpe match {
      case v: Vec[_] =>
        val updated = port.flatPorts.map(p => backPropagate(v.head, p))
        val fp = if (v.length > updated.length) {
          // adding proper vec elements to reach the expected length
          val base = updated.head
          port.flatPorts ++ (updated.length until v.length).map(i => base.updateNameRec(s"${port.name}_$i"))
        } else {
          port.flatPorts
        }
        port.copy(tpe = tpe, flatPorts = fp)
      case r: Record =>
        val tpeMap = r.elements.map { case (field, d) => s"${port.name}_$field" -> d }
        port.copy(tpe = tpe, flatPorts = port.flatPorts.map(p => backPropagate(tpeMap(p.name), p)))
      case _ => port.copy(tpe = tpe)
    }
  }

  /** Perform recursive type update with associated update of flatPorts */
  private def updatePortRec(base: VerilogPort, remote: VerilogPort): VerilogPort = {
    if (base.name != remote.name || base.dir != remote.dir) {
      throw WrapperException(s"Unexpected update of port: ${base.emit} with ${remote.emit}")
    }
    // base = wrapper ; remote = instance
    (base.tpe, remote.tpe) match {
      case (_: Clock, _) => base
      case (_: Reset, _) => base

      case (u: UInt, v: UInt) if (u.getWidth < v.getWidth) =>
        println(s"[NOTE] Increasing existing width for port ${base.name} from ${u.getWidth} to ${v.getWidth}")
        base.copy(tpe = v)

      case (_: UInt, _) => base

      case (u: Vec[_], v: Vec[_]) =>
        // 2 things to do
        // - check length
        // - check underlying tpe (homogeneous within the vec)
        if (DataMirror.checkTypeEquivalence(u, v)) {
          base
        } else {
          // all elements of a vec DO share the exact same type
          val inner = updatePortRec(base.flatPorts.head, remote.flatPorts.head)
          val fports = (if (u.length < v.length) remote.flatPorts else base.flatPorts)
            .map(p => backPropagate(inner.tpe, p))

          val tpe = Vec(Seq(u.length, v.length).max, inner.tpe.cloneType)
          println(s"[NOTE] Updating port ${base.name} with inner tpe: ${inner.tpe.getClass.getName}")

          base.copy(tpe = tpe, flatPorts = fports)

        }

      case (l: Record, r: Record) =>
        if (l.className != r.className) throw WrapperException(s"Mismatched bundle types: $l $r")
        val rMap = r.elements.map { case (field, d) => field -> d }.toMap
        val (requireNew, fields, fp) = l.elements
          .map {
            case (field, dl) => {
              val ref = s"${base.name}_$field"
              (rMap.get(field), collect(ref, remote.flatPorts), collect(ref, base.flatPorts)) match {
                // first case : type equivalence (recursively) => no need for tpe nor flatPorts update
                case (Some(dr), Some(p), _) if (DataMirror.checkTypeEquivalence(dl, dr)) =>
                  (false, Seq(field -> dr), Seq(p))
                // broken equivalence => need to update recursively
                case (Some(_), Some(pr), Some(pl)) =>
                  println(s"[NOTE] Upgrading existing field $field for port ${base.name}")
                  val updatedPort = updatePortRec(pl, pr)
                  (true, Seq(field -> updatedPort.tpe), Seq(updatedPort))
                // new field
                case (None, _, Some(pl)) =>
                  println(s"[NOTE] Adding new field $field for port ${base.name}")
                  (true, Seq(field -> dl), Seq(pl))

                case _ => throw WrapperException(s"Badly formed record fields")
              }
            }
          }
          .reduceOption((f1, f2) => ((f1._1 || f2._1), (f1._2 ++ f2._2), (f1._3 ++ f2._3)))
          .getOrElse((false, Seq(), Seq()))

        if (requireNew) {
          val tpe = new Record {
            def cloneType          = this // no parameters, no bound anyway
            val elements           = ListMap(fields: _*)
            override def className = l.className
          }
          base.copy(tpe = tpe, flatPorts = fp)
        } else {
          base
        }

      case _ => throw WrapperException(s"Unsupported type for verilog port inference: $tpe")
    }
  }

  /** Update the current port to cover remote port (length & width) */
  def update(that: VerilogPort): VerilogPort = {
    // NB: in the general context, `this` is remote (wrapper) and `that` is local (instances)
    updatePortRec(this, that)
  }
}

/** Generator Internal Representation of verilog modules instances
 *  @param name
 *    : name of this instance (used for wrapper emission)
 *  @param module
 *    : name of the target module
 *  @param ports
 *    : ports of this instance
 *  @param params
 *    : conditional set of parameters that would generate this instance
 *  @group InternalWrapperIR
 */
private final case class VerilogInstance(
    name: String,
    module: String,
    ports: Seq[VerilogPort],
    params: ParamSetLike
) {

  /** emit condition to be matched against parameters provided to the wrapper */
  def cond: String = {
    params.params
      .map(p => {
        p._2 match {
          case StringParam(v) => s"${p._1} == " + '"' + v + '"'
          case IntParam(v)    => s"${p._1} == $v"
          case RawParam(v)    => s"${p._1} == $v"
          case _              => s"${p._1} == ${p._2}"
        }
      })
      .mkString("", " && ", "")
  }

  /** Emit this instance as verilog string
   *  @param ind
   *    : indent level to be applied to the whole emitted block
   */
  def emit(ind: Int, wrapperPorts: Seq[VerilogPort]): String = {
    println(s" > Instanciating $name")
    ArrayBuffer[String]()
    val s = ArrayBuffer[String]()
    s += s"if($cond) begin\n"
    s += s"  $name $module"
    val portMap = ports
      .zip(wrapperPorts)
      .map(t => t._1.emitInstance(t._2))
      .reduceOption((t, r) => (t._1 ++ r._1, t._2 ++ r._2))
      .getOrElse((Seq(), Seq()))
    s += portMap._1.mkString("(\n    ", ",\n    ", "\n  );")
    s += portMap._2.mkString("\n  ", "\n  ", "")
    s += "\nend"
    s.mkString.split("\n").mkString("", "\n" + " " * ind, "")
  }
}

/** Generator Internal Representation of verilog parameters of the current wrapper
 *  @group InternalWrapperIR
 */
private final class VerilogParams() {
  private val params = LinkedHashMap[String, (Boolean, Param)]()

  /** Add provided params to the current set of parameters
   *  @param s
   *    : Set of parameter to be integrated
   */
  def addParams(s: ParamSetLike): Unit = {
    for ((n, v) <- s.params) {
      if (!params.contains(n)) {
        params(n) = (true, v)
      } else {
        // ensure switched params don't get default
        params(n) = (false, params(n)._2)
      }
    }
  }

  /** Emit the current set of parameters as verilog string for wrapper declaration */
  def emit(): String = {
    params
      .map(p => {
        val decl = s"parameter ${p._1}"
        p._2 match {
          case (true, StringParam(i)) => s"$decl = " + '"' + i + '"'
          case (true, IntParam(i))    => s"$decl = $i"
          case (true, RawParam(i))    => s"$decl = $i"
          case _                      => decl
        }
      })
      .mkString(" #(\n    ", ",\n    ", s"\n  )\n")
  }

  /** Emit the current set of parameters as verilog string for debug info intended to provide actual parameters passed
   *  to the wrapper in case of error
   */
  def info(): String = {
    val s = params.map(p => {
      val str = p._2._2 match {
        case StringParam(_) => s"${p._1} = " + '"' + "%s" + '"'
        case IntParam(_)    => s"${p._1} = %d"
        case RawParam(_)    => s"${p._1} = %b"
        case _              => s"${p._1} = %b"
      }
      (str, p._1)
    })
    val start =
      s.map(_._1).mkString("$info(\"The following values were provided:\\n >", "\\n >", "\", ")
    start + s.map(_._2).mkString("", ", ", ");\n")
  }
}

/** Helper function for ParamWrapperGenerator & VerilogPortWrapper */
private object getPorts {

  /** Retrieve port direction thanks to chisel experimental reflection API
   *  @param d
   *    Any bound instance of Data (chisel HW ground Type)
   */
  private def getPortDir(d: Data): String = {
    DataMirror.directionOf(d) match {
      case ActualDirection.Input            => "input"
      case ActualDirection.Output           => "output"
      case ActualDirection.Bidirectional(_) => "inout"
      case _                                => throw WrapperException("Unsupported direction")
    }
  }

  /** Retrieve port list from the Circuit (Elaborated RawModule)
   *
   *  The circuit must be elaborated to retrieve ports
   *  @param c
   *    Elaborated RawModule
   */
  def apply(c: RawModule, forcePreset: Boolean, unflatPorts: Boolean = true): (Seq[VerilogPort], Boolean) = {
    val ports     = DataMirror.modulePorts(c).filter { case (n, _) => (n != "reset" || !forcePreset) }
    val flatPorts = DataMirror.fullModulePorts(c)

    def getPortsRec(prefix: String, tpe: Data): Seq[VerilogPort] = {
      tpe match {
        case _: Vec[_] =>
          flatPorts.collect {
            case (n, d) if (n.matches(s"^(${prefix}_[0-9]+)")) =>
              VerilogPort(n, getPortDir(d), d, getPortsRec(n, d))
          }
        case b: Record =>
          b.elements.flatMap {
            case (field, data) => {
              flatPorts.collect {
                case (n, d) if (n.matches(s"^(${prefix}_${field})")) =>
                  VerilogPort(n, getPortDir(d), d, getPortsRec(n, data))
              }
            }
          }.toSeq
        case _ => Seq()
      }
    }

    val portSeq = ports.map { case (name, data) => VerilogPort(name, getPortDir(data), data, getPortsRec(name, data)) }
    val flatPortsSeq = if (unflatPorts) {
      portSeq
    } else {
      def flattenPorts(p: VerilogPort): Seq[VerilogPort] = {
        if (p.flatPorts.isEmpty) {
          p.tpe match {
            case _: Vec[_] => Seq() // filter empty vec
            case _: Record => Seq() // filter empty bundle
            case _         => Seq(p)
          }
        } else {
          p.flatPorts.flatMap(flattenPorts)
        }
      }
      portSeq.flatMap(flattenPorts)
    }
    (flatPortsSeq, flatPorts.collectFirst { case (n, d) if (n == "reset") => d }.isDefined)
  }

}

/** Transforms a set of parameterized chisel instances into a nice pseudo-parameterized verilog wrapper.
 *
 *  Useful for integration of chisel-time parameterized module into existing SV codebase.
 *
 *  Single method object: use ParamWrapperGenerator.emit(instances)
 */
object ParamWrapperGenerator {

  /** Main emit function
   *  @param instances
   *    map of <wrapper parameterization> -> <actual chisel-parameterized module generator>
   *  @param forceName
   *    force a name for the generated module
   *  @param unflatPorts
   *    provide a wrapper with verilog unflat ports (multi-dimension packed, struct), mapped to chisel flattened ports
   *  @param forcePreset
   *    use if preset are being used (helps properly manage the reset signal)
   *  @param unusedParams
   *    set of parameters to be emitted as wrapper parameters (bad practice for verilog compatibility only)
   *  @return
   *    a string containing the wrapper
   */
  def emit[T <: ParamSetLike](
      instances: Map[T, () => RawModule],
      forceName: Option[String] = None,
      unflatPorts: Boolean = false,
      forcePreset: Boolean = false,
      unusedParams: ParamSetLike = ParamSet(Seq()),
      args: Array[String] = Array.empty
  ): String = {
    var topNameStore = ""
    lazy val emittedName = forceName match {
      case Some(s) => s
      case None    => topNameStore
    }
    var instanceCount = -1
    def getNames(m: RawModule): (String, ModuleRenameChiselAnnotation) = {
      if (instanceCount == -1) {
        topNameStore = m.desiredName
      } else if (topNameStore != m.desiredName) {
        throw WrapperException("Instances should be all of the same Module")
      }
      instanceCount += 1
      (topNameStore, ModuleRenameChiselAnnotation(emittedName, s"$instanceCount", forceName.isDefined))
    }

    val verilogParams = new VerilogParams()

    val verilogInstances = ArrayBuffer[VerilogInstance]()
    val verilogModules   = ArrayBuffer[String]()
    var autoIOs          = Seq[VerilogPort]()

    for ((p, m) <- instances) {
      verilogParams.addParams(p)

      val ed                 = ChiselGen.elaborate(m.apply, args)
      val (ports, hasReset)  = getPorts(ed.designOption.get, forcePreset, unflatPorts)
      val (topName, renames) = getNames(ed.designOption.get)
      val instName           = renames.newTopName(topName)

      verilogInstances += VerilogInstance(instName, topName, ports, p)
      if (autoIOs.isEmpty) {
        autoIOs = ports
      } else if (autoIOs.size != ports.size) {
        throw WrapperException("Not the same number of ports in current instance")
      } else {
        autoIOs = autoIOs.zip(ports).map(t => t._1.update(t._2))
      }

      verilogParams.addParams(unusedParams)

      println(s">>>> EMITTING INSTANCE $instName <<<<<")

      val annos = (forcePreset, hasReset) match {
        case (true, true) => Seq(ModulePresetChiselAnnotation(CircuitTarget(topName).module(topName)), renames)
        case (true, _) =>
          throw WrapperException("Requesting transformation of reset into preset but no reset port was found")
        case _ => Seq(renames)
      }
      verilogModules += ChiselGen.getVerilog(ed.mapChiselAnnos(s => annos ++ s))
    }

    println(s">>>> EMITTING WRAPPER $emittedName <<<<<")

    val wrapper = ArrayBuffer[String]()
    wrapper += s"module $emittedName"
    wrapper += verilogParams.emit()
    wrapper += autoIOs.map(_.emit).mkString("  (\n    ", ",\n    ", "\n  );\n")
    wrapper += "  initial begin\n"
    wrapper += "    if (!( "
    wrapper += verilogInstances.map(_.cond).mkString("(", ") || \n        (", ")")
    wrapper += " )) begin\n"
    wrapper += "      " + verilogParams.info()
    wrapper += "      $error(\"CRITICAL FAILURE: Unmapped parameter set\");\n"
    wrapper += "      $fatal(1, \"CRITICAL FAILURE: Unmapped parameter set\");\n"
    wrapper += "    end\n"
    wrapper += "  end\n"
    wrapper += "  generate\n"
    wrapper += verilogInstances
      .map(_.emit(4, autoIOs))
      .mkString("    ", " else ", " else begin: unmapped_param_set\n")
    wrapper += "      initial begin\n"
    wrapper += "        " + verilogParams.info()
    wrapper += "        $info(\"CRITICAL FAILURE: Unmapped parameter set\");\n"
    wrapper += "      end\n"
    wrapper += "    end\n"
    wrapper += "  endgenerate\n"
    wrapper += "endmodule\n"

    verilogModules += wrapper.mkString
    verilogModules.mkString("", "\n", "\n")
  }
}

/** Emit provided chisel module & generate a top level wrapper with Verilog-style input (not flattened)
 *
 *  Useful for integration of chisel-time parameterized module into existing SV codebase.
 */
object VerilogPortWrapper {

  /** Main generation function (returns a string but do not write the wrapper to a file)
   *  @param module
   *    function generating a RawModule
   *  @param wrapperName
   *    name of the generated wrapper (must be different from module name)
   *  @param emitModule
   *    Compile and emit module to verilog file (optional, default false)
   *  @param forcePreset
   *    top level synchronous reset (required) is converted to Preset (optional, default false)
   *  @param args
   *    additional args for chisel generation (optional, default empty array)
   *  @return
   *    a string containing the wrapper
   */
  def generate(
      module: () => RawModule,
      wrapperName: String,
      emitModule: Boolean = false,
      forcePreset: Boolean = false,
      args: Array[String] = Array.empty
  ): String = {

    val ed                = ChiselGen.elaborate(module.apply, args)
    val (ports, hasReset) = getPorts(ed.designOption.get, forcePreset)

    val instName = ed.designOption.get.desiredName
    if (instName == wrapperName) throw WrapperException(s"Cannot use the same name for instance and wrapper $instName")

    if (emitModule) {
      println(s">>>> EMITTING UNDERLYING INSTANCE $instName <<<<<")
      val updatedED = (forcePreset, hasReset) match {
        case (true, true) =>
          ed.mapChiselAnnos(s => s :+ ModulePresetChiselAnnotation(CircuitTarget(instName).module(instName)))
        case (true, _) =>
          throw WrapperException("Requesting transformation of reset into preset but no reset port was found")
        case _ => ed
      }
      ChiselGen.emitVerilog(updatedED) // main verilog file emitted as a separate file
    }

    println(s">>>> EMITTING WRAPPER $wrapperName <<<<<")

    val wrapper = ArrayBuffer[String]()
    wrapper += s"module $wrapperName"
    wrapper += ports.map(_.emit).mkString(" (\n    ", ",\n    ", "\n  );\n")

    val portMap =
      ports.map(p => p.emitInstance(p)).reduceOption((t, r) => (t._1 ++ r._1, t._2 ++ r._2)).getOrElse((Seq(), Seq()))
    wrapper += portMap._1.mkString(s"  $instName inst (\n    ", ",\n    ", "\n  );\n")

    wrapper += "endmodule\n"

    wrapper.mkString
  }

  /** Generate the wrapper & write it to a file (<wrapperName>.sv)
   *  @param module
   *    function generating a RawModule
   *  @param wrapperName
   *    name of the generated wrapper (must be different from module name)
   *  @param emitModule
   *    Compile and emit module to verilog file (optional, default false)
   *  @param forcePreset
   *    top level synchronous reset (required) is converted to Preset
   *  @param args
   *    additional args for chisel generation
   *  @return
   *    a string containing the wrapper
   */
  def emit(
      module: () => RawModule,
      wrapperName: String,
      emitModule: Boolean = false,
      forcePreset: Boolean = false,
      args: Array[String] = Array.empty
  ): String = {

    val emissionPath = args.indexOf("--target-dir") match {
      case -1 => s"${wrapperName}.v"
      case i =>
        args.lift(i + 1) match {
          case Some(p) => s"$p/${wrapperName}.v"
          case _       => s"${wrapperName}.v"
        }
    }

    import java.io.{File, FileWriter}

    emissionPath.split("/").dropRight(1) match {
      case Array() =>
      case a =>
        val dirs = new File(a.mkString("", "/", ""))
        dirs.mkdirs()
    }
    val wrapper = generate(module, wrapperName, emitModule, forcePreset, args)
    val writer  = new FileWriter(emissionPath)
    writer.write(wrapper)
    writer.close()

    wrapper
  }
}
