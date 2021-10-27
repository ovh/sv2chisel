// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import sv2chisel.ir._
import sv2chisel.ir.PrimOps._

import logger.EasyLogging

// implicits
import sv2chisel.ir.evalExpression._

import collection.mutable.{ArrayBuffer}
// define all implict chiselize function for all nodes of the IR

package object chiselize extends EasyLogging {
  def unsupportedChisel(ctx: ChiselEmissionContext, d: SVNode, str: String=""): Seq[ChiselTxt] = {
    fatal(s"At ${ctx.getInfo(d)}: Unsupported conversion to chisel ($str) for node ${d.serialize}. Resulting chisel code will likely not compile.")
    Seq(ChiselTxt(UndefinedInterval, 0, false, "???"))
  }
  
  // weird : the overloaded functions (from EasyLogging) are not visible later 
  // I definitely suspect a scala bug here ...
  // prefixing with r for raise
  
  def rfatal(ctx: ChiselEmissionContext, d: SVNode, msg: => String): Unit = {
    fatal(s"At ${ctx.getInfo(d)}: $msg")
  }
  def rcritical(ctx: ChiselEmissionContext, d: SVNode, msg: => String): Unit = {
    critical(s"At ${ctx.getInfo(d)}: $msg")
  }
  def rwarn(ctx: ChiselEmissionContext, d: SVNode, msg: => String): Unit = {
    warn(s"At ${ctx.getInfo(d)}: $msg")
  }
  def rinfo(ctx: ChiselEmissionContext, d: SVNode, msg: => String): Unit = {
    info(s"At ${ctx.getInfo(d)}: $msg")
  }
  def rdebug(ctx: ChiselEmissionContext, d: SVNode, msg: => String): Unit = {
    debug(s"At ${ctx.getInfo(d)}: $msg")
  }
  def rtrace(ctx: ChiselEmissionContext, d: SVNode, msg: => String): Unit = {
    trace(s"At ${ctx.getInfo(d)}: $msg")
  }

  implicit def stringToSnake(s: String) = new SnakeString(s)
  
  implicit def sourceFileToChisel(s: SourceFile) = new ChiselSourceFile(s)

  implicit def descriptionToChisel(s: Description) = new ChiselDescription(s)
  implicit def headerToChisel(s: Header) = new ChiselHeader(s)

  implicit def packageRefToChisel(s: PackageRef) = new ChiselPackageRef(s)

  implicit def importPackagesToChisel(s: ImportPackages) = new ChiselImportPackages(s)

  implicit def defModuleToChisel(s: DefModule) = new ChiselDefModule(s)
  implicit def defPackageToChisel(s: DefPackage) = new ChiselDefPackage(s)

  implicit def moduleToChisel(s: Module) = new ChiselModule(s)
  
  implicit def defParamToChisel(s: DefParam) = new ChiselDefParam(s)
  
  implicit def statementToChisel(s: Statement) = new ChiselStatement(s)
  implicit def defPortToChisel(s: Port) = new ChiselPort(s)
  implicit def defFunctionToChisel(s: DefFunction) = new ChiselDefFunction(s)
  implicit def simpleBlockToChisel(s: SimpleBlock) = new ChiselSimpleBlock(s)
  implicit def defLogicToChisel(s: DefLogic) = new ChiselDefLogic(s)
  implicit def defTypeToChisel(s: DefType) = new ChiselDefType(s)
  implicit def switchToChisel(s: Switch) = new ChiselSwitch(s)
  
  implicit def expressionToChisel(s: Expression) = new ChiselExpression(s)
  implicit def rawScalaExprWrapperToChisel(s:RawScalaExprWrapper) = new ChiselRawScalaExprWrapper(s)
  implicit def rawScalaExpressionToChisel(s:RawScalaExpression) = new ChiselRawScalaExpression(s)
  implicit def undefinedExpressionToChisel(s:UndefinedExpression) = new ChiselUndefinedExpression(s)
  implicit def stringLitToChisel(s:StringLit) = new ChiselStringLit(s)
  implicit def referenceToChisel(s:Reference) = new ChiselReference(s)
  implicit def autoAssignToChisel(s:AutoAssign) = new ChiselAutoAssign(s)
  implicit def namedAssignToChisel(s:NamedAssign) = new ChiselNamedAssign(s)
  implicit def noNameAssignToChisel(s:NoNameAssign) = new ChiselNoNameAssign(s)
  implicit def subFieldToChisel(s:SubField) = new ChiselSubField(s)
  implicit def subIndexToChisel(s:SubIndex) = new ChiselSubIndex(s)
  implicit def SubRangeToChisel(s:SubRange) = new ChiselSubRange(s)
  implicit def numberToChisel(s:Number) = new ChiselNumber(s)
  implicit def fillingBitPatternToChisel(s:FillingBitPattern) = new ChiselFillingBitPattern(s)
  implicit def assignPatternToChisel(s:AssignPattern) = new ChiselAssignPattern(s)
  implicit def defaultAssignPatternToChisel(s:DefaultAssignPattern) = new ChiselDefaultAssignPattern(s)
  implicit def mappedValuesToChisel(s:MappedValues) = new ChiselMappedValues(s)
  implicit def seqValuesToChisel(s:SeqValues) = new ChiselSeqValues(s)
  implicit def replicatePatternToChisel(s:ReplicatePattern) = new ChiselReplicatePattern(s)
  implicit def boolLiteralToChisel(s:BoolLiteral) = new ChiselBoolLiteral(s)
  implicit def uintLiteralToChisel(s:UIntLiteral) = new ChiselUIntLiteral(s)
  implicit def sintLiteralToChisel(s:SIntLiteral) = new ChiselSIntLiteral(s)
  implicit def fixedLiteralToChisel(s:FixedLiteral) = new ChiselFixedLiteral(s)
  implicit def doPrimToChisel(s:DoPrim) = new ChiselDoPrim(s)
  implicit def doCastToChisel(s:DoCast) = new ChiselDoCast(s)
  implicit def doCallToChisel(s:DoCall) = new ChiselDoCall(s)
  implicit def concatToChisel(s:Concat) = new ChiselConcat(s)
  implicit def typeInstToChisel(s: TypeInst) = new ChiselTypeInst(s)
  
  implicit def commentToChisel(s: Comment) = new ChiselComment(s)
  implicit def connectToChisel(s: Connect) = new ChiselConnect(s)
  implicit def ifGenToChisel(s: IfGen) = new ChiselIfGen(s)
  implicit def forGenToChisel(s: ForGen) = new ChiselForGen(s)
  implicit def conditionallyToChisel(s: Conditionally) = new ChiselConditionally(s)
  implicit def defInstanceToChisel(s: DefInstance) = new ChiselDefInstance(s)
  
  implicit def typeToChisel(s: Type) = new ChiselType(s)
  implicit def widthToChisel(s: Width) = new ChiselWidth(s)
  implicit def fieldToChisel(s: Field) = new ChiselField(s)
  implicit def enumFieldToChisel(s: EnumField) = new ChiselEnumField(s)
  
  def getRef(e: Expression): Reference = {
    e match {
      case r: Reference => r
      case s: SubField => 
        // TODO : better ref system ???
        // might not be an issue as full info will be retrieved ??
        getRef(s.expr) 
      case s: SubIndex => getRef(s.expr)
      case s: SubRange => getRef(s.expr)
      case _ => Utils.throwInternalError(s"Unable to fetch proper reference for ${e.serialize} ($e)")
    }
  }
  
}

case class SnakeString(s: String) {
  /* 
   * Takes an underscore separated identifier name and returns a camel cased one
   *
   * Example:
   *    toCamel("this_is_a_1_test") == "thisIsA1Test"
   */
  def toCamel(ctx: ChiselEmissionContext) = if (ctx.toCamelCase) {
    "_([a-z\\d])".r.replaceAllIn(s, {m =>
      m.group(1).toUpperCase()
    }).capitalize
  } else { s }
}

case class ChiselTxt(
    tokens: Interval,
    indentLevel: Int,
    newLine: Boolean,
    txt: String
  ) {
  def incr(n: Int = 1): ChiselTxt = {
    this.copy(indentLevel = indentLevel + n)
  }
  def decr(n: Int = 1): ChiselTxt = {
    this.copy(indentLevel = indentLevel - n)
  }
  override def toString: String = {
    "  "*indentLevel + txt.replace("\n",  "\n" + "  "*indentLevel)
  }
}
object ChiselTxt {
  def apply(n: SVNode, ctx: ChiselEmissionContext, txt: String): ChiselTxt = {
    new ChiselTxt(n.tokens, ctx.indentLevel, false, txt)
  }
  def apply(ctx: ChiselEmissionContext, txt: String): ChiselTxt = {
    new ChiselTxt(UndefinedInterval, ctx.indentLevel, false, txt)
  }
  def apply(txt: String): ChiselTxt = {
    new ChiselTxt(UndefinedInterval, 0, false, txt)
  }
  def apply(tokens: Interval, txt: String): ChiselTxt = {
    new ChiselTxt(tokens, 0, false, txt)
  }
}
object ChiselTxtS {
  def apply(n: SVNode, ctx: ChiselEmissionContext, txt: String): Seq[ChiselTxt] = {
    Seq(new ChiselTxt(n.tokens, ctx.indentLevel, false, txt))
  }
  def apply(ctx: ChiselEmissionContext, txt: String): Seq[ChiselTxt] = {
    Seq(new ChiselTxt(UndefinedInterval, ctx.indentLevel, false, txt))
  }
  def apply(txt: String): Seq[ChiselTxt] = {
    Seq(new ChiselTxt(UndefinedInterval, 0, false, txt))
  }
  def apply(tokens: Interval, txt: String): Seq[ChiselTxt] = {
    Seq(new ChiselTxt(tokens, 0, false, txt))
  }
}

object ChiselLine {
  def apply(n: SVNode, ctx: ChiselEmissionContext, txt: String): ChiselTxt = {
    new ChiselTxt(n.tokens, ctx.indentLevel, true, txt)
  }
  def apply(ctx: ChiselEmissionContext, txt: String): ChiselTxt = {
    new ChiselTxt(UndefinedInterval, ctx.indentLevel, true, txt)
  }
  def apply(txt: String): ChiselTxt = {
    new ChiselTxt(UndefinedInterval, 0, true, txt)
  }
}

trait Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt]
}


import chiselize._

class ChiselSourceFile(val src: SourceFile) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    // descriptions must be chiselized prior to getDep
    val desc = src.descriptions.map(_.chiselize(ctx)).flatten
    val dep = src.getDep.map(p => ImportPackages(UndefinedInterval, Seq(p)).chiselize(ctx)).flatten
    val rootP = ChiselTxtS(ctx, "package " + ctx.srcBasePath.split("/").last)
    val localP = src.path.split("/").dropRight(1).mkString(".") match {
      case "" => Seq()
      case s => Seq(ChiselLine(ctx, "package " + s))
    }
      
    rootP ++ localP ++ Seq(ChiselLine(ctx, "\nimport chisel3._")) ++ dep ++
      Seq(ChiselLine(ctx, "")) ++ desc // clear 1 line before descriptions
  }
}

class ChiselDescription(val d: Description) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    d match {
      case s: IsolatedStatement => s.body.chiselize(ctx)
      case m: DefModule => m.chiselize(ctx)
      case p: DefPackage => p.chiselize(ctx)
      case _ => unsupportedChisel(ctx,d)
    }
  }
}

class ChiselPackageRef(val p: PackageRef) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val last = if(p.path == "") "" else "."
    val str = s"import ${p.path.replace("::",".")}$last${p.item}"
    Seq(ChiselLine(p, ctx, str))
  }
}

class ChiselImportPackages(val p: ImportPackages) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    p.packages.map(_.chiselize(ctx)).flatten
  }
}


class ChiselDefPackage(val p: DefPackage) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val s = ArrayBuffer[ChiselTxt]()
    s += ChiselLine(p, ctx, s"package object ${p.name} {")
    s ++= p.body.chiselize(ctx.incr())
    s += ChiselLine(ctx, "}")
  }
}

class ChiselDefModule(val m: DefModule) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    m match {
      case mod: Module => mod.chiselize(ctx)
      case _ => unsupportedChisel(ctx,m) 
    }
  }
}

class ChiselModule(val m: Module) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val mCtxt = ctx.incr()
    val pCtxt = mCtxt.incr()
    
    // filter out clock & reset ports provided by MultiIOModule if inference succeeded earlier
    val (mod, kind) = (m.clock, m.reset) match {
      case (_, Some(r)) => 
        unsupportedChisel(ctx, m, s"Reset inference ($r) unsupported for now")
        (m, "MultiIOModule")
      case (Some(c), None) => (m.mapPort(p => if(p.name == c) EmptyStmt else p), "MultiIOModule")
      case (None, None) => (m, "RawModule")
    }
    
    val comma = Seq(ChiselTxt(mCtxt, ", "))
    val s = ArrayBuffer[ChiselTxt]()

    s += ChiselLine(m, ctx, s"class ${m.name}(")
    if(mod.params.size > 0) {
      s ++= mod.params.map(_.chiselize(pCtxt, forceType=true) ++ comma).flatten.dropRight(1)
      s += ChiselLine(mCtxt, s") extends $kind {")
    } else {
      s += ChiselTxt(mCtxt, s") extends $kind {")
    }
    s ++= mod.body.chiselize(mCtxt)
    s += ChiselLine(ctx, "}")
  }
}

class ChiselDefFunction(val f: DefFunction) extends Chiselized {
  
  private def removeImplicitReturn(s: Statement): Statement = {
    def applyToLastStatement(s: Statement, f: Statement => Statement): Statement = {
      s match {
        case b:SimpleBlock => b.copy(stmts = b.stmts.dropRight(1) :+ applyToLastStatement(b.stmts.last, f))
        case st => f(st)
      }
    }
    applyToLastStatement(s, s => s match {
        case Connect(_,_,Reference(_,n,_,_,_,_),e,_,_) if(f.name == n) => ExpressionStatement(e)
        case _ => s 
      }
    )
  }
  
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val mCtxt = ctx.incr()
    val pCtxt = mCtxt.incr()
    
    val comma = Seq(ChiselTxt(mCtxt, ", "))
    val s = ArrayBuffer[ChiselTxt]()

    s += ChiselLine(f, ctx, s"def ${f.name}(")
    s ++= f.ports.map(_.chiselizeAsArgument(pCtxt) ++ comma).flatten.dropRight(1)
    s += ChiselTxt(mCtxt, s")")
    f.tpe match {
      case _:VoidType =>
      case u:UnknownType => 
        unsupportedChisel(ctx,u, "non-usable UnknownType")
        s += ChiselTxt(mCtxt, s" =")
      case tpe => 
        s += ChiselTxt(mCtxt, s": ")
        s ++= tpe.chiselize(mCtxt.hw(), scalaTypeOnly = true)
        s += ChiselTxt(mCtxt, s" =")
    }
    s += ChiselTxt(mCtxt, s" {")
    s ++= removeImplicitReturn(f.mapPort(_ => EmptyStmt).body).chiselize(mCtxt)
    s += ChiselLine(ctx, "}")
  }
}

class ChiselDefParam(val p: DefParam) extends Chiselized {
  val eq = ChiselTxtS(" = ")
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = ??? // should never happen
  def chiselize(ctx: ChiselEmissionContext, forceType: Boolean): Seq[ChiselTxt] = {
    val (value, tpe) = (p.value, p.tpe) match {
      case (None, _: UnknownType) => (Seq(), Seq(ChiselTxt(ctx, s"Int")))
      case (None, t: Type) => (Seq(), t.chiselize(ctx, scalaTypeOnly = true))
      case (Some(e), _: UnknownType) => // TO DO : remove should be done before
        val tpe = e match {
          case _: StringLit => "String"
          case _ => "Int"
        }
        (eq ++ e.chiselize(ctx), Seq(ChiselTxt(ctx, tpe)))
      case (Some(e), t: Type) => 
        // priority of HwExpressionKind value over ctx hw/sw
        val eCtx = e.kind match {
          case HwExpressionKind => ctx.hw() 
          case SwExpressionKind => ctx.sw()
          case _ => ctx 
        }
        (eq ++ e.chiselize(eCtx), t.chiselize(eCtx, scalaTypeOnly = true))
    }

    (forceType, value, p.tpe) match {
      case (true, _, _) => Seq(ChiselLine(p, ctx, s"val ${p.name}: ")) ++ tpe ++ value
      case (false, Seq(), _) => Seq(ChiselLine(p, ctx, s"val ${p.name}: ")) ++ tpe
      case (false, s, _: IntType) => Seq(ChiselLine(p, ctx, s"val ${p.name}")) ++ s
      case (false, s, _: StringType) => Seq(ChiselLine(p, ctx, s"val ${p.name}")) ++ s
      case (false, s, _: UnknownType) => Seq(ChiselLine(p, ctx, s"val ${p.name}")) ++ s
      case (false, s, _) => Seq(ChiselLine(p, ctx, s"val ${p.name}: ")) ++ tpe ++ s
    }
  }
}

class ChiselPort(val p: Port) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    Seq(ChiselLine(p, ctx, s"val ${p.name} = IO(${p.direction.serialize}(")) ++
      p.tpe.chiselize(ctx.hw()) ++ 
      Seq(ChiselTxt(ctx, s"))"))
  }
  def chiselizeAsArgument(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    Seq(ChiselTxt(p, ctx, s"${p.name}:")) ++ p.tpe.chiselize(ctx.hw(),scalaTypeOnly=true)
  }
}

class ChiselField(val f: Field) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val decl = s"val ${f.name} ="
    f.flip match {
      case Flip => 
        Seq(ChiselLine(f, ctx, s"$decl Flipped(")) ++
          f.tpe.chiselize(ctx.incr()) ++
          Seq(ChiselTxt(f, ctx, ")"))
        
      case _ => 
        Seq(ChiselLine(f, ctx, s"$decl ")) ++ f.tpe.chiselize(ctx.incr())
    }
  }
}

class ChiselEnumField(val f: EnumField) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = chiselize(ctx, false)
  def chiselize(ctx: ChiselEmissionContext, forceValues: Boolean): Seq[ChiselTxt] = {
    val decl = s"val ${f.name} ="
    forceValues match {
      case true => 
        Seq(ChiselLine(f, ctx, s"$decl Val(")) ++
          f.value.chiselize(ctx.incr()) ++
          Seq(ChiselTxt(f, ctx, ")"))
        
      case false => Seq(ChiselLine(f, ctx, s"$decl Value"))
    }
  }
}

class ChiselType(val t: Type) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = chiselize(ctx, false)
  
  def chiselize(ctx: ChiselEmissionContext, scalaTypeOnly: Boolean): Seq[ChiselTxt] = {
    val iCtxt = ctx.incr()
    
    def getVec(tpe: Seq[Type], w: Expression): Seq[ChiselTxt] = {
      tpe match {
        case Seq(t) if(scalaTypeOnly) => 
          ChiselTxtS(t, ctx, s"Vec[") ++ t.chiselize(ctx, scalaTypeOnly) ++ Seq(ChiselTxt(ctx, "]"))
            
        case Seq(t) => 
          ChiselTxtS(t, ctx, s"Vec(") ++ w.chiselize(ctx) ++ ChiselTxtS(", ") ++
            t.chiselize(ctx) ++
            Seq(ChiselTxt(ctx, ")"))
          
        case _ => unsupportedChisel(ctx,t, "TODO? MixedVec Type")
      }
    }
    
    t match {
      case v : VecType if(!ctx.isHardware) =>
        v.tpe match {
          case Seq(t) => 
            ChiselTxtS(t, ctx, s"Seq[") ++ t.chiselize(ctx) ++ Seq(ChiselTxt(ctx, "]"))
            
          case _ => unsupportedChisel(ctx,t, "MixedVec Type in scala context")
        }
    
      case p :PackedVecType => getVec(p.tpe, p.getWidth)
      case u :UnpackedVecType => getVec(u.tpe, u.getWidth)
      
      case b: BundleType if(scalaTypeOnly) => unsupportedChisel(ctx,b, "cannot refer to anonymous Bundle as scalaType")
      case b: BundleType =>
        Seq(ChiselTxt(t, ctx, s"new Bundle { ")) ++
          b.fields.map(_.chiselize(iCtxt)).flatten ++
          Seq(ChiselLine(ctx, "}"))

      case u: UnknownType => 
        unsupportedChisel(ctx,u, "non-usable UnknownType")
      
      case i: IntType => ChiselTxtS(i.tokens, "Int")
      case i: UIntType => 
        (i.width, ctx.isHardware) match {
          case (_, false) => ChiselTxtS(i.tokens, "UInt")
          case (UnknownWidth(), _) => ChiselTxtS(i.tokens, "UInt")
          case (_, _) if(scalaTypeOnly) => ChiselTxtS(i.tokens, "UInt")
          case (w, _) => ChiselTxtS(i.tokens, "UInt(") ++ w.chiselize(ctx) ++ ChiselTxtS(")")
        }
      case i: SIntType => 
        i.width match {
          case UnknownWidth() => ChiselTxtS(i.tokens, "SInt")
          case _ if(scalaTypeOnly) => ChiselTxtS(i.tokens, "SInt")
          case w => ChiselTxtS(i.tokens, "SInt(") ++ w.chiselize(ctx) ++ ChiselTxtS(")")
        }
        
      case t: TypeOf => 
        if(scalaTypeOnly) unsupportedChisel(ctx,t, "cannot use typeOf as scalaType")
        ChiselTxtS(t.tokens, "chiselTypeOf(") ++ t.expr.chiselize(ctx) ++ ChiselTxtS(")")

      
      case b: BoolType if(ctx.isHardware && scalaTypeOnly) => ChiselTxtS(b.tokens, "Bool")
      case b: BoolType if(ctx.isHardware) => ChiselTxtS(b.tokens, "Bool()")
      case b: BoolType => ChiselTxtS(b.tokens, "Boolean")
      case u: UserRefType => 
        u.tpe match {
          case _: BundleType if(scalaTypeOnly) => ChiselTxtS(u.tokens, u.serialize)
          case _: BundleType => ChiselTxtS(u.tokens, "new " + u.serialize)
          case _ => ChiselTxtS(u.tokens, u.serialize)
        }
        
      case r: RawScalaType => ChiselTxtS(r.tokens, r.str)
      case s: StringType => ChiselTxtS(s.tokens, "String")
      
      case _ => unsupportedChisel(ctx,t, "Unsupported Type: " + t)
    }
  }
}

class ChiselStatement(val s: Statement) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    s match {
      case h: Header => h.chiselize(ctx)
      case b: SimpleBlock => b.chiselize(ctx)
      case l: DefLogic => l.chiselize(ctx)
      case f: DefFunction => f.chiselize(ctx)
      case t: DefType => t.chiselize(ctx)
      case p: DefParam => p.chiselize(ctx, forceType=false)
      case c: Comment => c.chiselize(ctx)
      case c: Connect => c.chiselize(ctx)
      case i: IfGen => i.chiselize(ctx)
      case f: ForGen => f.chiselize(ctx)
      case c: Conditionally => c.chiselize(ctx)
      case i: DefInstance => i.chiselize(ctx)
      case s: Switch => s.chiselize(ctx)
      case p: Port => p.chiselize(ctx)
      case e: ExpressionStatement => Seq(ChiselLine(e, ctx, "")) ++ e.expr.chiselize(ctx)
      
      case p: Print => Seq(ChiselLine(s, ctx, p.serialize))
      case RawScala(str) => 
        // TODO : refacto => will fail on small chunks without intended newlines
        str.split("\n").map(txt => ChiselLine(s, ctx, txt))
      case EmptyStmt => Seq()
      
      // default, not likely to provide anything chisel-compatible  
      case _ => 
        unsupportedChisel(ctx,s, s"Unknown statement")
    }
    
  }
}

class ChiselHeader(val s: Statement) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    s match {
      case h: ImportPackages => h.chiselize(ctx)
      case c: CompilerDirective => Seq(ChiselLine(c, ctx, "// " + c.text + "\n"))
      case _ => unsupportedChisel(ctx,s, s"Unknown header")
    }
  }
}

class ChiselSimpleBlock(val s: SimpleBlock) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    s.stmts.map(_.chiselize(ctx)).flatten
  }
}

class ChiselDefLogic(val s: DefLogic) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val hCtxt = ctx.hw()
    
    val kind = (s.resolution, s.init) match {
      case (LogicRegister, _:UndefinedExpression)=> "Reg" 
      case (LogicRegister, _) => "RegInit" // optimizations required here 
      case (LogicWire, _:UndefinedExpression)=> "Wire" 
      case (LogicWire, _) => "WireDefault" 
      case _ => unsupportedChisel(ctx,s,s"Unexpected unresolved logic in chiselize step ${s.serialize}"); "Wire"
    }
    
    // to do : proper management of clock & reset for non trivial cases
    val decl = s.tpe match {
      case u: UnpackedVecType => 
        u.tpe match {
          case Seq(t) => Seq(ChiselLine(s, ctx, s"val ${s.name} = Mem(")) ++ u.getWidth().chiselize(ctx) ++
            ChiselTxtS(",") ++ t.chiselize(hCtxt)
          case _ => unsupportedChisel(ctx, s, "MixedVecType")
        }
      case _ => Seq(ChiselLine(s, ctx, s"val ${s.name} = $kind(")) ++ s.tpe.chiselize(hCtxt)
    }
    
    val comma = Seq(ChiselTxt(ctx, ", "))
    val rpar = Seq(ChiselTxt(ctx, ") "))
    
    decl ++ (s.init match {
      case _: UndefinedExpression => rpar
      case e => comma ++ e.chiselize(hCtxt) ++ rpar
    })
  }
}

class ChiselDefType(val t: DefType) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    def addHwEnumDep() = ctx.src.addDep(PackageRef(UndefinedInterval, "sv2chisel.helpers.enum", "_"))
    
    t.tpe match {
      case b: BundleType => 
        Seq(ChiselLine(t, ctx, s"class ${t.name} extends Bundle {")) ++
          b.fields.flatMap(_.chiselize(ctx.hw().incr())) ++
          Seq(ChiselLine(ctx, "} "))
          
      case e: EnumType if(e.isGeneric) => 
        addHwEnumDep()
        Seq(ChiselLine(t, ctx, s"object ${t.name} extends GenericHwEnum {")) ++
          e.fields.flatMap(_.chiselize(ctx.hw().incr())) ++
          Seq(ChiselLine(ctx, "} "))
          
      case e: EnumType => 
        addHwEnumDep()
        Seq(ChiselLine(t, ctx, s"object ${t.name} extends CustomHwEnum {")) ++
          e.fields.flatMap(_.chiselize(ctx.hw().incr(), forceValues = true)) ++
          Seq(ChiselLine(ctx, "} "))
      
      case tpe => // assuming alias case
        Seq(ChiselLine(t, ctx, s"object ${t.name} {"),
          ChiselLine(ctx.incr(), s"def apply() = ")) ++
          tpe.chiselize(ctx.hw()) ++
          Seq(ChiselLine(ctx, "} "))
    }
  }
}

// TO DO : remove all the logic within this function 
// => all what is defined here won't be applied recursively ...
// everything should be handled through serialize
class ChiselExpression(val e: Expression) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    e match {
      case x: StringLit =>  x.chiselize(ctx)
      case x: FillingBitPattern => x.chiselize(ctx)
      case x: AssignPattern => x.chiselize(ctx)
      case x: Reference => x.chiselize(ctx)
      case x: DoPrim => x.chiselize(ctx)
      case x: DoCast => x.chiselize(ctx)
      case x: DoCall => x.chiselize(ctx)
      case x: Concat => x.chiselize(ctx)
      case x: SubField => x.chiselize(ctx)
      case x: SubIndex => x.chiselize(ctx)
      case x: SubRange => x.chiselize(ctx)
      case x: Number => x.chiselize(ctx)
      case x: MaskedNumber => unsupportedChisel(ctx,e,s"MaskedNumber ($x) shall be emitted in very precise context") 
      case x: Assign => x.chiselize(ctx)
      case x: MappedValues => x.chiselize(ctx)
      case x: SeqValues => x.chiselize(ctx)
      case x: ReplicatePattern => x.chiselize(ctx)
      case x: UIntLiteral => x.chiselize(ctx)
      case x: SIntLiteral => x.chiselize(ctx)
      case x: BoolLiteral => x.chiselize(ctx)
      case x: RawScalaExpression => x.chiselize(ctx) 
      case x: RawScalaExprWrapper => x.chiselize(ctx)
      case x: TypeInst => x.chiselize(ctx)
      case x: DontCare => ChiselTxtS(x.tokens, "DontCare")
      
      case _: FixedLiteral => unsupportedChisel(ctx,e,"TODO: FixedLiteral") 
      case _: DefaultAssignPattern => unsupportedChisel(ctx,e,"DefaultAssignPattern") 
      case _: UndefinedExpression => unsupportedChisel(ctx,e,"UndefinedExpression")
    }
  }
}

class ChiselTypeInst(t: TypeInst){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    (t.name) match {
      case Some(n) => ChiselTxtS(t.tokens, "new " + n)
      case None => t.tpe.chiselize(ctx)
    }
  }
}

class ChiselRawScalaExpression(e: RawScalaExpression){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    (e.str.count(_ == '\n'), e.str.split("\n")) match {
      case (0, _) => Seq(ChiselTxt(e, ctx, e.str))
      case (1, Array(s)) => Seq(ChiselTxt(e, ctx, s))
      case (_, s) => s.toSeq.map(l => Seq(ChiselLine(e, ctx, l))).flatten
    }
  }
}

class ChiselRawScalaExprWrapper(e: RawScalaExprWrapper){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    def splitLines(str: String) = {
      (str.count(_ == '\n'), str.split("\n")) match {
        case (0, _) => Seq(ChiselTxt(e, ctx, str))
        case (1, Array(s)) => Seq(ChiselTxt(e, ctx, s))
        case (_, s) => 
          ChiselTxtS(e, ctx, s(0)) ++ 
            s.toSeq.tail.map(l => Seq(ChiselLine(e, ctx, l))).flatten
      }
    }
    
    val splitt = e.fmt.split("%e")
    val store = ArrayBuffer[ChiselTxt]()
    store ++= splitLines(splitt(0))
    splitt.toSeq.tail.zip(e.exprs).foreach(t => {
      store ++= t._2.chiselize(ctx)
      store ++= splitLines(t._1)
    })
    if (splitt.size == e.exprs.size) {
      // finishing with an expression
      store ++= e.exprs.last.chiselize(ctx)
    }
    store
  }
}

class ChiselUndefinedExpression(e: UndefinedExpression){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    unsupportedChisel(ctx,e, "UndefinedExpression shall be resolved before emission")
  }
}

class ChiselStringLit(e: StringLit){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    // to be refactored within stringlit ?
    val base = ChiselTxtS(e, ctx, e.escape)
    e.kind match {
      case HwExpressionKind => 
        e.width.expr.evalBigIntOption() match {
          case Some(b) if(b == e.string.length) => base ++ ChiselTxtS(".V")
          case _ => base ++ ChiselTxtS(".V(") ++ e.width.chiselize(ctx) ++ ChiselTxtS(")")
        }
      case _ => base
    }
    
  }
}

class ChiselReference(e: Reference){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    if(e.name.size > 1 && e.name.head == '`'){
      unsupportedChisel(ctx,e, "Unsupported tick reference in this context")
    } else {
      Seq(ChiselTxt(e, ctx, (e.path :+ e.name).mkString(".")))
    }
    
    // note : to re-integrate ??
    // I think it sould be based on local Type (new with refacto)??
    // it might be better to propagate Types first and then to do emission ???
    
    // override def serialize(isHardware: Boolean): String = {
    //   val ser =this.serialize
    //   if (isHardware) {
    //     tpe match {
    //       // Supported Software Types
    //       case u: IntType => s"$ser.U"
    //       case s: StringType =>
    //           unsupportedChisel(ctx,e,"String Type in hardware")
    //         ser
    //       // Any other
    //       case _ => ser
    //     }
    //   } else {
    //     ser
    //   }
    // 
    // }
    
  }
}

class ChiselAutoAssign(e: AutoAssign){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    unsupportedChisel(ctx,e, "AutoAssign shall be removed before emission")
  }
}

class ChiselNamedAssign(e: NamedAssign){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    Seq(ChiselTxt(e, ctx, s"${e.name} = ")) ++ e.expr.chiselize(ctx)
  }
}

class ChiselNoNameAssign(e: NoNameAssign){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    e.expr.chiselize(ctx)
  }
}

class ChiselSubField(e: SubField){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    e.expr.chiselize(ctx) ++ Seq(ChiselTxt(e, ctx, s".${e.name}"))
  }
}

class ChiselSubIndex(e: SubIndex){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    e.expr.chiselize(ctx) ++ ChiselTxtS(e, ctx, "(") ++ 
      e.index.chiselize(ctx) ++ ChiselTxtS(")")
  }
}

class ChiselSubRange(e: SubRange){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    def addVecImpl = ctx.src.addDep(PackageRef(UndefinedInterval, "sv2chisel.helpers.vecconvert", "_"))
    (e.expr.tpe, e.flow) match {
      case (_: VecType, _) => addVecImpl
      case (_:UIntType | _: SIntType, SourceFlow) => // implemented in Bits (superclass of UInt & SInt)
      case (_:UIntType | _: SIntType, SinkFlow) => addVecImpl
      case _ => rwarn(ctx, e, s"Probably unsupported subrange of expression ${e.expr.serialize} of type ${e.expr.tpe.serialize} (${e.flow})")
    }
    
    e.expr.chiselize(ctx) ++ ChiselTxtS(e, ctx, "(") ++ 
      e.left.chiselize(ctx) ++ ChiselTxtS(",") ++
      e.right.chiselize(ctx) ++ ChiselTxtS(")")
  }
}

class ChiselNumber(e: Number){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    def baseValue: Seq[ChiselTxt] = {
      e.base match {
        case NumberDecimal if(!e.value.contains("_") && e.value.length < 16) => ChiselTxtS(e, ctx, e.value)
        case NumberDecimal => ChiselTxtS(e, ctx, "\"d" + e.value + "\"")
        case NumberBinary  => ChiselTxtS(e, ctx, "\"b" + e.value + "\"")
        case NumberOctal   => ChiselTxtS(e, ctx, "\"o" + e.value + "\"")
        case NumberHexa    => ChiselTxtS(e, ctx, "\"h" + e.value + "\"")
      }
    }
    
    (e.kind, e.tpe) match {
      case (HwExpressionKind, SIntType(_, UnknownWidth())) => baseValue ++ ChiselTxtS(".U.asSInt")
      case (HwExpressionKind, SIntType(_, w)) => baseValue ++ ChiselTxtS(".U.asTypeOf(SInt(") ++ w.chiselize(ctx) ++ ChiselTxtS("))")
      case (HwExpressionKind, UIntType(_, UnknownWidth(), _)) => baseValue ++ ChiselTxtS(".U")
      case (HwExpressionKind, UIntType(_, w, _)) => baseValue ++ ChiselTxtS(".U(") ++ w.chiselize(ctx) ++ ChiselTxtS(")")
      case (SwExpressionKind, _) => baseValue
      case _ => 
        rcritical(ctx, e, s"Probably unsupported emission for number '${e.serialize}' $e")
        ChiselTxtS(e, ctx, e.value + "/* expect trouble here */")
    }
  }
}

class ChiselFillingBitPattern(e: FillingBitPattern){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    unsupportedChisel(ctx,e, "Filling bit pattern shall be removed before chiselize phase")
  }
}

class ChiselAssignPattern(e: AssignPattern){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    unsupportedChisel(ctx,e, "AssignPatterns shall be removed before chiselize phase")
  }
}

class ChiselDefaultAssignPattern(e: DefaultAssignPattern){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    unsupportedChisel(ctx,e, "DefaultAssignPattern shall be removed before chiselize phase")
  }
}

class ChiselMappedValues(e: MappedValues){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    unsupportedChisel(ctx,e, "MappedValues: TODO")
    ChiselTxtS(e, ctx, e.serialize)
  }
}

class ChiselSeqValues(e: SeqValues){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    (ctx.isHardware, e.kind) match {
      case (true, HwExpressionKind) => 
        val values = e.tpe match {
          case v: VecType if(v.downto) => e.values.reverse
          case _: VecType => e.values
          case _ => rcritical(ctx, e, s"unexpected type for SeqValues ${e.serialize}") ; e.values
        }
        
        if (values.isEmpty) {
          unsupportedChisel(ctx,e, "Empty VecInit are not allowed")
        } else {
          ChiselTxtS(e, ctx, "VecInit(") ++
            values.map(_.chiselize(ctx) ++ ChiselTxtS(", ")).flatten.dropRight(1) ++
            ChiselTxtS(")")
        }
      
      case (false, SwExpressionKind) => 
        val values = e.tpe match {
          case v: VecType if(v.downto) => e.values.reverse
          case _: VecType => e.values
          case _ => rcritical(ctx, e, s"unexpected type for SeqValues ${e.serialize}") ; e.values
        }
        
        if (values.isEmpty) {
          ChiselTxtS(e, ctx, "Seq()")
        } else {
          ChiselTxtS(e, ctx, "Seq(") ++
            values.map(_.chiselize(ctx) ++ ChiselTxtS(", ")).flatten.dropRight(1) ++
            ChiselTxtS(")")
        }
        
      case (b, _) => unsupportedChisel(ctx, e, s"Unexpected SeqValues of kind ${e.kind} in ${if(b) "hardware" else "software"} context")
    }
  }
}

class ChiselReplicatePattern(r: ReplicatePattern){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    ChiselTxtS(r, ctx, "VecInit.tabulate(") ++ 
      r.scaler.chiselize(ctx) ++ ChiselTxtS(")(_ => ") ++
      r.pattern.chiselize(ctx) ++ ChiselTxtS(")")
  }
}

class ChiselBoolLiteral(e: BoolLiteral){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    ChiselTxtS(e, ctx, e.serialize)
  }
}

class ChiselUIntLiteral(e: UIntLiteral){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    ChiselTxtS(e, ctx, e.serialize) // to refactor ???
  }
}

class ChiselSIntLiteral(e: SIntLiteral){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    ChiselTxtS(e, ctx, e.serialize) // to refactor ???
  }
}

class ChiselFixedLiteral(e: FixedLiteral){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    unsupportedChisel(ctx,e, "FixedLiteral")
  }
}

class ChiselDoPrim(e: DoPrim){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val uctx = ctx.unraw()
    (e.op, e.args)match {
      // Special cases:
      /// Parenthesis 
      case (p: Par, Seq(expr)) => ChiselTxtS(p, ctx, "(") ++ expr.chiselize(uctx) ++ ChiselTxtS(")")
      case (_: Par, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 1 was expected.") 
      /// Power of 
      case (p: Pow, Seq(expr, exp)) => 
        expr.serialize match {
          case "2" => ChiselTxtS(p, ctx, "1 << ") ++ exp.chiselize(uctx)
          case _ if(!ctx.isHardware) => 
            ChiselTxtS(p, ctx, "math.pow(") ++ exp.chiselize(uctx) ++
              ChiselTxtS(", ") ++ expr.chiselize(uctx)
          case _ => unsupportedChisel(ctx,e, "Unsupported `power of` operation in hardware")
        }
      /// Eq 
      case (e: Eq, Seq(e1, e2)) => 
        val op = if(ctx.isHardware) " === " else " == "
        e1.chiselize(uctx) ++ ChiselTxtS(e, ctx, op) ++ e2.chiselize(uctx)
      /// Neq 
      case (e: Neq, Seq(e1, e2)) => 
        val op = if(ctx.isHardware) " =/= " else " != "
        e1.chiselize(uctx) ++ ChiselTxtS(e, ctx, op) ++ e2.chiselize(uctx)

      /// LogicalShiftRight 
      case (e: LogShr, Seq(e1, e2)) => 
        val op = if(ctx.isHardware) " >> " else " >>> " // should not rely on context but on HwExpressionKind
        (e1.tpe, e2.kind) match {
          case (_: SIntType, SwExpressionKind) => // fine thanks to the bug (see below)
            rwarn(ctx, e, "FIXME: This logical shift on a SInt only behaves as expected due to a bug (https://github.com/freechipsproject/chisel3/issues/1528)")
          case (_: SIntType, HwExpressionKind) =>
            rcritical(ctx, e, "FIXME: this logical shift will be treated as arithmetic shift")
          case _ => 
        }
        
        e1.chiselize(uctx) ++ ChiselTxtS(e, ctx, op) ++ e2.chiselize(uctx)
        
      /// Arithmetic Shift Right 
      case (e: Shr, Seq(e1, e2)) => 
        val shTxt = e1.chiselize(uctx) ++ ChiselTxtS(e, ctx, " >> ") ++ e2.chiselize(uctx)
        (e1.tpe, e2.kind) match {
          case (_: SIntType, SwExpressionKind) => 
            // glitch on static shift, see https://github.com/freechipsproject/chisel3/issues/1528
            // if shift value is hardware, this will behave properly
            e2.evalBigIntOption match {
              case Some(bg) => 
                ChiselTxtS(e, ctx, "(") ++ shTxt ++ ChiselTxtS(s").pad($bg)")
              case _ => 
                // it should 
                ChiselTxtS(e, ctx, "(") ++ shTxt ++ ChiselTxtS(s").pad(") ++ e2.chiselize(ctx) ++ ChiselTxtS(")")
            }
          // if shift value is hardware, shift behaves properly
          // if shifted value is not SInt we don't care so we don't want to add cumbersome logic
          case _ => shTxt
        }
        
        

      /// Incr 
      case (i: Incr, Seq(expr)) => 
        if(i.prefix){
          unsupportedChisel(ctx,e, "Prefix incr")
        } else if(ctx.isHardware) {
          val ce = expr.chiselize(uctx)
          ce ++ ChiselTxtS(i, ctx, " := ") ++ ce ++ ChiselTxtS(" + 1.U")
        } else {
          expr.chiselize(uctx) ++ ChiselTxtS(i, ctx, " += 1")
        }
      case (_: Incr, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 1 was expected.") 
      /// Decr 
      case (i: Decr, Seq(expr)) => 
        if(i.prefix){
          unsupportedChisel(ctx,e, "Prefix incr")
        } else if(ctx.isHardware) {
          val ce = expr.chiselize(uctx)
          ce ++ ChiselTxtS(i, ctx, " := ") ++ ce ++ ChiselTxtS(" - 1.U")
        } else {
          expr.chiselize(uctx) ++ ChiselTxtS(i, ctx, " -= 1")
        }
      case (_: Decr, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 1 was expected.") 
      /// CeilLog2 
      case (i: CeilLog2, Seq(expr)) => 
        ChiselTxtS(i, ctx, "util.log2Ceil(") ++ expr.chiselize(uctx) ++ ChiselTxtS(")")
      case (_: CeilLog2, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 1 was expected.") 
      /// GetWidth 
      case (i: GetWidth, Seq(expr)) => 
        ChiselTxtS(i, ctx, "(") ++ expr.chiselize(uctx) ++ ChiselTxtS(").getWidth()")
      case (_: GetWidth, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 1 was expected.") 
      /// InlineIf 
      case (i: InlineIf, Seq(cond, conseq, alt)) => 
        val cd = cond.chiselize(uctx)
        val cs = conseq.chiselize(uctx)
        val al = alt.chiselize(uctx)
        if(e.kind == HwExpressionKind){
          ChiselTxtS(i, ctx, "Mux(") ++ cd ++ ChiselTxtS(", ") ++ cs ++ 
            ChiselTxtS(", ") ++ al ++ ChiselTxtS(")")
        } else {
          ChiselTxtS(i, ctx, "if(") ++ cd ++ ChiselTxtS(") ") ++ cs ++ 
            ChiselTxtS(" else ") ++ al
        }
      case (_: InlineIf, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 3 were expected.") 
      
      // Reduction operators
      case (r: RedOp, Seq(expr)) => 
        val chiExpr = expr.chiselize(uctx)
        r match {
          case o: OrRed => chiExpr ++ ChiselTxtS(o, ctx, ".orR()")
          case o: AndRed => chiExpr ++ ChiselTxtS(o, ctx, ".andR()")
          case o: XorRed => chiExpr ++ ChiselTxtS(o, ctx, ".xorR()")
          case o: NorRed => ChiselTxtS(o, ctx, "~") ++ chiExpr ++ ChiselTxtS(o, ctx, ".orR()")
          case o: NandRed => ChiselTxtS(o, ctx, "~") ++ chiExpr ++ ChiselTxtS(o, ctx, ".andR()")
          case o: XnorRed => ChiselTxtS(o, ctx, "~") ++ chiExpr ++ ChiselTxtS(o, ctx, ".xorR()")
        }
      case (_: RedOp, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 1 was expected.") 
      
      // Usual unary operators
      case (u: UnaryOp, Seq(expr)) => ChiselTxtS(u, ctx, s" ${u.serialize}") ++ expr.chiselize(uctx)
      case (_: UnaryOp, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 1 was expected.") 
      
      // usual bool & binary operators (add spaces)
      case (b: BoolOp, Seq(e1, e2)) => e1.chiselize(uctx) ++ ChiselTxtS(b, ctx, s" ${b.serialize} ") ++ e2.chiselize(uctx)
      case (_: BoolOp, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 2 were expected.")
       
      // usual binary operators
      case (b: BinaryOp, Seq(e1, e2)) => e1.chiselize(uctx) ++ ChiselTxtS(b, ctx, s"${b.serialize}") ++ e2.chiselize(uctx)
      case (_: BinaryOp, _) => unsupportedChisel(ctx,e, s"Unexpected number of args providen (${e.args.size}) where 2 were expected.") 
      
      // default: unsupported
      case _ => unsupportedChisel(ctx,e, "PrimOps")
    }
    
  }
}




class ChiselDoCast(e: DoCast){
  
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    rtrace(ctx, e, s"chiselize: ${e.expr.serialize}: ${e.expr}")
    trace(s"kind: ${e.expr.kind}")
    val uctx = ctx.unraw()
    (Utils.isSimple(e.expr), e.expr.kind, e.tpe, e.expr.tpe.widthOption) match {
      // Bool to Int Special for Software & Hardware
      case (_, SwExpressionKind, IntType(t, _), _) if(e.expr.tpe.isInstanceOf[BoolType]) =>
        ChiselTxtS("(if(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ") 1 else 0)")
      
      // No need to cast here for hardware => Bool is a subtype os UInt 
      // case (_, HwExpressionKind, UIntType(t, _)) if(e.expr.tpe.isInstanceOf[BoolType]) =>
      //   ChiselTxtS("Mux(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ", 1.U, 0.U)")
        
      // UInt Specials for Software
      // > Simple Expr
      case (true, SwExpressionKind, UIntType(t, UnknownWidth(), _), _) =>
        e.expr.chiselize(uctx) ++ ChiselTxtS(t, ".U")
      case (true, SwExpressionKind, UIntType(t, w, _), _) => 
        e.expr.chiselize(uctx) ++ ChiselTxtS(t, ".U(") ++ w.chiselize(uctx) ++ ChiselTxtS(")")
      // > Require brace
      case (false, SwExpressionKind, UIntType(t, UnknownWidth(), _), _) => 
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ").U") 
      case (false, SwExpressionKind, UIntType(t, w, _), _) => 
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ").U(") ++ w.chiselize(uctx) ++ ChiselTxtS(")")
        
      // SInt Specials for Software
      // > Simple Expr
      case (true, SwExpressionKind, SIntType(t, UnknownWidth()), _) =>
        e.expr.chiselize(uctx) ++ ChiselTxtS(t, ".S")
      case (true, SwExpressionKind, SIntType(t, w), _)=> 
        e.expr.chiselize(uctx) ++ ChiselTxtS(t, ".S(") ++ w.chiselize(uctx) ++ ChiselTxtS(")")
      // > Require brace
      case (false, SwExpressionKind, SIntType(t, UnknownWidth()), _) => 
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ").S") 
      case (false, SwExpressionKind, SIntType(t, w), _) => 
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ").S(") ++ w.chiselize(uctx) ++ ChiselTxtS(")")
        
      // UInt Specials for Hardware NB: asUInt cannot cast to given width 
      // > Simple Expr
      case (true, HwExpressionKind, UIntType(t, UnknownWidth(), _), _) =>
        e.expr.chiselize(uctx) ++ ChiselTxtS(t, ".asUInt")
      // > Require brace
      case (false, HwExpressionKind, UIntType(t, UnknownWidth(), _), _) => 
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ").asUInt")
      
      case (true, HwExpressionKind, UIntType(t, wc, _), Some(we)) if(Utils.eq(wc, we)) =>
        e.expr.chiselize(uctx) ++ ChiselTxtS(t, ".asUInt")
      case (false, HwExpressionKind, UIntType(t, wc, _), Some(we)) if(Utils.eq(wc, we)) =>
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ").asUInt")
      
      // SInt Specials for Hardware  NB: asSInt cannot cast to given width 
      // > Simple Expr
      case (true, HwExpressionKind, SIntType(t, UnknownWidth()), _) =>
        e.expr.chiselize(uctx) ++ ChiselTxtS(t, ".asSInt")
      // > Require brace
      case (false, HwExpressionKind, SIntType(t, UnknownWidth()), _) => 
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ").asSInt") 
      
      // Bool Specials
      case (_, _, BoolType(_, s), _) if(s) => unsupportedChisel(ctx, e, "Signed BoolType unexpected here")
      case (true, SwExpressionKind, BoolType(t, _), _) if(e.kind == SwExpressionKind) => 
        ChiselTxtS("(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, " != 0)")
      case (true, SwExpressionKind, BoolType(t, _), _) if(e.kind == HwExpressionKind) => 
        e.expr.tpe match {
          case _: BoolType => e.expr.chiselize(uctx) ++ ChiselTxtS(t, ".B")
          case _ => ChiselTxtS("(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, " != 0).B")
        }
      
        
      case (false, SwExpressionKind, BoolType(t, _), _) if(e.kind == SwExpressionKind) => 
        ChiselTxtS("((") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ") != 0)")
      case (false, SwExpressionKind, BoolType(t, _), _) if(e.kind == HwExpressionKind) => 
        e.expr.tpe match {
          case _: BoolType => ChiselTxtS("(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ").B")
          case _ => ChiselTxtS("((") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(t, ") != 0).B")
        }
      
      case (isSimple, HwExpressionKind, BoolType(t, _), _) =>
        val pre = if(isSimple) "" else "("
        val post = if(isSimple) "" else ")"
        val comp = e.expr.tpe match {
          case _: UIntType => ChiselTxtS(t, s"$post =/= 0.U)") 
          case _: SIntType => ChiselTxtS(t, s"$post =/= 0.S)")
          case _ => ChiselTxtS(t, s"$post.asUInt =/= 0.U)")
        }
        ChiselTxtS(s"($pre") ++ e.expr.chiselize(uctx) ++ comp
      
      // x.asTypeOf(TypeOf(x)) => x
      case (_, _, TypeOf(_, expr), _) if(expr == e.expr) => e.expr.chiselize(uctx)
      
      // asTypeOf(TypeOf(x)) => asTypeOf(x)
      case (true, _, TypeOf(_, expr), _) => 
        e.expr.chiselize(uctx) ++ ChiselTxtS(e, ctx, ".asTypeOf(") ++
        expr.chiselize(uctx) ++ ChiselTxtS(")")
      case (false, _, TypeOf(_, expr), _) => 
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(").asTypeOf(") ++
        expr.chiselize(uctx) ++ ChiselTxtS(")")
        
      // Classique asTypeOf => first cast as signed integer for sw int 
      case (true, SwExpressionKind, _, _) => 
        e.expr.chiselize(uctx) ++ ChiselTxtS(e, ctx, ".S.asTypeOf(") ++
        e.tpe.chiselize(uctx) ++ ChiselTxtS(")")
      case (false, SwExpressionKind, _, _) => 
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(").S.asTypeOf(") ++
        e.tpe.chiselize(uctx) ++ ChiselTxtS(")")
        
      case (isS, _, v: VecType, _) => 
        val width = e.expr.tpe.widthOption match {
          case Some(w) => w.expr.evalBigIntOption() 
          case None => None 
        }
        rtrace(ctx, e, s"width: $width ; e.tpe : ${e.expr.tpe.serialize} ; e: ${e.expr.serialize}")
        val cast = (v.getWidth.evalBigIntOption, width, e.expr.tpe, v.tpe) match {
          case (Some(i1), Some(i2), (_:UIntType | _: SIntType),_) if (i1 == i2 && ctx.isRawConnect) => ChiselTxtS(".asBools")
          case (Some(i), _, _, Seq(t)) => ChiselTxtS(s".asTypeOf(Vec($i, ") ++ t.chiselize(uctx) ++ ChiselTxtS("))") 
          case _ => ChiselTxtS(".asTypeOf(") ++ v.chiselize(uctx) ++ ChiselTxtS(")") 
        }
        if(isS)
          e.expr.chiselize(uctx) ++ cast
        else
          ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(")") ++ cast
        
      case (true, _, _, _) => 
        e.expr.chiselize(uctx) ++ ChiselTxtS(e, ctx, ".asTypeOf(") ++
        e.tpe.chiselize(uctx) ++ ChiselTxtS(")")
      case _ => 
        ChiselTxtS(e, ctx, "(") ++ e.expr.chiselize(uctx) ++ ChiselTxtS(").asTypeOf(") ++
        e.tpe.chiselize(uctx) ++ ChiselTxtS(")")
    }
    
  }
}

class ChiselDoCall(e: DoCall){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val c = ChiselTxtS(", ")
    val args = e.args.map(_.chiselize(ctx) ++ c).flatten match {
      case Seq() => Seq()
      case s => s.dropRight(1) 
    }
    e.fun.chiselize(ctx) ++ ChiselTxtS(e, ctx, "(") ++ args ++ ChiselTxtS(")")
  }
}

class ChiselConcat(e: Concat){
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    ctx.src.addDep(PackageRef(UndefinedInterval, "chisel3.util", "Cat"))
    val c = ChiselTxtS(", ")
    val args = e.args.map(_.chiselize(ctx) ++ c).flatten match {
      case Seq() => Seq()
      case s => s.dropRight(1) 
    }
    val decl = ChiselTxtS(e, ctx, "Cat(") ++ args ++ ChiselTxtS(")")
    e.tpe match {
      case v: VecType => 
        (v.getWidth.evalBigIntOption, v.tpe) match {
          case (Some(i), Seq(t)) => decl ++ ChiselTxtS(s".asTypeOf(Vec($i, ") ++ t.chiselize(ctx) ++ ChiselTxtS("))") 
          case _ => decl ++ ChiselTxtS(".asTypeOf(") ++ v.chiselize(ctx) ++ ChiselTxtS(")") 
        }
      case _ => decl // add case for SInt ??
    }
    
  }
}


class ChiselComment(val c: Comment) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    c.str.split("\n").map(s => {
      ChiselLine(c, ctx, s"// $s")
    })
  }
}

class ChiselWidth(val w: Width) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    w.expr match {
      // avoid useless parenthesis
      case r: Reference => r.chiselize(ctx) ++ ChiselTxtS(".W")
      case n: Number => n.chiselize(ctx) ++ ChiselTxtS(".W")
      case l: Literal => l.chiselize(ctx) ++ ChiselTxtS(".W")
      case e => ChiselTxtS("(") ++ e.chiselize(ctx) ++ ChiselTxtS(").W")
    }
  }
}

class ChiselConnect(val c: Connect) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val hCtxt = ctx.hw()
    c.loc.chiselize(hCtxt.raw()) match {
      case Seq() => Seq()
      case head :: tail =>
        Seq(head.copy(newLine = true)) ++ tail ++
          Seq(ChiselTxt(ctx, " := ")) ++
          c.expr.chiselize(hCtxt.raw()) // , getRef(c.loc).tpe => should be done previously ...
    }
  }
}

class ChiselIfGen(val i: IfGen) extends Chiselized {
  
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val iCtxt = ctx.incr()
    
    def getAlt(alt: Statement): Seq[ChiselTxt] = {
      alt match {
        case EmptyStmt => Seq()
        case inner: IfGen => // flatten "else { if () {} [else {}]}"
          Seq(ChiselLine(inner, ctx, "} else if(")) ++
            inner.pred.chiselize(ctx) ++
            Seq(ChiselTxt(inner, ctx, ") {")) ++
            inner.conseq.chiselize(iCtxt) ++
            getAlt(inner.alt)
          
        case s: Statement =>
          Seq(ChiselLine(ctx, "} else {")) ++
            s.chiselize(iCtxt)
      }
    }
    
    val r = ArrayBuffer[ChiselTxt]()
    r += ChiselLine(i, ctx, "if(")
    r ++= i.pred.chiselize(ctx)
    r += ChiselTxt(ctx, ") {")
    r ++= i.conseq.chiselize(iCtxt)
    r ++= getAlt(i.alt)
    r += ChiselLine(i, ctx, "}")
    r
  }
}

class ChiselConditionally(val i: Conditionally) extends Chiselized {
  
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val iCtxt = ctx.incr()
    val hCtxt = ctx.hw()
    
    def getAlt(alt: Statement): Seq[ChiselTxt] = {
      alt match {
        case EmptyStmt => Seq()
        case inner: Conditionally => // flatten "else { if () {} [else {}]}"
          Seq(ChiselLine(inner, ctx, "} .elsewhen (")) ++
            inner.pred.chiselize(hCtxt) ++
            Seq(ChiselTxt(inner, ctx, ") {")) ++
            inner.conseq.chiselize(iCtxt) ++
            getAlt(inner.alt)
          
        case s: Statement =>
          Seq(ChiselLine(ctx, "} .otherwise {")) ++
            s.chiselize(iCtxt)
      }
    }
    
    val r = ArrayBuffer[ChiselTxt]()
    r += ChiselLine(i, ctx, "when(")
    r ++= i.pred.chiselize(hCtxt)
    r += ChiselTxt(ctx, ") {")
    r ++= i.conseq.chiselize(iCtxt)
    r ++= getAlt(i.alt)
    r += ChiselLine(i, ctx, "}")
    r
  }
}

class ChiselForGen(val f: ForGen) extends Chiselized {
  // return the loop declaration when it is a simple one
  // Empty Seq() means not a simple Iter
  private def isSimpleBound(e: Expression): Option[(String, String, Expression)] = {
    e match {
      case DoPrim(_, PrimOps.Lt(_), Seq(r: Reference, exp: Expression), _, _) => 
      Some((r.serialize, "until", exp))
      case DoPrim(_, PrimOps.Leq(_), Seq(r: Reference, exp: Expression), _, _) => 
      Some((r.serialize, "to", exp))
      case _ => None
    }
  }
  
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val iCtxt = ctx.incr()
    
    def simpleIncr(name: String, toUntil: String, bound: Expression): (Seq[ChiselTxt]) = {
      val (b, n, bytxt) = f.step match {
        case DoPrim(_, PrimOps.Incr(_, prefix), Seq(r: Reference), _, _) => 
          if(prefix){
            rcritical(ctx, f.step, "Prefixed increments are not supported.")
            (false, "", Seq())
          } else {
            (true, r.serialize, Seq())
          }
        case i: NamedAssign => 
          val (sub, exp) = i.expr match {
            case DoPrim(_,PrimOps.Add(_),Seq(r: Reference, e),_,_) if(r.serialize == i.name) => ("", Some(e))
            case DoPrim(_,PrimOps.Sub(_),Seq(r: Reference, e),_,_) if(r.serialize == i.name) => ("-", Some(e))
            case _ => ("", None)
          }
          exp match {
            case Some(e) => 
              (e, sub) match {
                case (Number(_, "1",_,_,_), "") => (true, i.name, Seq())
                case _ => (true, i.name, ChiselTxtS(s" by $sub") ++ e.chiselize(ctx))
              }
              
            case None => 
              rcritical(ctx, f.step, s"step: ${f.step}")
              (false, i.name, Seq())
          }

        case _ => (false, "", Seq())
      }
      (b, f.init) match {
        case (true, i: NamedAssign) if(name == i.name && name == n) => 
          Seq(ChiselLine(f, ctx, s"for(${name} <- ")) ++
              i.expr.chiselize(ctx.hw()) ++
              Seq(ChiselTxt(ctx, s" $toUntil ")) ++
              bound.chiselize(ctx) ++ bytxt ++
              Seq(ChiselTxt(ctx, s")"))

        case (true, i: NamedAssign) => 
          rwarn(ctx, f, s"Unable to infer simple for loop due to the use of different variables in init (${i.name}), bound ($name) and step ($n) fields.")
          Seq()
          
        case _ => Seq()
      }
    }
    
    def simpleIter(): Seq[ChiselTxt] = {
      isSimpleBound(f.stop) match {
        case None => Seq()
        case Some((name, toUntil, exp)) => simpleIncr(name, toUntil, exp) 
      }
    }
    // TWO CASES : 
    // FIRST simple pattern with simple increment
    // => emit for(<ref> )
    val decl = simpleIter() match {
      case Seq() => unsupportedChisel(ctx,f, "Non-trivial for loop shall be converted to while loop")
        
      case s => s
    }
    decl ++ Seq(ChiselTxt(ctx, "{")) ++
      f.stmt.chiselize(iCtxt) ++
      Seq(ChiselLine(ctx, "}"))
  }
}

class ChiselDefInstance(val i: DefInstance) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val iiCtxt = ctx.incr().incr()
    val hctx = ctx.hw()
    
    def getParams(a: Assign): Seq[ChiselTxt] = {
      val params = a match {
        case na: NamedAssign => 
          Seq(ChiselLine(na, iiCtxt, s"${na.name} = ")) ++
            na.expr.chiselize(iiCtxt.hw()) 
            
        case raw: NoNameAssign => raw.expr.chiselize(iiCtxt.hw())
            
        case _ => 
          unsupportedChisel(ctx,a,"unexpected param assign")
      }
      params ++ Seq(ChiselTxt(iiCtxt, s","))
    }
    
    // common part
    val openBracket = Seq(ChiselTxt(ctx, "("))
    val closeBracket = Seq(ChiselTxt(ctx, ")"))
    val finalBracket = Seq(ChiselLine(ctx, ")"))
    val decl = ArrayBuffer[ChiselTxt]()
    decl += ChiselLine(i, ctx, s"val ${i.name} = Module(new ${i.module.serialize}")
    decl ++= ((i.paramMap.map(getParams).flatten) match {
      case Seq() => closeBracket
      case s => openBracket ++ s.dropRight(1) ++ finalBracket ++ closeBracket
    })
    // Port map
    // Best effort based on predefined flows
    decl ++ (i.portMap.map( a => {
      a match {
        case na: NamedAssign => 
          rdebug(ctx, na, s"${na.serialize}: $na")
          (na.expr, na.flow) match {
            case (_:UndefinedExpression, UnknownFlow) => 
              val str = s"Unable to infer proper direction for port ${na.name} in instance ${i.name} of module ${i.module.serialize}"
              rcritical(ctx, na, s"$str. Some comment were left in code for fast manual resolution")
              val comment = s"""// TODO FIXME $str
              |// NOTE: > uncomment the following line if ${na.name} is an input of ${i.name}.
              |//       > remove these comments it if ${na.name} is an output of ${i.name}.
              |// ${i.name}.${na.name} := DontCare
              """.stripMargin
              Seq(ChiselLine(na, ctx, comment))
            case (r@Reference(_,_,_, _: BundleType,_, _), SinkFlow) => 
                // default: cast with asTypeOf
                val chi = r.chiselize(hctx)
                Seq(ChiselLine(na, ctx, "")) ++ chi ++
                  ChiselTxtS(s" := ${i.name}.${na.name}.asTypeOf(") ++
                  chi ++ ChiselTxtS(")")
                  
            case (r@Reference(_,_,_, _: BundleType,_, _), SourceFlow) => 
                // default: cast with asTypeOf
                Seq(ChiselLine(na, ctx, s"${i.name}.${na.name} := ")) ++
                  r.chiselize(hctx) ++
                  ChiselTxtS(s".asTypeOf(${i.name}.${na.name})")
            
            // special case for undefined expression (nothing connected)
            case (_:UndefinedExpression, SourceFlow) => 
              Seq(ChiselLine(na, ctx, s"${i.name}.${na.name} := DontCare"))
              
            case (_:UndefinedExpression, SinkFlow) => Seq() // no need to emit
            
            case (e, SourceFlow) => 
              Seq(ChiselLine(na, ctx, s"${i.name}.${na.name} := ")) ++
                e.chiselize(hctx)
                
            case (e, SinkFlow) => 
              val ref = na.assignExpr match {
                case Some(exp) => ChiselTxtS(na, ctx, s" := ") ++ exp.chiselize(hctx)
                case _ => ChiselTxtS(na, ctx, s" := ${i.name}.${na.name}")
              }
              Seq(ChiselLine(na, ctx, "")) ++ e.chiselize(hctx) ++ ref
                
            case _ => 
              Seq(ChiselLine(na, ctx, s"${i.name}.${na.name} <> ")) ++
                na.expr.chiselize(hctx) // UnknownType without more context here ...
          }
        case _ => // to do => feasible thanks to flows
          unsupportedChisel(ctx,a, s" implicit port map as Module `${i.module.serialize}` is not known in current scope")
      }
      
    }).flatten)
  }
}

class ChiselSwitch(val s: Switch) extends Chiselized {
  def chiselize(ctx: ChiselEmissionContext): Seq[ChiselTxt] = {
    val iCtxt = ctx.incr()
    unsupportedChisel(ctx, s, "Untested switch statement emission")
    // always emit when .elsewhen .otherwise for now
    // other approaches could be considered for example emit chisel switch in case of literals
    lazy val comp = s.expr.chiselize(ctx)
    def getExpression(e: Expression) : Seq[ChiselTxt] = {
      e match {
        case MaskedNumber(_, num, mask) =>
          ChiselTxtS(s.expr, ctx, "(") ++ comp ++ 
            ChiselTxtS(" & ") ++ mask.chiselize(iCtxt) ++ ChiselTxtS(") === ") ++ num.chiselize(iCtxt)
        case _ => comp ++ ChiselTxtS(" === ") ++ e.chiselize(iCtxt)
      }
    }
    
    s.cases match {
      case Seq() => unsupportedChisel(ctx, s, "Empty case statement")
      case c => 
        val main = Seq(ChiselLine(s, ctx, s"when(")) ++ getExpression(c.head._1) ++ ChiselTxtS("){") ++
          c.head._2.chiselize(iCtxt) ++
          c.tail.flatMap(t => {
            Seq(ChiselLine(ctx, s"} .elsewhen(")) ++ getExpression(t._1) ++ ChiselTxtS("){") ++
              t._2.chiselize(iCtxt)
          })
        s.default match {
          case EmptyStmt => main ++ Seq(ChiselLine(ctx, s"}"))
          case d => main ++ Seq(ChiselLine(ctx, s"} .otherwise { ")) ++ 
            d.chiselize(iCtxt) ++ Seq(ChiselLine(ctx, s"}"))
        }
    }
  }
}
