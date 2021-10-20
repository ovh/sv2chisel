// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import org.antlr.v4.runtime.{ParserRuleContext,CommonTokenStream, Token}
import org.antlr.v4.runtime.tree.{AbstractParseTreeVisitor, ParseTreeVisitor, TerminalNode}
import scala.collection.JavaConverters._
import scala.collection.mutable.{ArrayBuffer}
import scala.util.matching.Regex

import sv2chisel.antlr._
import sv2017Parser._
import sv2chisel.ir._
import sv2chisel.ir.PrimOps._
import sv2chisel.ir.evalExpression._
import Utils.throwInternalError

import logger.EasyLogging

class Visitor(
    val unsupportedMode: UnsupportedBehavior,
    val tokenStream: CommonTokenStream,
    val path: String
  ) extends AbstractParseTreeVisitor[SVNode] with ParseTreeVisitor[SVNode] with ParserLogging {

  def visit(ctx: Source_textContext): SourceFile = {
    val s = visitSourceText(ctx, path)
    unsupported.raise()
    s
  }
  /////// END OF PUBLIC FACING API //////
  private val ut = UnknownType()
  private val uk = UnknownExpressionKind
  private val uw = UnknownWidth()
  
  // underlying unsupported logger
  private val unsupported = new RaiseUnsupported(unsupportedMode, tokenStream, path)
  
  // helper
  private def throwParserError(ctx: ParserRuleContext, str: String = "") = {
    Utils.throwInternalError(s"Parser Issue ($str) in context: ${getRawText(ctx)}")
  }
  
  private def visitSourceText(ctx: Source_textContext, path: String): SourceFile = {
    SourceFile(ctx.getSourceInterval(), path, ctx.description.asScala.map(visitDescription))
  }
  
  private def unsupportedDesc(ctx: ParserRuleContext, msg : String): Description = {
    unsupported.raiseIt(ctx, s"Unsupported Description $msg please feel free to contribute !"); 
    UnrecognizedDescription(ctx.getSourceInterval)
  }
  
  private def visitDescription(ctx: DescriptionContext): Description = {
    ctx.getChild(0) match {
      case m: Module_declarationContext => visitModule(m)
      case h: Header_textContext => visitHeader(h)
      case p: Package_declarationContext => visitPackage_declaration(p)
      
      case u: Udp_declarationContext => unsupportedDesc(u, "Udp_declarationContext")
      case i: Interface_declarationContext => unsupportedDesc(i, "Interface_declarationContext")
      case p: Program_declarationContext => unsupportedDesc(p, "Program_declarationContext")
      case c: Config_declarationContext => unsupportedDesc(c, "Config_declarationContext")
      case _ => 
        val attr = visitAttributes(ctx.attribute_instance.asScala)
        (ctx.package_item_item, ctx.bind_directive) match {
          case (p, null) => unsupportedDesc(p, "Isolated Package Item")
          case (null, b) => unsupportedDesc(b, "Isolated Bind Directive")
          case _ => throwParserError(ctx)
        }
    }
  }
  
  private def visitPackage_item(ctx: Package_itemContext): Statement = {
    val attr = visitAttributes(ctx.attribute_instance.asScala)
    ctx.package_item_item.getChild(0) match {
      case n: Net_declarationContext => visitNet_declaration(n, attr)
      case d: Data_declarationContext => visitData_declaration(d, attr)
      case l: Local_parameter_declarationContext => getStmtOrSimpleBlock(ctx, visitLocal_parameter_declaration(l, attr))
      case p: Parameter_declarationContext => getStmtOrSimpleBlock(ctx, visitParameter_declaration(p, attr))
      
      case f: Function_declarationContext => unsupportedStmt(f, "TODO function_declaration")
      case p: Package_export_declarationContext => unsupportedStmt(p, "TODO ? package_export_declaration")
      
      case t: Task_declarationContext => unsupportedStmt(t, "task_declaration")
      case c: Checker_declarationContext => unsupportedStmt(c, "checker_declaration")
      case d: Dpi_import_exportContext => unsupportedStmt(d, "dpi_import_export")
      case e: Extern_constraint_declarationContext => unsupportedStmt(e, "extern_constraint_declaration")
      case c: Class_declarationContext => unsupportedStmt(c, "class_declaration")
      case i: Interface_class_declarationContext => unsupportedStmt(i, "interface_class_declaration")
      case c: Class_constructor_declarationContext => unsupportedStmt(c, "class_constructor_declaration")
      case c: Covergroup_declarationContext => unsupportedStmt(c, "covergroup_declaration")
      case p: Property_declarationContext => unsupportedStmt(p, "property_declaration")
      case s: Sequence_declarationContext => unsupportedStmt(s, "sequence_declaration")
      case l: Let_declarationContext => unsupportedStmt(l, "let_declaration")
      case a: Anonymous_programContext => unsupportedStmt(a, "anonymous_program")
      case t: Timeunits_declarationContext => unsupportedStmt(t, "timeunits_declaration")
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitPackage_declaration(ctx: Package_declarationContext): Description = {
    unsupported.check(ctx)
    
    val attr = visitAttributes(ctx.attribute_instance.asScala)
    val id = ctx.identifier.asScala match {
      case Seq(id) => id.getText  
      case Seq(id1, id2) => 
        val decl = id1.getText
        val closing = id2.getText
        if(decl != closing)
          unsupported.raiseIt(ctx, "Inconsistent package identifiers found, using ${id1.getText} while ignoring ${id2.getText}")
        decl
      case _ => throwParserError(ctx)
    }
    val body = ctx.package_item.asScala.map(visitPackage_item) match {
      case Seq() => EmptyStmt
      case Seq(s) => s
      case s => SimpleBlock(s)
    }

    DefPackage(ctx.getSourceInterval, attr, id, body)
  }
  
  private def visitPackageImport(ctx: Package_import_declarationContext): ImportPackages = {
    val packages = ctx.package_import_item.asScala.map{ p => 
      p.identifier.asScala match {
        case Seq(h) => PackageRef(ctx.getSourceInterval(),h.getText())
        case Seq(h,q) => PackageRef(ctx.getSourceInterval(),h.getText(),q.getText())
        case _ => throwParserError(ctx)
      }
    }
    ImportPackages(ctx.getSourceInterval(),packages)
  }
  
  private def visitHeader(ctx: Header_textContext): Header = {
    ctx.getChild(0) match {
      case c : Compiler_directiveContext => 
        c.getChild(0) match {
          case t: Timescale_compiler_directiveContext => 
            TimescaleDirective(t.getSourceInterval(),c.getText)
            
          case d: Default_nettype_statementContext =>     
            DefaultNettypeStatement(d.getSourceInterval(),c.getText)
            
          case i: Include_svhContext => 
            unsupported.raiseIt(ctx, "TO DO => go find include & parse it as a package to be included")
            // TO DO
            val pkg = PackageRef(i.getSourceInterval(),i.STRING_LITERAL.getText())
            ImportPackages(i.getSourceInterval(),Seq(pkg))
            
        }

      case a: Design_attributeContext => DesignAttribute(a.getSourceInterval,visitAttributeInstance(a.attribute_instance))

      case i: Package_import_declarationContext => visitPackageImport(i)
        
    }
  }
  
  private def visitAttributes(s: Seq[Attribute_instanceContext]): VerilogAttributes = {
    s match {
      case Seq() => NoVerilogAttribute
      case _ => s.map(visitAttributeInstance).reduce((a: VerilogAttributes, b: VerilogAttributes) => a ++ b)
    }
  }
  
  private def visitAttributeInstance(ctx: Attribute_instanceContext) : VerilogAttributes = {
    (ctx.attr_spec.asScala, ctx.TICK_IDENTIFIER) match {
      case (Seq(), id) => VerilogAttribute(ctx.getSourceInterval(), id.getText())
      case (l, null) => 
        l.map { child =>
          val name = getRefText(child.identifier)
          child.expression match {
            case null => VerilogAttribute(child.getSourceInterval(),name)
            case expr => VerilogAttribute(child.getSourceInterval(),name, Some(expr.getText()))
          }
        }.reduce((a: VerilogAttributes, b: VerilogAttributes) => a ++ b)
      case _ => throwParserError(ctx)
    }
    
    
  }
  
  
  private def getExpressionType(ctx: ExpressionContext) : Type = {
    unsupported.raiseIt(ctx, s"Unsupported type expression ${getRawText(ctx)}")
    UnknownType()
  }
  
  private def getIntegerWidth(ctx: Integer_atom_typeContext) : Width = {
    (ctx.KW_BYTE,ctx.KW_SHORTINT,ctx.KW_INT,ctx.KW_LONGINT,ctx.KW_INTEGER,ctx.KW_TIME) match {
      case (b,null,null,null,null,null) => Width(8)
      case (null,s,null,null,null,null) => Width(16)
      case (null,null,i,null,null,null) => Width(32)
      case (null,null,null,l,null,null) => Width(64)
      case (null,null,null,null,i,null) => Width(64)
      case (null,null,null,null,null,t) => unsupported.raiseIt(ctx, s"Unsupported keyword $t") ; UnknownWidth()
      case _ => throwParserError(ctx)
    }
  }
  
  private def getResolution(ctx: Integer_vector_typeContext) : LogicResolution = {
    (ctx.KW_BIT,ctx.KW_LOGIC,ctx.KW_REG) match {
      case (b, null, null) => LogicUnresolved
      case (null, l, null) => LogicUnresolved
      case (null, null, l) => LogicRegister
      case _ => throwParserError(ctx)
    }
  }
  
  private def isSigned(ctx: SigningContext) : Boolean = {
    ctx match {
      case null => false
      case s => (s.KW_UNSIGNED, s.KW_SIGNED) match {
        case (u, null) => false  
        case (null, s) => true
        case _ => throwParserError(ctx)
      }
    }
  }
  
  private def getIntegerType(ctx: Integer_typeContext, signed: Boolean, isHw: Boolean): (Type, LogicResolution) = {
    val itvl = ctx.getSourceInterval()
    (ctx.integer_atom_type, ctx.integer_vector_type) match {
      case (a, null) => 
        val w = getIntegerWidth(a)
        val t = (signed, isHw) match {
          case (true, true) => SIntType(itvl,w)
          case (false, true) => UIntType(itvl,w, NumberDecimal)
          case (_, false) => IntType(itvl, NumberDecimal)
        }
        (t, LogicUnresolved)
      case (null, r) => 
        val t = isHw match {
          case true => BoolType(itvl, signed)
          case false => throwParserError(ctx)
        }
        (t, getResolution(r))
      case _ => throwParserError(ctx)
    }
  }
  
  private def getNonIntegerType(ctx: Non_integer_typeContext): (Type, LogicResolution) = {
    unsupported.raiseIt(ctx, s"Unsupported type ${getRawText(ctx)}")
    (UnknownType(ctx.getSourceInterval), LogicUnresolved)
  }
  
  
  private def getDataPrimitiveType(ctx: Data_type_primitiveContext, isHw: Boolean): (Type, LogicResolution) = {
    (ctx.integer_type, ctx.non_integer_type) match {
      case (i, null) => getIntegerType(i, isSigned(ctx.signing), isHw)
      case (null, n) => getNonIntegerType(n)
      case _ => throwParserError(ctx)
    }
  }
  
  private def unsupportedType(ctx: ParserRuleContext, str: String): (Type, LogicResolution) = {
    unsupported.raiseIt(ctx, s"Unsupported type: $str within context ${getRawText(ctx)}")
    (UnknownType(), LogicUnresolved)
  }
  
  private def getStructUnionMember(ctx: Struct_union_memberContext): Field = {
    val attr = visitAttributes(ctx.attribute_instance.asScala)
    (ctx.random_qualifier) match {
      case null => // ok
      case _ => unsupported.raiseIt(ctx, "random qualifier is not supported")
    }
    val (tpe, res) = (ctx.data_type_or_void.KW_VOID, ctx.data_type_or_void.data_type) match {
      case (null, dtp) => getDataType(dtp, true)
      case (v, null) => unsupportedType(ctx, "void in bundle field")
      case _ => throwParserError(ctx)
    }
    
    val l = ctx.list_of_variable_decl_assignments.variable_decl_assignment
      .asScala.map(d => visitVariable_decl_assignment(d, tpe, res, attr))
    
    l match {
      case Seq(l) => Field(ctx.getSourceInterval, attr, l.name, Default, l.tpe)
      case _ => 
        unsupported.raiseIt(ctx, "Unsupported list of variable declaration for bundle field")
        Field(ctx.getSourceInterval, attr, "???", Default, UnknownType())
    }
  }
  
  private def getDataTypeUsual(ctx: Data_type_usualContext, isHw: Boolean): (Type, LogicResolution) = {
    trace(ctx, s"getDataTypeUsual ${getRawText(ctx)}")
    val dtp = ctx.data_type_primitive
    val enum = ctx.enum_name_declaration.asScala
    val struct = ctx.struct_union
    val pack = ctx.package_or_class_scoped_path
      
    val (tpe, res) = (dtp, enum, struct, pack) match {
      case (p, Seq(), null, null) => getDataPrimitiveType(p, isHw)
      case (null, s, null, null) => unsupportedType(ctx, "TO DO enum")
      case (null, Seq(), s, null) => 
        s.getText match {
          case "struct" => 
            if(ctx.KW_PACKED == null) info(ctx, "All struct declaration are treated as packed")
            if(ctx.signing != null) unsupported.raiseIt(ctx, "Signed typedef")
            val fields = ctx.struct_union_member.asScala.map(getStructUnionMember)
            (BundleType(ctx.getSourceInterval, fields), LogicUnresolved)
            
          case "union" => unsupportedType(ctx, "union")
        }
        
      case (null, Seq(), null, p) => (getPath(p).asUserRefType(), LogicUnresolved)
      case _ => throwParserError(ctx)
    }
    // IMPORTANT NOTE: variable dim is to be applied to any result of this function !!
    val packed = ctx.variable_dimension.asScala.map(getVariableDim)
      .collect {case u: UnpackedVecType => u.asPackedVecType}
    (packed, tpe) match {
      case (Seq(), _) => (tpe, res)
      case (s, BoolType(_, true)) => (getFullType(s.dropRight(1), s.last.asSIntType()), res)
      case (s, _) => (getFullType(s, tpe), res)
    }
  }
  
  private def getDataType(ctx: Data_typeContext, isHw: Boolean) : (Type, LogicResolution) = {
    trace(ctx, s"getDataType ${getRawText(ctx)}")
    unsupported.check(ctx)
    (ctx.KW_STRING, ctx.data_type_usual, ctx.type_reference) match {
      case (s   , null, null) => 
        (StringType(s.getSourceInterval(), UnknownWidth()), LogicUnresolved)
        
      case (null, dtp, null) => getDataTypeUsual(dtp, isHw)
      
      case (null, null, tprf) => 
        (tprf.expression, tprf.data_type) match {
          case (e, null) => (getExpressionType(e), LogicUnresolved)
          case (null, d) => getDataType(d, isHw)
          case _ => throwParserError(ctx)
        }
        
      case _ => throwParserError(ctx)
    }
  }
  
  private def parseVerilogInt(str: String, i: Interval, wildComp: Option[Expression]=None): Expression = {
    // integral_number:
    //    BASED_NUMBER_WITH_SIZE
    //    | ( UNSIGNED_NUMBER )? ANY_BASED_NUMBER
    //    | UNSIGNED_NUMBER
    // ;
    // real_number:
    //    REAL_NUMBER_WITH_EXP
    //    | FIXED_POINT_NUMBER
    // ;
    // ANY_BASED_NUMBER:
    //  OCTAL_NUMBER 
    //   | DECIMAL_NUMBER_WITH_BASE 
    //   | BINARY_NUMBER 
    //   | DECIMAL_INVALID_NUMBER_WITH_BASE 
    //   | DECIMAL_TRISTATE_NUMBER_WITH_BASE 
    //   | HEX_NUMBER 
    //  ;
    val hexPattern: Regex = "([0-9]+)?'(?:x|X|h|H)\\s*([0-9_a-fA-FxXzZ?_]+)".r
    val octPattern: Regex = "([0-9]+)?'(?:o|O)\\s*([0-7xXzZ?_]+)".r
    val decPattern: Regex = "([0-9]+)?'(?:d|D)\\s*([0-9_]+)".r
    val binPattern: Regex = "([0-9]+)?'(?:b|B)\\s*([01xXzZ?_]+)".r
    val intPattern: Regex = "([0-9_]+)".r
    
    def getW(str: String): Width = {
      Width(BigInt(str, 10))
    }
    
    def safeNum(n: Number): Expression = {
      val maskB = ArrayBuffer[String]()
      val resB = ArrayBuffer[String]()
      var replacedXZ = false
      var replacedMark = false
      val maskBase = n.base match {
        case NumberDecimal => "" // raise error latter if used
        case NumberBinary => "1"
        case NumberOctal => "7"
        case NumberHexa => "F"
      }
      
      n.value.foreach(c => {
        c.toString match {
          case "_" => maskB += "_"; resB += "_"
          case "?" => maskB += "0"; resB += "0" ; replacedMark = true
          case "x" => maskB += "0"; resB += "0" ; replacedXZ = true
          case "X" => maskB += "0"; resB += "0" ; replacedXZ = true
          case "z" => maskB += "0"; resB += "0" ; replacedXZ = true
          case "Z" => maskB += "0"; resB += "0" ; replacedXZ = true
          case _ =>   maskB += maskBase; resB += c.toString 
        }
      })
      val mask = maskB.mkString
      val res = resB.mkString
      (replacedMark, replacedXZ, wildComp) match {
        case (m, r, Some(e)) if (m || r) => 
          if(maskBase == "") unsupported.raiseIt(i, "Unsupported use of mask with decimal base")
          MaskedNumber(n.tokens, n.copy(value = res), n.copy(value = mask))
        case (true, _, None) => 
          unsupported.raiseIt(i, "Unsupported use of quotation mark, replaced with '0'")
          n.copy(value = res)
        case (false, true, None) => 
          unsupported.raiseWarn(i, "Unsupported state 'X' or 'Z' used where chisel is only 2-state, replaced with '0'")
          n.copy(value = res)
        case _ => n
      }
    }
    
    ((((hexPattern.unapplySeq(str) match {
      case Some(Seq(null, value)) => Some(safeNum(Number(i, value, NumberHexa)))
      case Some(Seq(width, value)) => Some(safeNum(Number(i, value, getW(width), NumberHexa)))
      case _ => None
    }) match {
      case Some(e) => Some(e)
      case None => octPattern.unapplySeq(str) match {
        case Some(Seq(null, value)) => Some(safeNum(Number(i, value, NumberOctal)))
        case Some(Seq(width, value)) => Some(safeNum(Number(i, value, getW(width), NumberOctal)))
        case _ => None
      }
    }) match {
      case Some(e) => Some(e)
      case None => decPattern.unapplySeq(str) match {
        case Some(Seq(null, value)) => Some(Number(i, value, NumberDecimal))
        case Some(Seq(width, value)) => Some(Number(i, value, getW(width), NumberDecimal))
        case _ => None
      }
    }) match {
      case Some(e) => Some(e)
      case None => binPattern.unapplySeq(str) match {
        case Some(Seq(null, value)) => Some(safeNum(Number(i, value, NumberBinary)))
        case Some(Seq(width, value)) => Some(safeNum(Number(i, value, getW(width), NumberBinary)))
        case _ => None
      }
    }) match {
      case Some(e) => e // finally 
      case None => intPattern.unapplySeq(str) match {
        case Some(Seq(value)) => Number(i, value)
        case _ => 
          unsupported.raiseIt(i, s"Unsupported number: $str")
          UndefinedExpression(i)
      }
    }
  }
  
  private def getNumber(ctx: NumberContext, wildComp: Option[Expression]=None) : Expression = {
    // IMPORTANT NOTE:
    // parser is made such as there is no signed lit 
    // it is always primary minus + unsigned lit
    
    (ctx.integral_number, ctx.real_number) match {
      case (i, null) => parseVerilogInt(i.getText(), i.getSourceInterval, wildComp)
      case (null, r) => unsupportedExpr(ctx, "Fixed point literals")
      case _ => throwParserError(ctx)
    }
  }
  
  private def getPrimaryLiteral(ctx: Primary_literalContext, wildComp: Option[Expression]=None): Expression = {
    (ctx.time_lit,ctx.UNBASED_UNSIZED_LITERAL,ctx.STRING_LITERAL,ctx.number,ctx.KW_NULL,ctx.KW_THIS,ctx.DOLAR) match {
      case (l, null, null, null, null, null, null) => unsupportedExpr(ctx, "TIME_LITERAL")
      case (null, l, null, null, null, null, null) => FillingBitPattern(l.getSourceInterval(), l.getText(), UnknownExpressionKind, UnknownType())
      case (null, null, l, null, null, null, null) => 
        StringLit(ctx.getSourceInterval, ctx.getText().tail.dropRight(1))
      case (null, null, null, l, null, null, null) => getNumber(l, wildComp)
      case (null, null, null, null, l, null, null) => unsupportedExpr(ctx, "KW_NULL")
      case (null, null, null, null, null, l, null) => unsupportedExpr(ctx, "KW_THIS")
      case (null, null, null, null, null, null, l) => unsupportedExpr(ctx, "DOLAR")
      case _ => throwParserError(ctx)
    }
  }
  
  private def getPathItem(ctx: Package_or_class_scoped_path_itemContext) : Reference = {
    ctx.parameter_value_assignment match {
      case null => 
      case _ => unsupported.raiseIt(ctx, s"Unsupported parameter value assignment within expression path ${getRawText(ctx)}")
    }
    getRef(ctx.identifier)
  }
  
  private def getPath(ctx: Package_or_class_scoped_pathContext): Reference = {
    unsupported.check(ctx)
    val path = ctx.package_or_class_scoped_path_item.asScala.map(getPathItem).reverse
    val intv = ctx.getSourceInterval()
    path match {
      case Seq()  => throwParserError(ctx)
      case Seq(h) => h.copy(tokens = intv)
      case s   => s.head.copy(tokens = intv, path = s.tail.map(_.serialize))
    }
  }
  // assignment_pattern:
  //     APOSTROPHE_LBRACE (
  //         expression ( COMMA expression )*
  //         | structure_pattern_key COLON expression
  //            ( COMMA structure_pattern_key COLON expression )*
  //         | array_pattern_key COLON expression
  //            ( COMMA array_pattern_key COLON expression )*
  //         | constant_expression LBRACE expression ( COMMA expression )* RBRACE
  //     )? RBRACE;
  // structure_pattern_key:
  //  identifier
  //   | assignment_pattern_key
  // ;
  // array_pattern_key:
  //  constant_expression
  //   | assignment_pattern_key
  // ;
  // assignment_pattern_key:
  //  KW_DEFAULT
  //   | integer_type
  //   | non_integer_type
  //   | package_or_class_scoped_path
  
  // TODO : extends the IR to integrate a new branch with assignpattern 
  
  private def getAssignPatternKey(ctx: Assignment_pattern_keyContext): Expression = {
    (ctx.KW_DEFAULT, ctx.integer_type, ctx.non_integer_type, ctx.package_or_class_scoped_path) match {
      case (d, null, null, null) => DefaultAssignPattern(ctx.getSourceInterval())
      case (null, i, null, null) => unsupportedExpr(i, "integer type within assign pattern key")
      case (null, null, n, null) => unsupportedExpr(n, "non integer type within assign pattern key")
      case (null, null, null, p) => getPath(p)
      case _ => throwParserError(ctx)
    }
    
  }
  
  private def getStructPatternKey(ctx: Assignment_patternContext): Expression = {    
    val exprs = ctx.expression.asScala.map(getExpression(_))
    val assigns = ctx.structure_pattern_key.asScala.map(c => 
      (c.identifier, c.assignment_pattern_key) match {
        case (i, null) => getRef(i)
        case (null, a) => getAssignPatternKey(a)
        case _ => throwParserError(ctx)
      }
    )
    AssignPattern(ctx.getSourceInterval(),assigns.zip(exprs), UnknownExpressionKind, UnknownType())
  }
  
  private def getArrayPatternKey(ctx: Assignment_patternContext): Expression = {    
    val exprs = ctx.expression.asScala.map(getExpression(_))
    val assigns = ctx.array_pattern_key.asScala.map(c => 
      (c.constant_expression, c.assignment_pattern_key) match {
        case (e, null) => getExpression(e.expression)
        case (null, a) => getAssignPatternKey(a)
        case _ => throwParserError(ctx)
      }
    )
    AssignPattern(ctx.getSourceInterval(),assigns.zip(exprs), UnknownExpressionKind, UnknownType())
  }
  
  private def getAssignPattern(ctx: Assignment_pattern_expressionContext): Expression = {
    ctx.assignment_pattern_expression_type match {
      case null => 
      case tpe => unsupported.raiseIt(ctx, s"Unsupported assignement pattern type ${tpe.getText()} ")
    }
    val assig = ctx.assignment_pattern
    val exprs = assig.expression.asScala
    
    val struct = assig.structure_pattern_key.asScala
    val array = assig.array_pattern_key.asScala
    
    (assig.constant_expression, exprs, struct, array) match {
      case (c,   Seq(), Seq(), Seq()) => getExpression(c.expression)
      case (null, e,    Seq(), Seq()) => 
        AssignPattern(ctx.getSourceInterval(), e.map(c => (UndefinedExpression(),getExpression(c))), UnknownExpressionKind, UnknownType())
      case (null, e,    s,     Seq()) => getStructPatternKey(assig)
      case (null, e,    Seq(), s    ) => getArrayPatternKey(assig)
      case _ => throwParserError(ctx)
    }
    
  }
  // list_of_arguments:
  //     ( list_of_arguments_named_item
  //      | COMMA list_of_arguments_named_item
  //      | expression ( COMMA ( expression )? )*
  //      | ( COMMA ( expression )? )+
  //     )
  //     ( COMMA list_of_arguments_named_item )*;
  private def getCallArgs(ctx: List_of_argumentsContext): Seq[Expression] = {
    unsupported.check(ctx)
    ctx.expression.asScala.map(getExpression(_))
  }
  
  // | any_system_tf_identifier ( LPAREN data_type (COMMA list_of_arguments)?
  //      ( COMMA clocking_event )? RPAREN
  //      | LPAREN list_of_arguments ( COMMA clocking_event )?  RPAREN
  //      )?    
  private def getPrimaryTfCall(ctx: PrimaryTfCallContext): Expression = {
    unsupported.check(ctx)
    val value = (ctx.list_of_arguments, ctx.data_type) match {
      case (null, null) => Seq()
      case (l, null) => getCallArgs(ctx.list_of_arguments)
      case (null, d) => 
        // weird reference to single param as arg hidden within data_type
        d.data_type_usual match {
          case null => Seq(unsupportedExpr(d, "Data type context"))
          case dtp => dtp.package_or_class_scoped_path match {
            case null =>  Seq(unsupportedExpr(d, "Data type context"))
            case p => 
              val loc = getPath(p)
              val res = dtp.variable_dimension.asScala.map(d => getVariableDimSubRange(d, loc)) match {
                case Seq() => loc 
                case s => s.reduceRight((a, b) => a match {
                  case sr: SubRange => sr.copy(expr = b)
                  case si: SubIndex => si.copy(expr = b)
                  case _ => unsupportedExpr(ctx, "Should not happen")
                })
              }
              Seq(res)    
          }
        }
        
      case _ => Seq(unsupportedExpr(ctx, "Weird primary call"))
    }
    
    ctx.any_system_tf_identifier.getText() match {
      case "$anyseq" => DontCare(ctx.getSourceInterval)
      case "$anyconst" => DontCare(ctx.getSourceInterval)
        
      case "$clog2" => 
        val primop = CeilLog2(ctx.any_system_tf_identifier.getSourceInterval())
        DoPrim(ctx.getSourceInterval, primop, value)
        
      case "$bits" =>
        val primop = GetWidth(ctx.any_system_tf_identifier.getSourceInterval())
        DoPrim(ctx.getSourceInterval, primop, value)
        
      case "$signed" =>
        val tpe = SIntType(ctx.any_system_tf_identifier.getSourceInterval(), UnknownWidth())
        value match {
          case Seq(a) => DoCast(ctx.getSourceInterval, a, HwExpressionKind, tpe)
          case _ => unsupportedExpr(ctx, "$signed requires exactly one argument")
        }
        
      case "$unsigned" =>
        val tpe = UIntType(ctx.any_system_tf_identifier.getSourceInterval(), UnknownWidth(), NumberDecimal)
        value match {
          case Seq(a) => DoCast(ctx.getSourceInterval, a, HwExpressionKind, tpe)
          case _ => unsupportedExpr(ctx, "$unsigned requires exactly one argument")
        }
      
      case _ => unsupportedExpr(ctx, "Unsupported PrimaryTfCall")
    }
  }
  
  private def getBitSelect(ctx: PrimaryBitSelectContext): Expression = {
    val expr = getPrimary(ctx.primary)
    val index = getExpression(ctx.bit_select.expression)
    SubIndex(ctx.getSourceInterval, expr, index, UnknownExpressionKind, UnknownType())
  }
  
  private def getConcatenation(ctx: ConcatenationContext): Expression = {
    val i = ctx.getSourceInterval
    val exp = ctx.expression.asScala.map(getExpression(_))
    ctx.concatenation match {
      case null => Concat(i, exp, uk, ut)
      case c => 
        getConcatenation(c) match {
          case Concat(_, Seq(e), _, _, _) => ReplicatePattern(i, exp.head, e, uk, ut)
          case concat => ReplicatePattern(i, exp.head, concat, uk, ut)
        }
    }
  }
  
  private def getArrayRangeExpr(ctx: Array_range_expressionContext, loc: Expression) : Expression = {
    val intv = ctx.getSourceInterval()
    ctx.expression.asScala.map(getExpression(_)) match {
      case Seq(i) => SubIndex(intv, loc, i, UnknownExpressionKind, UnknownType())
      case Seq(left, right) => 
        val (l,r) = getComplexRange(ctx.operator_plus_minus, left, right)
        SubRange(intv, loc, l, r, UnknownExpressionKind, UnknownType())
      case _ => throwParserError(ctx)
    }
  }
  
  private def getRange(ctx: PrimaryRangeContext): Expression = {
    getArrayRangeExpr(ctx.array_range_expression, getPrimary(ctx.primary))
  }
  
  private def getCast2(ctx: PrimaryCast2Context): Expression = {
    val expr = getExpression(ctx.expression)
    val tpe = getPrimary(ctx.primary) match {
      case r: Reference => r.asUserRefType()
      case _ => unsupportedType(ctx, "PrimaryCast2Context")._1
    }
    DoCast(ctx.getSourceInterval, expr, UnknownExpressionKind, tpe)
  }
  
  
  // | primary ( DOT array_method_name )? ( attribute_instance )*
  //               LPAREN ( list_of_arguments )? RPAREN
  //               ( KW_WITH LPAREN expression RPAREN )? #PrimaryCall
  private def getCall(ctx: PrimaryCallContext): Expression = {
    forbidExpressionAttributes(ctx.attribute_instance.asScala)
    unsupported.check(ctx)
    val ref = getPrimary(ctx.primary) match {
      case r: Reference => r 
      case _ => unsupportedExpr(ctx.primary, "function must be a reference") 
    }
    val args = ctx.list_of_arguments match {
      case null => Seq()
      case l => getCallArgs(l) 
    }
    DoCall(ctx.getSourceInterval, ref, args, UnknownExpressionKind)
  }
  
  private def getSubField(ctx: PrimaryDotContext): Expression = {
    val expr = getPrimary(ctx.primary)
    val tpe = UnknownType()
    SubField(ctx.getSourceInterval, expr, getRefText(ctx.identifier), UnknownExpressionKind, tpe)
  }
  
  private def getPrimary(ctx: PrimaryContext, wildComp: Option[Expression]=None): Expression = {
    ctx match {
      case p: PrimaryLitContext => getPrimaryLiteral(p.primary_literal, wildComp)
      case p: PrimaryPathContext => getPath(p.package_or_class_scoped_path)
      case p: PrimaryParContext => 
        val e = getMinTypMaxExpression(p.mintypmax_expression)
        val i = p.getSourceInterval
        DoPrim(i, Par(i), Seq(e), UnknownExpressionKind)
      case p: PrimaryTfCallContext => getPrimaryTfCall(p)
      case p: PrimaryAssigContext => getAssignPattern(p.assignment_pattern_expression)
      case p: PrimaryBitSelectContext => getBitSelect(p)
      case p: PrimaryConcatContext => getConcatenation(p.concatenation)
      case p: PrimaryRangeContext => getRange(p)
      case p: PrimaryCast2Context => getCast2(p)
      case p: PrimaryCallContext => getCall(p)
      case p: PrimaryDotContext => getSubField(p)
      
      case p: PrimaryCastContext => unsupportedExpr(p, "PrimaryCastContext")
      case p: PrimaryStreaming_concatenationContext => unsupportedExpr(p, "PrimaryStreaming_concatenationContext")
      case p: PrimaryRandomizeContext => unsupportedExpr(p, "PrimaryRandomizeContext")
      case p: PrimaryRandomize2Context => unsupportedExpr(p, "PrimaryRandomize2Context")
      case p: PrimaryTypeRefContext => unsupportedExpr(p, "PrimaryTypeRefContext")
      case p: PrimaryCallArrayMethodNoArgsContext => unsupportedExpr(p, "PrimaryCallArrayMethodNoArgsContext")
      case p: PrimaryCallWithContext => unsupportedExpr(p, "PrimaryCallWithContext")
      case _ => throwParserError(ctx)
    }
  }
  
  private def forbidExpressionAttributes(ctx: Seq[Attribute_instanceContext]): Unit = {
    ctx foreach { a => unsupported.raiseIt(a, s"Unsupported Attribute instance in expression : ${a.getText()}")}
  }
  
  private def unsupportedExpr(ctx: ParserRuleContext, str: String): Expression = {
    unsupported.raiseIt(ctx, s"Unsupported $str : ${getRawText(ctx)}");
    UndefinedExpression()
  }
  
  
  private def getSubIndexes(path: Expression, sb: Seq[Bit_selectContext]): Expression = {
    sb match {
      case Seq() => path
      case s => 
        s.map(bs => SubIndex(bs.getSourceInterval, path, getExpression(bs.expression), UnknownExpressionKind, UnknownType()))
        .reduceLeft((a,b) => b.copy(expr = a))
    }
    
  }
  
  private def updateLastIndexRef(e: Expression, last: Expression): Expression = {
    e match {
      case si: SubIndex => si.copy(expr = updateLastIndexRef(si.expr, last))
      case sf: SubField => sf.copy(expr = last)
      case e: Expression => last
    }
  }
  
  private def getSubFieldsSubIndexes(path: Expression, sf: Seq[Identifier_with_bit_selectContext]): Expression = {
    sf match {
      case Seq() => path
      case s =>
        s.map(ctx => {
          val field = getRefText(ctx.identifier)
          val expr = SubField(ctx.getSourceInterval, path, field, UnknownExpressionKind, UnknownType())
          getSubIndexes(expr, ctx.bit_select.asScala)
        }).reduceLeft((a,b) => updateLastIndexRef(b, a))
    }
  }
  
  // package_or_class_scoped_hier_id_with_select:
  //     package_or_class_scoped_path ( bit_select )* 
  //     ( DOT identifier_with_bit_select )* 
  //     ( LSQUARE_BR expression ( operator_plus_minus )? COLON expression RSQUARE_BR )?;
  
  private def getComplexRange(ctx: Operator_plus_minusContext, l: Expression, r: Expression): (Expression, Expression) = {
    val ui = UndefinedInterval
    ctx match {
      case null => (l, r)
      case op => 
        // logic [31: 0] a_vect;
        // a_vect[ 0 +: 8] // == a_vect[ 7 : 0]
        // a_vect[15 -: 8] // == a_vect[15 : 8]
        // logic [0 :31] b_vect;
        // b_vect[ 0 +: 8] // == b_vect[0 : 7]
        // b_vect[15 -: 8] // == b_vect[8 :15]
        lazy val r_minus_one = r match {
          case DoPrim(_,Add(_),Seq(e, Number(_, "1",_,_,_)),_,_) => e // remove trailing +1
          case n: Number => n.copy(value = s"${n.evalBigInt()-1}") // do the simple math
          case _ => DoPrim(ui, Sub(ui), Seq(r, Number(ui, "1"))) // default
        }
        lazy val r_plus_one = r match {
          case DoPrim(_,Sub(_),Seq(e, Number(_, "1",_,_,_)),_,_) => e // remove trailing -1
          case n: Number => n.copy(value = s"${n.evalBigInt()+1}") // do the simple math
          case _ => DoPrim(ui, Add(ui), Seq(r, Number(ui, "1"))) // default
        }
      
        (op.PLUS, op.MINUS, l, r) match {
        case (p, null, nl:Number, nr: Number) => (Number(ui, s"${nl.evalBigInt()+nr.evalBigInt()-1}"), l)
        case (p, null, _, _) => (DoPrim(ui, Add(ui), Seq(l, r_minus_one)), l)
        case (null, m, nl:Number, nr: Number) => (l, Number(ui, s"${nl.evalBigInt()-nr.evalBigInt()+1}"))
        case (null, m, _, _) => (l, DoPrim(ui, Sub(ui), Seq(l, r_plus_one)))
        case _ => throwParserError(ctx)
      }
    }
  }
  
  private def getVariablePath(ctx: Package_or_class_scoped_hier_id_with_selectContext): Expression = {
    val path = getPath(ctx.package_or_class_scoped_path)
    val bs = ctx.bit_select.asScala
    val id_bs = ctx.identifier_with_bit_select.asScala
    val range = ctx.expression.asScala.map(getExpression(_))
    
    val expr = getSubFieldsSubIndexes(getSubIndexes(path, bs), id_bs)
    
    range match {
      case Seq() => expr
      case Seq(left, right) => 
        val (l,r) = getComplexRange(ctx.operator_plus_minus, left, right)
        SubRange(ctx.getSourceInterval, expr, l, r, UnknownExpressionKind, UnknownType())
      case _ => throwParserError(ctx)
    }
  }
  
  //TO DO 
  // variable_lvalue:
  //  LBRACE variable_lvalue ( COMMA variable_lvalue )* RBRACE
  //   | package_or_class_scoped_hier_id_with_select
  //   | ( assignment_pattern_expression_type )? assignment_pattern_variable_lvalue
  //   | streaming_concatenation
  private def getVariableLvalue(ctx: Variable_lvalueContext) : Expression = {
    ctx match {
      case v: VarLConcatContext => 
        val exprs = v.variable_lvalue.asScala.map(getVariableLvalue)
        Concat(ctx.getSourceInterval, exprs, UnknownExpressionKind)

      case v: VarLPathContext =>
        getVariablePath(v.package_or_class_scoped_hier_id_with_select)
        
      case v: VarLAssignContext => unsupportedExpr(v, "VarLAssign")
      case v: VarLStreamConcatContext => unsupportedExpr(v, "VarLConcat")
      case _ => throwParserError(ctx)
    }
    
  }
  
  private def getIncrDecr(ctx: Inc_or_dec_expressionContext): Expression = {
    val (a,v,op,prefix) = ctx match {
      case c:Inc_or_dec_expressionPreContext => 
        (c.attribute_instance.asScala, c.variable_lvalue, c.inc_or_dec_operator, true)
      case c:Inc_or_dec_expressionPostContext => 
        (c.attribute_instance.asScala, c.variable_lvalue, c.inc_or_dec_operator, false)
    }
    forbidExpressionAttributes(a)
    
    val primop = (op.INCR, op.DECR) match {
      case (i, null) => Incr(i.getSourceInterval(),prefix)
      case (null, d) => Decr(d.getSourceInterval(),prefix)
      case _ => throwParserError(ctx)
    }
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    DoPrim(ctx.getSourceInterval, primop, Seq(getVariableLvalue(v)), kind, tpe)
  }
  
  private def getMulDivRem(ctx: ExpressionBinOpMulContext) : Expression = {
    forbidExpressionAttributes(ctx.attribute_instance.asScala)
    val op = ctx.operator_mul_div_mod
    val primop = (op.MUL, op.DIV, op.MOD) match {
      case (m, null, null) => Mul(m.getSourceInterval())
      case (null, d, null) => Div(d.getSourceInterval())
      case (null, null, r) => Rem(r.getSourceInterval())
      case _ => throwParserError(ctx)
    }
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    DoPrim(ctx.getSourceInterval, primop, ctx.expression.asScala.map(getExpression(_)), kind, tpe)
  }
  private def getShift(ctx: ExpressionBinOpShiftContext) : Expression = {
    forbidExpressionAttributes(ctx.attribute_instance.asScala)
    val op = ctx.operator_shift
    val primop = (op.SHIFT_LEFT, op.SHIFT_RIGHT, op.ARITH_SHIFT_LEFT, op.ARITH_SHIFT_RIGHT) match {
      case (s, null, null, null) => Shl(s.getSourceInterval())
      case (null, s, null, null) => LogShr(s.getSourceInterval())
      case (null, null, s, null) => Shl(s.getSourceInterval())
      case (null, null, null, s) => Shr(s.getSourceInterval())
      case _ => throwParserError(ctx)
    }
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    DoPrim(ctx.getSourceInterval, primop, ctx.expression.asScala.map(getExpression(_)), kind, tpe)
  }
  
  private def getAddSub(ctx: ExpressionBinOpAddContext) : Expression = {
    forbidExpressionAttributes(ctx.attribute_instance.asScala)
    val op = ctx.operator_plus_minus
    val primop = (op.PLUS, op.MINUS) match {
      case (a, null) => Add(a.getSourceInterval())
      case (null, s) => Sub(s.getSourceInterval())
      case _ => throwParserError(ctx)
    }
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    DoPrim(ctx.getSourceInterval, primop, ctx.expression.asScala.map(getExpression(_)), kind, tpe)
  }
  
  private def getPowerOp(ctx: ExpressionBinOpPowerContext): Expression = {
    forbidExpressionAttributes(ctx.attribute_instance.asScala)
    val primop = Pow(ctx.DOUBLESTAR.getSourceInterval())
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    DoPrim(ctx.getSourceInterval, primop, ctx.expression.asScala.map(getExpression(_)), kind, tpe)
  }
  
  private def getCmpOp(ctx: ExpressionBinOpCmpContext): Expression = {
    forbidExpressionAttributes(ctx.attribute_instance.asScala)
    val op = ctx.operator_cmp
    val primop = (op.LT, op.LE, op.GT, op.GE) match {
      case (lt, null, null, null) => Lt(lt.getSourceInterval())
      case (null, le, null, null) => Leq(le.getSourceInterval())
      case (null, null, gt, null) => Gt(gt.getSourceInterval())
      case (null, null, null, ge) => Geq(ge.getSourceInterval())
      case _ => throwParserError(ctx) 
    }
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    DoPrim(ctx.getSourceInterval, primop, ctx.expression.asScala.map(getExpression(_)), kind, tpe)
  }
  
  private def getEqOp(ctx: ExpressionBinOpEqContext): Expression = {
    forbidExpressionAttributes(ctx.attribute_instance.asScala)
    val op = ctx.operator_eq_neq
    val primop = (op.EQ,op.NEQ,op.CASE_EQ,op.CASE_NEQ,op.WILDCARD_EQ,op.WILDCARD_NEQ) match {
      case (eq, null, null, null, null, null) => Eq(eq.getSourceInterval())
      case (null, ne, null, null, null, null) => Neq(ne.getSourceInterval())
      case (null, null, eq, null, null, null) => Eq(eq.getSourceInterval())
      case (null, null, null, ne, null, null) => Neq(ne.getSourceInterval())
      case (null, null, null, null, eq, null) => Eq(eq.getSourceInterval())
      case (null, null, null, null, null, ne) => Neq(ne.getSourceInterval())
      case _ => throwParserError(ctx) 
    }
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    DoPrim(ctx.getSourceInterval, primop, ctx.expression.asScala.map(getExpression(_)), kind, tpe)
  }
  
  private def getTernaryOp(ctx: ExpressionTernaryContext): Expression = {
    forbidExpressionAttributes(ctx.attribute_instance.asScala)
    ctx.KW_MATCHES match {
      case null =>
      case k => unsupported.raiseIt(ctx, s"Unsupported keyword ${k.getText()}") 
    }
    
    val primop = InlineIf(ctx.QUESTIONMARK.getSourceInterval())
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    DoPrim(ctx.getSourceInterval, primop, ctx.expression.asScala.map(getExpression(_)), kind, tpe)
  }
  
  private def unsupportedUnary(o: TerminalNode): PrimOp = {
    unsupported.raiseIt(o, s"Unsupported unary operator: ${o.getText()}")
    Plus(o.getSourceInterval())
  }
  
  private def getUnaryOp(op: Unary_module_path_operatorContext): PrimOp = {
    (op.NOT,op.NEG,op.AMPERSAND,op.NAND,op.BAR,op.NOR,op.XOR,op.NXOR,op.XORN) match {
      case (o,null,null,null,null,null,null,null,null) => Not(o.getSourceInterval()) 
      case (null,o,null,null,null,null,null,null,null) => BitNeg(o.getSourceInterval()) 
      case (null,null,o,null,null,null,null,null,null) => AndRed(o.getSourceInterval()) // AMPERSAN
      case (null,null,null,o,null,null,null,null,null) => NandRed(o.getSourceInterval()) // NAND
      case (null,null,null,null,o,null,null,null,null) => OrRed(o.getSourceInterval()) // BAR
      case (null,null,null,null,null,o,null,null,null) => NorRed(o.getSourceInterval()) // NOR
      case (null,null,null,null,null,null,o,null,null) => XorRed(o.getSourceInterval()) // XOR
      case (null,null,null,null,null,null,null,o,null) => XnorRed(o.getSourceInterval()) // NXOR
      case (null,null,null,null,null,null,null,null,o) => unsupportedUnary(o) // XORN
      case _ => throwParserError(op)
    }
  }
  
  private def getUnaryOp(ctx: ExpressionUnaryContext): Expression = {
    val op = ctx.unary_operator
    val primop = (op.PLUS, op.MINUS, op.unary_module_path_operator) match {
      case (p, null, null) => Plus(p.getSourceInterval())
      case (null, m, null) => Minus(m.getSourceInterval())
      case (null, null, u) => getUnaryOp(u)
      case _ => throwParserError(ctx)
    }
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    DoPrim(ctx.getSourceInterval, primop, Seq(getPrimary(ctx.primary)), kind, tpe)
  }
  
  private def getExpression(ctx: ExpressionContext, wildComp: Option[Expression]=None): Expression = {
    val i = ctx.getSourceInterval()
    val kind = UnknownExpressionKind
    val tpe = UnknownType()
    ctx match {
      case e: ExpressionTernaryContext => getTernaryOp(e)
      case e: ExpressionPrimaryContext => getPrimary(e.primary, wildComp)
      case e: ExpressionIncDecContext => getIncrDecr(e.inc_or_dec_expression)
      case e: ExpressionBinOpPowerContext => getPowerOp(e)
      case e: ExpressionBinOpMulContext => getMulDivRem(e)
      case e: ExpressionBinOpAddContext => getAddSub(e)
      case e: ExpressionBinOpShiftContext => getShift(e)
      case e: ExpressionBinOpCmpContext => getCmpOp(e)
      case e: ExpressionBinOpEqContext => getEqOp(e)
      
      case e: ExpressionUnaryContext => getUnaryOp(e)
      case e: ExpressionBinOpBitAndContext => 
        DoPrim(i, BitAnd(e.AMPERSAND.getSourceInterval()), e.expression.asScala.map(getExpression(_)), kind, tpe)
      case e: ExpressionBinOpBitXorContext => 
        val opX = e.operator_xor
        val op = (opX.XOR,opX.NXOR,opX.XORN) match {
          case (x, null, null) => BitXor(opX.getSourceInterval())
          case (null, x, null) => BitXnor(opX.getSourceInterval())
          case (null, null, x) => 
            unsupported.raiseIt(opX, s"Unsupported XORN operator: ${opX.getText()}")
            BitXor(opX.getSourceInterval())
          case _ => throwParserError(ctx)
        }
        DoPrim(i, op, e.expression.asScala.map(getExpression(_)), kind, tpe)
        
      case e: ExpressionBinOpBitOrContext => 
        DoPrim(i, BitOr(e.BAR.getSourceInterval()), e.expression.asScala.map(getExpression(_)), kind, tpe)
      case e: ExpressionBinOpAndContext => 
        DoPrim(i, And(e.LOG_AND.getSourceInterval()), e.expression.asScala.map(getExpression(_)), kind, tpe)
      case e: ExpressionBinOpOrContext => DoPrim(i, Or(e.LOG_OR.getSourceInterval()), e.expression.asScala.map(getExpression(_)), kind, tpe)
      
      case e: ExpressionBinOpImplContext => unsupportedExpr(e, "operator imply") 
      case e: ExpressionAssignContext => unsupportedExpr(e, "assign expression")
      case e: ExpressionTaggedIdContext => unsupportedExpr(e, "tagged id expression")
      case e: ExpressionInsideContext => unsupportedExpr(e, "inside expression")
      case e: ExpressionTripleAndContext => unsupportedExpr(e, "triple and")
      
      case _ => throwParserError(ctx)
    }
  }
  
  private def getRangeExpression(ctx: Range_expressionContext): SomeVecType = {
    val intv = ctx.getSourceInterval()
    ctx match {
      case null => SomeVecType(intv, Seq(UnknownType()), UndefinedExpression(intv), false)
      case r => r.expression.asScala match {
        case Seq(h, l) => val (high, downto) = (h.getText(),l.getText()) match {
            case (e,"0") => (h, true)
            case ("0",e) => (l, false)
            case _ => unsupported.raiseIt(ctx, s"Packed Range Declaration must have one bound set to 0, got : ${getRawText(ctx)}"); (h, true) // this will cause trouble
          }
          val bound = getExpression(high)
          SomeVecType(intv, Seq(BoolType(intv)), bound, downto)
        case _ => unsupported.raiseIt(ctx, s"Packed Range Declaration must have exactly 2 values, got : ${getRawText(ctx)}"); SomeVecType(intv, Seq(UnknownType()), UndefinedExpression(intv), false)
      }
    }
  }
  
  private def getPackedDim(ctx: Packed_dimensionContext) : PackedVecType = {
    getRangeExpression(ctx.range_expression).asPackedVecType()
  }
  private def getUnpackedDim(ctx: Unpacked_dimensionContext) : UnpackedVecType = {
    getRangeExpression(ctx.range_expression).asUnpackedVecType()
  }
  
  private def getArrayRangeType(ctx: Array_range_expressionContext) : Type = {
    val intv = ctx.getSourceInterval()
    ctx.operator_plus_minus match {
      case null => 
      case o => unsupported.raiseIt(ctx, "Complex array range declaration are not supported.")
    }
    
    ctx.expression.asScala match {
      case Seq(l, h) => val (high, downto) = (l.getText(),h.getText()) match {
          case ("0",e) => (h, false)
          case (e,"0") => (l, true)
          case _ => unsupported.raiseIt(ctx, s"Unpacked Range Declaration must have one bound set to 0, got : ${getRawText(ctx)}"); (h, false) // this will cause trouble
        }
        val bound = getExpression(high)
        UnpackedVecType(intv, Seq(BoolType(intv)), bound, downto)
      case _ => unsupported.raiseIt(ctx, s"Unpacked Range Declaration must have exactly 2 values, got : ${getRawText(ctx)}"); UnknownType()
    }
   
  }
  
  private def getImplicitDataType(ctx: Implicit_data_typeContext) : Type = {
    def err(){
      unsupported.raiseIt(ctx, s"Unsupported signed implicit data type ${getRawText(ctx)}")
    }
    
    // NOTE : when signed is required convert vec of BoolType into SIntType
    // raise error when not feasible
    val signed = isSigned(ctx.signing)
    
    ctx.packed_dimension.asScala.map(getPackedDim) match {
      case Seq() => 
        if(isSigned(ctx.signing)) { err() }
        BoolType(ctx.getSourceInterval())
      case Seq(t) => if(signed) t.asSIntType() else t
      case Seq(p, t) => if(signed) p.copy(tpe = Seq(t.asSIntType())) else p.copy(tpe = Seq(t))
      case s     => 
        val ss = if(signed){
          s.last match {
            case v: VecType => 
              val last = s(s.size-2).copy(tpe = Seq(v.asSIntType()))
              s.dropRight(2) ++ Seq(last)
            case t => err(); s
          }
        } else {s}
        ss.reduceRight((a,b) => a.copy(tpe=Seq(b)))
    }
  }
  
  private def visitData_type_or_implicit(ctx: Data_type_or_implicitContext, isHw: Boolean): (Type, LogicResolution) = {
    ctx match {
      case null =>  unsupported.raiseIt(ctx, s"Excepted data type for null context"); (UnknownType(), LogicUnresolved)
      case t => (t.data_type, t.implicit_data_type) match {
        case (d, null) => getDataType(d, isHw)
        case (null, i) => (getImplicitDataType(i), LogicUnresolved)
        case _ => throwParserError(ctx)
      }
    }
  }
  
  private def visitTypeRes(ctx: Net_or_var_data_typeContext, isHw: Boolean) : (Type, LogicResolution) = {
    ctx match {
      case null => (BoolType(UndefinedInterval), LogicUnresolved)
      case c => unsupported.check(ctx)
        c.data_type_or_implicit match {
          case null => 
            unsupported.raiseIt(ctx, s"To investigate? weird Net_or_var_data_typeContext: `${getRawText(ctx)}`")
            (BoolType(UndefinedInterval), LogicUnresolved)
          case cdi => visitData_type_or_implicit(cdi, isHw)
        }
    }
  }
  private def visitTypeRes(ctx: Net_port_typeContext, isHw: Boolean) : (Type, LogicResolution) = {
    ctx match {
      case null => (BoolType(UndefinedInterval), LogicUnresolved)
      case c => unsupported.check(ctx)
        c.data_type_or_implicit match {
          case null => 
            unsupported.raiseIt(ctx, s"To investigate? weird Net_port_typeContext: `${getRawText(ctx)}`")
            (BoolType(UndefinedInterval), LogicUnresolved)
          case cdi => visitData_type_or_implicit(cdi, isHw)
        }
    }
  }
  
  
  private def getVariableDim(ctx: Variable_dimensionContext): Type = {
    unsupported.check(ctx)
    ctx.array_range_expression match {
      case null => UnknownType()
      case a => getArrayRangeType(a)
    }
    
  }
  
  private def getVariableDimSubRange(ctx: Variable_dimensionContext, loc: Expression): Expression = {
    unsupported.check(ctx)
    ctx.array_range_expression match {
      case null => unsupportedExpr(ctx, "not an array range expression")
      case a => getArrayRangeExpr(a, loc)
    }
    
  }
  
  private def getFullType(s: Seq[PackedVecType], tpe: Type)(implicit i1: DummyImplicit) : Type = {
    s match {
      case Seq() => tpe
      case s => s.updated(s.size-1, s.last.copy(tpe=Seq(tpe)))
        .reduceRight((a,b) => a.copy(tpe=Seq(b)))
    }
  }
  private def getFullType(s: Seq[UnpackedVecType], tpe: Type) : Type = {
    s match {
      case Seq() => tpe
      case s => s.updated(s.size-1, s.last.copy(tpe=Seq(tpe)))
        .reduceRight((a,b) => a.copy(tpe=Seq(b)))
    }
  }

  private def visitPortsDeclaration(ctx: List_of_port_declarationsContext): (Seq[Port], Seq[String]) = {
    ctx match {
      case null => (Seq(), Seq())
      case list =>
        // nonansi port listing prior inline declaration 
        // warning only here because this list of declared port name is purely informative
        val declaredPortNames = list.nonansi_port.asScala.map(p => {
          (p.identifier, p.nonansi_port__expr) match {
            case (null, expr) => expr.identifier_doted_index_at_end.asScala match {
              case Seq(id) => id.identifier.asScala match {
                  case Seq(name) => 
                    if(!id.range_expression.asScala.isEmpty) 
                      critical(ctx, s"NonAnsi port with unexpected range_expression");
                    name.getText()
                  case s => 
                    warn(ctx, s"NonAnsi port with unexpected identifier: ${s.map(_.getText()).mkString(".")}");
                    ""
                }
              case s => 
                warn(ctx, s"NonAnsi port with unexpected identifier: ${s.map(_.getText()).mkString(".")}");
                ""
            }
            case (name, null) => 
              warn(ctx, s"NonAnsi port with identifier only: ${name.getText()}");
              ""
            case (name, expr) => 
              warn(ctx, s"NonAnsi port with both identifier: ${name.getText()} and expr:${expr.getText()}");
              ""
            case _ => throwParserError(p)
          }
          
        })
        
        var previousDirection : Option[Direction] = None
        val ports = list.list_of_port_declarations_ansi_item.asScala.map(p => {
          val attr = visitAttributes(p.attribute_instance.asScala)
          val decl = p.ansi_port_declaration
          val direction = (decl.port_direction, previousDirection) match {
            case (null, None) => 
              unsupported.raiseIt(ctx, s"Unsupported undefined port direction for port ${getRawText(p)}. Defaulting to Input direction");
              Input(UndefinedInterval)
            case (null, Some(d)) => d.mapInterval(_ => UndefinedInterval)
            case (pd, _) => 
              (pd.KW_INPUT,pd.KW_OUTPUT,pd.KW_INOUT) match {
                case (d, null, null) => Input(d.getSourceInterval())
                case (null, d, null) => Output(d.getSourceInterval())
                case (null, null, d) => Inout(d.getSourceInterval())
                case _ => unsupported.raiseIt(ctx, s"Unsupported port direction ${pd}"); Input(pd.getSourceInterval())
              }
          }
          previousDirection = Some(direction)
          val (tpe, res) = visitTypeRes(decl.net_or_var_data_type, true)
          val resolution = res match {
            case LogicUnresolved => LogicWire // unresolved ports are wire by default
            // TO DO : remove this logic when inference pass will be implemented
            // Strategies to choose => warning / error / auto correct ..?
            case r => r
          }
          val name = decl.port_identifier.getText()
          
          val unpacked = decl.variable_dimension.asScala.map(getVariableDim)
            .collect {case u: UnpackedVecType => u}
          
          val fullType = getFullType(unpacked, tpe)
          
          (resolution, decl.constant_expression) match {
            case (_, null) => Port(p.getSourceInterval(), attr, name, direction, fullType, resolution)
            case (LogicRegister, a) => Port(p.getSourceInterval(), attr, name, direction, fullType, resolution, getExpression(a.expression))
            case _ => 
              unsupported.raiseIt(ctx, "Unexpected assign on a none register logic")
              Port(p.getSourceInterval(), attr, name, direction, fullType, resolution)
          }
        })
        (ports, declaredPortNames)
    }
  }
  
  private def visitNonansi_port_declaration(ctx: Nonansi_port_declarationContext): Statement = {
    val attr = visitAttributes(ctx.attribute_instance.asScala)
    
    val (direction, (tpe, resolution)) = (ctx.KW_INPUT,ctx.KW_OUTPUT,ctx.KW_INOUT) match {
      case (d, null, null) => (Input(d.getSourceInterval()), visitTypeRes(ctx.net_or_var_data_type, isHw = true))
      case (null, d, null) => (Output(d.getSourceInterval()), visitTypeRes(ctx.net_or_var_data_type, isHw = true))
      case (null, null, d) => (Inout(d.getSourceInterval()), visitTypeRes(ctx.net_port_type, isHw = true))
      case _ => 
        unsupported.raiseIt(ctx, s"Unsupported port declaration style")
        (Input(ctx.getSourceInterval()), (BoolType(UndefinedInterval), LogicUnresolved))
    }
    
    // IDs for INPUT and INOUT
    val portA = ctx.list_of_variable_identifiers match {
      case null => Seq()
      case l => l.list_of_variable_identifiers_item.asScala.map(id => {
        val name = id.identifier.getText()
        
        val unpacked = id.variable_dimension.asScala.map(getVariableDim)
          .collect {case u: UnpackedVecType => u}
        
        val fullType = getFullType(unpacked, tpe)
        
        Port(id.getSourceInterval(), attr, name, direction, fullType, resolution)
      })
    }
    // OUTPUTS
    val portB = ctx.list_of_variable_port_identifiers match {
      case null => Seq()
      case l => l.list_of_tf_variable_identifiers.list_of_tf_variable_identifiers_item.asScala.map(id => {
        val name = id.identifier.getText()
        
        val unpacked = id.variable_dimension.asScala.map(getVariableDim)
          .collect {case u: UnpackedVecType => u}
        
        val fullType = getFullType(unpacked, tpe)
        
        id.expression match {
          case null => Port(id.getSourceInterval(), attr, name, direction, fullType, resolution)
          case e => Port(id.getSourceInterval(), attr, name, direction, fullType, resolution, getExpression(e))
        }
      })
    }
    
    getStmtOrSimpleBlock(ctx, portA ++ portB)
  }
  
  private def getMinTypMaxExpression(ctx: Mintypmax_expressionContext): Expression = {
    ctx.expression.asScala match {
      case Seq() => UndefinedExpression(ctx.getSourceInterval())
      case Seq(e) => getExpression(e)
      case _ => unsupportedExpr(ctx, "Min-typ-max parameter expression")
    }
  }
  
  private def getParamExpression(ctx: Param_expressionContext): Expression = {
    (ctx.mintypmax_expression, ctx.data_type) match {
      case (e, null) => getMinTypMaxExpression(e)
      case (null, t) => unsupportedExpr(t, "data type in this context")
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitParam_assignment(ctx: Param_assignmentContext, t: Type, attr: VerilogAttributes) : DefParam = {
    val name = getRefText(ctx.identifier)
    val value : Option[Expression] = ctx.constant_param_expression match {
      case null => None
      case c => Some(getParamExpression(c.param_expression))
    }
    
    val (updatedTpe, kind) = (t, value) match {
      case (v: VecType, _) => 
        // NOTE: not sure to cover all cases here ...
        (v.bound.evalBigIntOption, value) match {
          case (Some(bg), _) if (bg == 0) => (BoolType(v.bound.tokens), SwExpressionKind)
          case (Some(bg), _) => (UIntType(v.bound.tokens, Width(bg+1), NumberDecimal), HwExpressionKind)
          case (_, Some(u:Number)) => (v.asUIntType(), HwExpressionKind)
          case (_, Some(u:UIntLiteral)) => (v.asUIntType(), HwExpressionKind)
          case _ => (BoolType(v.bound.tokens), HwExpressionKind)
        }
      case (_, Some(u:UIntLiteral)) =>(u.tpe, HwExpressionKind)
      case (_, Some(n:Number)) if(n.width != UnknownWidth()) =>
        (UIntType(n.tokens, n.width, n.base), HwExpressionKind)
      case _ => (t, SwExpressionKind)
    }
    trace(ctx, s"param $name: ${updatedTpe.serialize} $kind")
    
    val tpe = ctx.unpacked_dimension.asScala match {
      case Seq() => updatedTpe
      case s => getFullType(s.map(getUnpackedDim),updatedTpe)
    }

    DefParam(ctx.getSourceInterval(), attr, name, tpe, value, kind)
  }
  
  private def visitParamDeclPrim(ctx: Parameter_declaration_primitiveContext, attr: VerilogAttributes): Seq[DefParam] = {
    (ctx.KW_TYPE, ctx.list_of_param_assignments) match {
      case (t, null) => unsupported.raiseIt(ctx, "Unsupported type declaration"); Seq()
      case (null, l) =>
        val tpe = ctx.data_type_or_implicit match {
          case null => UnknownType() 
          case dt   => visitData_type_or_implicit(dt, false)._1
        }
        l.param_assignment.asScala.map(visitParam_assignment(_, tpe, attr))
        
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitParameter_declaration(ctx: Parameter_declarationContext, attr: VerilogAttributes): Seq[DefParam] = {
    visitParamDeclPrim(ctx.parameter_declaration_primitive, attr)
  }
  
  private def visitLocal_parameter_declaration(ctx: Local_parameter_declarationContext, attr: VerilogAttributes): Seq[DefParam] = {
    visitParamDeclPrim(ctx.parameter_declaration_primitive, attr)
  }
  
  
  private def visitParameterDeclaration(ctx: Parameter_port_declarationContext) : Seq[DefParam] = {
    // parameter_port_declaration:
    //     KW_TYPE list_of_type_assignments
    //     | parameter_declaration
    //     | local_parameter_declaration
    //     | data_type list_of_param_assignments
    // ;
    ctx match {
      case p: ParamPortTypeContext => unsupported.raiseIt(ctx, "Unsupported type declaration"); Seq()
      case p: ParamSimpleContext => visitParameter_declaration(p.parameter_declaration, NoVerilogAttribute)
      case p: ParamLocalContext => visitLocal_parameter_declaration(p.local_parameter_declaration, NoVerilogAttribute)
      case p: ParamAssignContext => 
        val tpe = p.data_type match {
          case null => UnknownType()
          case d => getDataType(d, false)._1
        }
        p.list_of_param_assignments.param_assignment.asScala.map(visitParam_assignment(_, tpe, NoVerilogAttribute))
    }
  }
  
  private def visitParams(ctx: Parameter_port_listContext) : Seq[DefParam] = {
    ctx match {
      case null => Seq()
      case _ =>
        (ctx.list_of_param_assignments, ctx.parameter_port_declaration.asScala) match {
          case (l, Seq()) => l.param_assignment.asScala.map(visitParam_assignment(_, UnknownType(), NoVerilogAttribute))
          case (null, s) => s.flatMap(visitParameterDeclaration)
          case _ => throwParserError(ctx)
        }
    }
    
  }
  
  private def unsupportedStmt(ctx: ParserRuleContext, str: String): Statement = {
    unsupported.raiseIt(ctx, s"Unsupported Statement: ${str} - ${getRawText(ctx)}")
    EmptyStmt
  }
  
  private def visitCoreItemParam(ctx: CoreItemParamContext, attr: VerilogAttributes): Statement = {
    unsupported.check(ctx)
    val s = (ctx.local_parameter_declaration, ctx.parameter_declaration) match {
      case (l, null) => visitLocal_parameter_declaration(l, attr)
      case (null, p) => visitParameter_declaration(p, attr)
      case _ => Seq(unsupportedStmt(ctx, "CoreItemParamContext"))
    }
    getStmtOrSimpleBlock(ctx, s)
  }
  
  
  private def visitNet_decl_assignment(ctx: Net_decl_assignmentContext, tpe: Type, attr: VerilogAttributes): DefLogic = {
    val name = getRefText(ctx.identifier)
    val fullType = getFullType(ctx.unpacked_dimension.asScala.map(getUnpackedDim),tpe)
    
    val u = UndefinedExpression()
    val init = ctx.expression match {
      case null => u
      case e    => getExpression(e)
    }
      
    DefLogic(ctx.getSourceInterval(), attr, name, fullType, u, u, init, LogicWire)
  }
  
  
  private def visitNet_declaration(ctx: Net_declarationContext, attr: VerilogAttributes): Statement = {
    unsupported.check(ctx) // includes net_type check
    // First get type & logic resolution (common to all upcoming elements)
    val dtp = ctx.data_type_or_implicit
    val userTpe = ctx.identifier.asScala
    
    val tpe = (dtp, userTpe) match {
      case (null, Seq()) => BoolType(UndefinedInterval)
      case (null, Seq(id)) => UserRefType(id.getSourceInterval, id.getText(), Seq())
      case (c   , Seq()) => visitData_type_or_implicit(c, true)._1
      case _ => unsupportedType(ctx, "complex declaration")._1
    }
    
    // Second get respective name & potential assignments 
    val s = ctx.list_of_net_decl_assignments.net_decl_assignment.asScala
      .map(c => visitNet_decl_assignment(c,tpe,attr)) 
    getStmtOrSimpleBlock(ctx, s)
  }
  
  private def visitVariable_decl_assignment(ctx: Variable_decl_assignmentContext, tpe: Type, res: LogicResolution, attr: VerilogAttributes): DefLogic = {
    unsupported.check(ctx)
    
    val name = getRefText(ctx.identifier)
    
    val unpacked = ctx.variable_dimension.asScala.map(getVariableDim)
      .collect {case u: UnpackedVecType => u}
    val fullType = getFullType(unpacked, tpe)
    
    val u = UndefinedExpression()  
    val init = ctx.expression match {
      case null => u
      case e => getExpression(e)
    }
  
    DefLogic(ctx.getSourceInterval(), attr, name, fullType, u, u, init, res)
  }
  
  private def visitType_declaration(ctx: Type_declarationContext, attr: VerilogAttributes): Statement = {
    ctx.data_type match {
      case null => // barely used old-C-style of typedef in 2 statements (this would be the second, actual typedef) 
        ctx.getChild(2) match {
          case i: IdentifierContext => unsupportedStmt(ctx, "TO DO simple type alias")
          case o: Identifier_with_bit_selectContext => unsupportedStmt(ctx, "TO DO type alias")
          case _ =>
            (ctx.KW_ENUM, ctx.KW_STRUCT, ctx.KW_UNION, ctx.KW_CLASS) match {
              case (e, null, null, null) => unsupportedStmt(ctx, "TO DO enum")
              case (null, s, null, null) => unsupportedStmt(ctx, "TO DO struct")
              case (null, null, u, null) => unsupportedStmt(ctx, "typedef union")
              case (null, null, null, c) => unsupportedStmt(ctx, "typedef class")
              case _ => throwParserError(ctx)
            }
        }
      case dt => // all typedef combined with actual type defition
        val (tpe, _) = getDataType(dt, isHw = true) // assuming hardware (could be questioned)
        val id = ctx.identifier.asScala match {
          case Seq(i) => i.getText
          case _ => throwParserError(ctx)
        }
        if(!ctx.variable_dimension.asScala.isEmpty) 
          unsupported.raiseIt(ctx, s"Unsupported variable_dimension within context ${getRawText(ctx)}")
          
        DefType(ctx.getSourceInterval, attr, id, tpe)
    }
  }
  
  private def visitData_declaration(ctx: Data_declarationContext, attr: VerilogAttributes): Statement = {
    unsupported.check(ctx)
    val dt = ctx.data_type_or_implicit
    (dt, ctx.type_declaration, ctx.package_import_declaration, ctx.net_type_declaration) match {
      case (null, null, null, td) => unsupportedStmt(td, "net_type_declaration")
      case (null, null, pi, null) => visitPackageImport(pi)
      case (null, td, null, null) => visitType_declaration(td, attr)
      case (dt, null, null, null) =>
        val (tpe, res) = visitData_type_or_implicit(dt, true)

        val s = ctx.list_of_variable_decl_assignments.variable_decl_assignment
          .asScala.map(d => visitVariable_decl_assignment(d, tpe, res, attr)) 
        getStmtOrSimpleBlock(ctx, s)
        
      case _ => throwParserError(ctx)
    }
    
  }
  
  private def getStmtOrSimpleBlock(ctx: ParserRuleContext, s: Seq[Statement]): Statement = {
    s match {
      case Seq() => EmptyStmt
      case Seq(e) => e.mapInterval((i: Interval) => ctx.getSourceInterval())
      case s => SimpleBlock(ctx.getSourceInterval, s)
    }
  }
  
  private def visitVariable_assignment(ctx: Variable_assignmentContext, attr: VerilogAttributes): Statement = {
    val loc = getVariableLvalue(ctx.variable_lvalue)
    val expr = getExpression(ctx.expression)
    Connect(ctx.getSourceInterval(), attr, loc, expr, true, false)
  }

  private def visitContinuous_assign(ctx: Continuous_assignContext, attr: VerilogAttributes): Statement = {
    unsupported.check(ctx)
    val s = ctx.list_of_variable_assignments.variable_assignment.asScala
      .map(v => visitVariable_assignment(v, attr))
    getStmtOrSimpleBlock(ctx, s)
  }
  
  
  private def unsupportedEvent(ctx: ParserRuleContext, str: String): EventControl = {
    unsupported.raiseIt(ctx, s"Unsupported event within context ${getRawText(ctx)}")
    UndefinedEventControl(ctx.getSourceInterval())
  }
  
  private def getEventExpressionItem(ctx: Event_expression_itemContext): EventControl = {
    (ctx.event_expression, ctx.expression.asScala) match {
      case (e, Seq()) => getEventExpression(e)
      case (null, Seq(e)) => 
        val i = ctx.getSourceInterval()
        val expr = getExpression(e)
        ctx.edge_identifier match {
          case null => AlwaysComb(i, false, expr)
          case e => 
            (e.KW_POSEDGE, e.KW_NEGEDGE, e.KW_EDGE) match {
              case (d, null, null) => AlwaysFF(i, true, expr)
              case (null, d, null) => AlwaysFF(i, false, expr)
              case (null, null, d) => unsupportedEvent(ctx,"keyword edge")
              case _ => throwParserError(ctx) 
            }
        }
      case (null, s) => unsupportedEvent(ctx, "multiple expression with IFF keyword")
      case _ => throwParserError(ctx) 
    }
  }
  
  private def getEventExpression(ctx: Event_expressionContext): EventControl = {
    ctx.event_expression_item.asScala.map(getEventExpressionItem) match {
      case Seq() => unsupportedEvent(ctx,"null event_expression")
      case Seq(e) => e 
      case _ => unsupportedEvent(ctx,"multiple event_expression")
    }
  }
  
  private def getEventControl(ctx: Event_controlContext): EventControl = {
    val expr = ctx.event_expression
    val star = ctx.MUL
    val pack = ctx.package_or_class_scoped_hier_id_with_select
    (pack, expr, star) match {
      case (p, null, null) => getVariablePath(p) ; unsupportedEvent(p, "path context")
      case (null, e, null) => getEventExpression(e)
      case (null, null, s) => AlwaysComb(ctx.getSourceInterval())
      case _ => throwParserError(ctx)
    }
  }
  
  private def getProcTimingControl(ctx: Procedural_timing_controlContext): EventControl = {
    (ctx.delay_control, ctx.event_control, ctx.cycle_delay, ctx.cycle_delay_range) match {
      case (c, null, null, null) => unsupportedEvent(c,"delay_control");
      case (null, c, null, null) => getEventControl(c)
      case (null, null, c, null) => unsupportedEvent(c,"cycle_delay")
      case (null, null, null, c) => unsupportedEvent(c,"cycle_delay_range")
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitStatement_or_null(ctx: Statement_or_nullContext): Statement = {
    ctx.statement match {
      case null => unsupportedStmt(ctx,"null statement")
      case s => visitStatement(s)
    }
  }
  
  private def visitAlwaysProc(ctx: Procedural_timing_control_statementContext, attr: VerilogAttributes): Statement = {
    val event = getProcTimingControl(ctx.procedural_timing_control)
    val s = visitStatement_or_null(ctx.statement_or_null)

    val i = ctx.getSourceInterval()
    
    (event, s) match {
      case (UndefinedEventControl(_), st) => st.mapInterval(_ => i)
      case (AlwaysFF(_,pos,clock), SimpleBlock(_,stmts, n)) => ClockRegion(i, stmts, clock, pos, n)
      case (AlwaysFF(_,pos,clock), st) => ClockRegion(i, Seq(st), clock, pos)
      case (AlwaysComb(_,all,trig), st) => 
        if(!all) unsupported.raiseIt(ctx, s"Unsupported combinational block triggered on ${trig.serialize} in ctx: ${getRawText(ctx)}")
        st.mapInterval(_ => i)
      case _ => unsupportedStmt(ctx, s"$event - $s")
    }
  }
  
  private def visitBlock_item_declaration(ctx: Block_item_declarationContext): Statement = {
    val attr = visitAttributes(ctx.attribute_instance.asScala)
    val s = (ctx.data_declaration, ctx.local_parameter_declaration, ctx.parameter_declaration, ctx.let_declaration) match {
      case (d, null, null, null) => Seq(visitData_declaration(d, attr))
      case (null, l, null, null) => visitLocal_parameter_declaration(l, attr)
      case (null, null, p, null) => visitParameter_declaration(p, attr)
      case (null, null, null, l) => Seq(unsupportedStmt(l, "let_declaration"))
      case _ => throwParserError(ctx)
    }
    getStmtOrSimpleBlock(ctx, s)
  }

  private def visitSeq_block(ctx: Seq_blockContext, attr: VerilogAttributes): Statement = {
    val name = ctx.identifier.asScala match {
      case Seq() => ""
      case _ => unsupported.raiseIt(ctx, ">>> TODO add name management to blocks") ; ""
    }
    
    val items = ctx.block_item_declaration.asScala.map(visitBlock_item_declaration)
    val stmts = items ++ ctx.statement_or_null.asScala.map(visitStatement_or_null)
    
    SimpleBlock(ctx.getSourceInterval(),stmts)
  }
  
  
  
  private def visitOperator_assignment(ctx: Operator_assignmentContext, attr: VerilogAttributes): Statement = {
    ctx.assignment_operator.ASSIGN match {
      case null => unsupportedStmt(ctx, "complex assign")
      case a => // only simple assign supported for now
    }
    val loc = getVariableLvalue(ctx.variable_lvalue)
    val expr = getExpression(ctx.expression)
    Connect(ctx.getSourceInterval, attr, loc, expr, false, true)
  }
  
  private def visitBlocking_assignment(ctx: Blocking_assignmentContext, attr: VerilogAttributes): Statement = {
    // unsupported.check(ctx)
    val pack = ctx.package_or_class_scoped_hier_id_with_select
    val op = ctx.operator_assignment
    (ctx.variable_lvalue, pack, op) match {
      case (v, null, null) => 
        ctx.delay_or_event_control match {
          case null =>
          case d    => unsupportedStmt(d, "delay_or_event_control") 
        }
        val loc = getVariableLvalue(ctx.variable_lvalue)
        val expr = getExpression(ctx.expression)
        Connect(ctx.getSourceInterval, attr, loc, expr, false, true)
        
      case (null, pack, null) => unsupportedStmt(ctx, "class new")
      case (null, null, op)   => visitOperator_assignment(op, attr)
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitNonblocking_assignment(ctx: Nonblocking_assignmentContext, attr: VerilogAttributes): Statement = {
    ctx.delay_or_event_control match {
      case null =>
      case d    => unsupportedStmt(d, "delay_or_event_control") 
    }
    val loc = getVariableLvalue(ctx.variable_lvalue)
    val expr = getExpression(ctx.expression)
    Connect(ctx.getSourceInterval, attr, loc, expr, false, false)
    
  }
  
  private def getPrimaryTfCallStmt(ctx: PrimaryTfCallContext, attr: VerilogAttributes): Statement = {
    unsupported.check(ctx)
    val args = (ctx.list_of_arguments, ctx.data_type) match {
      case (l, null) => getCallArgs(ctx.list_of_arguments)
      case (null, d) => 
        // weird reference to single param as arg hidden within data_type
        d.data_type_usual match {
          case null => Seq(unsupportedExpr(d, "Data type context"))
          case dtp => dtp.package_or_class_scoped_path match {
            case null =>  Seq(unsupportedExpr(d, "Data type context"))
            case p => Seq(getPath(p))
          }
        }
        
      case _ => throwParserError(ctx)
    }
     
    val str = ctx.any_system_tf_identifier.getText() match {
      case "$display" => ""
      case "$info" => "[info] "
      case "$warning" => "[warning] "
      case "$error" => "[error] "
      case "$fatal" => "[fatal] "
      case _ =>  "[unsupported]"
    }
    
    val intv = ctx.getSourceInterval
    val (stri, exprs) = args match {
      case Seq() => (StringLit(UndefinedInterval, s"$str"), Seq())
      case s => 
      s.head match {
        case sl: StringLit => (StringLit(s.head.tokens, s"$str${sl.string}"), s.tail)
        case _ => (StringLit(s.head.tokens, s"$str${s.head.serialize}"), s.tail)
      }
      
    }
    val u = UndefinedExpression()
    val p = Print(intv, attr, stri, exprs)
    str match {
      case "[unsupported]" => unsupportedStmt(ctx, "Unsupported PrimaryTfCall Statement");
      case "[fatal]" =>
        val stmts = Seq(p, Stop(intv, attr, args.headOption.getOrElse(u), u, u))
        SimpleBlock(intv, stmts)
      case _ => p
    }
  }
  
  private def visitPrimarySmt(ctx: PrimaryContext, attr: VerilogAttributes): Statement = {
    ctx match {
      case c: PrimaryTfCallContext => getPrimaryTfCallStmt(c, attr)
      case _ => unsupportedStmt(ctx, "primary as statement")
    }
  }
  
  private def visitStmtItemMain(ctx: StmtItemMainContext, attr: VerilogAttributes): Statement = {
    ctx.getChild(0) match {
      case b: Blocking_assignmentContext => visitBlocking_assignment(b, attr)
      case n: Nonblocking_assignmentContext => visitNonblocking_assignment(n, attr)
      case p: PrimaryContext => visitPrimarySmt(p, attr)
      
      case m: Macro_statementContext => unsupportedComment(m, "TODO Macro_statement")
      
      case p: Procedural_continuous_assignmentContext => unsupportedStmt(p, "Procedural_continuous_assignment")
      case i: Inc_or_dec_expressionContext => unsupportedStmt(i, "Inc_or_dec_expression")
      case c: Clocking_driveContext => unsupportedStmt(c, "Clocking_drive")
      case _ =>  throwParserError(ctx)
    }
  }
  
  private def visitCond_predicate(ctx: Cond_predicateContext): Expression = {
    unsupported.check(ctx)
    getExpression(ctx.expression.asScala.head)
  }
  
  private def visitConditional_statement(ctx: Conditional_statementContext, attr: VerilogAttributes): Statement = {
    ctx.unique_priority match {
      case null =>
      case _ => unsupported.raiseIt(ctx, s"Unsupported unique priority within context ${getRawText(ctx)}") 
    }
    
    val pred = visitCond_predicate(ctx.cond_predicate)
    val stmt = ctx.statement_or_null.asScala.map(visitStatement_or_null)
    val (conseq, alt) = stmt match {
      case Seq(s) => (s, EmptyStmt)
      case Seq(c, a) => (c, a)
      case _ => throwParserError(ctx)
    }
    Conditionally(ctx.getSourceInterval(),attr, pred, conseq, alt)
  }
  
  // case_item:
  //     ( KW_DEFAULT ( COLON )?
  //       | expression ( COMMA expression )* COLON
  //     ) statement_or_null;
  
  private def getCaseItem(ctx: Case_itemContext, comp: Expression): (Expression, Statement) = {
    val expr = (ctx.KW_DEFAULT, ctx.expression.asScala) match {
      case (null, Seq()) => throwParserError(ctx)
      case (null, Seq(e)) => getExpression(e, Some(comp))
      case (null, s) => s.map(getExpression(_, Some(comp))).reduce((a, b) => DoPrim(UndefinedInterval, PrimOps.Or(UndefinedInterval), Seq(a, b)))
      case (d, _) => DefaultAssignPattern(d.getSourceInterval)
    }
    
    (expr, visitStatement_or_null(ctx.statement_or_null))
  }
  
  private def visitStmtItemCase(ctx: Case_statementContext, attr: VerilogAttributes): Statement = {
    unsupported.check(ctx)
    def getCondStack(e: Expression, items: Seq[(Expression, Statement)], last: Statement): Conditionally = {
      val cond = items.map(t => { 
        val (lhs, rhs) = t._1 match {
          case MaskedNumber(_, num, mask) => 
            (DoPrim(UndefinedInterval, PrimOps.BitXor(UndefinedInterval), Seq(e, mask)), num)
          case _ => (e, t._1)
        }
        val exp = DoPrim(UndefinedInterval, PrimOps.Eq(UndefinedInterval), Seq(lhs, rhs))
        Conditionally(t._1.tokens, NoVerilogAttribute, exp, t._2, last)
      })
      val res = cond.reduceRight((a, b) => a.copy(alt = b))
      res
    }
    
    (ctx.expression, ctx.case_item) match {
      case (null, _) => unsupportedStmt(ctx, "Malformed case statement")
      case (_, null) => unsupportedStmt(ctx, "Malformed case statement")
      case (e, i) => 
        val expr = getExpression(e)
        val items = i.asScala.map(getCaseItem(_, expr))
        items.collect{ case (_:DefaultAssignPattern, s) => s} match {
          case Seq() => 
            // Do not use Switch for now (more complicated update to legalize expression required)
            // Switch(ctx.getSourceInterval, attr, expr, items, EmptyStmt)
            getCondStack(expr, items, EmptyStmt).copy(tokens = ctx.getSourceInterval, attributes = attr)
          case Seq(s) => 
            val conds = items.filterNot(t => t._1.isInstanceOf[DefaultAssignPattern])
            // Do not use Switch for now (more complicated update to legalize expression required)
            // Switch(ctx.getSourceInterval, attr, expr, conds, s)
            getCondStack(expr, conds, s).copy(tokens = ctx.getSourceInterval, attributes = attr)
          case _ => unsupportedStmt(ctx, "Malformed case statement with multiple defaults")
        }
        
    }
  }
  
  private def getForInit(ctx: For_initializationContext): Expression = {
    ctx match {
      case null => unsupportedExpr(ctx, "Need a valid expression as for loop start condition")
      case _ =>
        (ctx.list_of_variable_assignments, ctx.for_variable_declaration.asScala) match {
          case (null, Seq()) => unsupportedExpr(ctx, "Unexpected null for loop initialization")
          case (null, Seq(v)) => unsupportedExpr(v, "TODO? for variable declaration")
          case (null, s) => unsupportedExpr(ctx, "Unexpected multiple for variable declaration in for loop initialization.")
          case (l, _) => 
            l.variable_assignment.asScala.map(v => {
              val loc = getVariableLvalue(v.variable_lvalue) match {
                case Reference(_, n, Seq(),_,_,_) => n
                case _ => 
                  unsupportedExpr(v.variable_lvalue, "Complex variable as for initialization value")
                  v.variable_lvalue.getText()
              }
              val expr = getExpression(v.expression)
              NamedAssign(v.getSourceInterval, loc, expr, SourceFlow)
            }) match {
              case Seq(v) => v
              case _ => unsupportedExpr(l, "Unexpected multiple for variable declaration in for loop initialization.")
            }
        }
        
    }
  }
  
  private def getForStep(ctx: For_stepContext): Expression = {
    ctx match {
      case null => unsupportedExpr(ctx, "Need a valid expression as for loop step condition")
      case _ =>
        ctx.sequence_match_item.asScala match {
          case Seq(s) => s.getChild(0) match {
            case o: Operator_assignmentContext => 
              o.assignment_operator.ASSIGN match {
                case null => unsupportedExpr(o, "complex assign resolved as simple assign")
                case a => // only simple assign supported for now
              }
              val loc = getVariableLvalue(o.variable_lvalue) match {
                case Reference(_, n, Seq(),_,_,_) => n
                case _ => 
                  unsupportedExpr(o.variable_lvalue, "Complex variable as for step value")
                  o.variable_lvalue.getText()
              }
              val expr = getExpression(o.expression)
              NamedAssign(ctx.getSourceInterval, loc, expr, SourceFlow)

            case e: ExpressionContext => getExpression(e)
          }
          case _ => unsupportedExpr(ctx, "Unexpected multiple operations as for loop step expression")
        }
    }
  }
  
  private def visitLoop_statement(ctx: Loop_statementContext, attr: VerilogAttributes): Statement = {
    unsupported.check(ctx)
    val init = getForInit(ctx.for_initialization)
    val stop = ctx.expression match {
      case null => unsupportedExpr(ctx, "Need a valid expression as for loop stop condition")
      case e => getExpression(e)
    }
    
    val step = getForStep(ctx.for_step)
    val stmt = visitStatement_or_null(ctx.statement_or_null)
    
    ForGen(ctx.getSourceInterval, attr, init, stop, step, stmt)
    
  }
  
  private def visitStatement_item(ctx: Statement_itemContext, attr: VerilogAttributes): Statement = {
    ctx match {
      case s: StmtItemProcTimeContext => visitAlwaysProc(s.procedural_timing_control_statement, attr)
      
      case s: StmtItemSeqContext => visitSeq_block(s.seq_block, attr)
      
      case s: StmtItemMainContext => visitStmtItemMain(s, attr)
      case s: StmtItemCondContext => visitConditional_statement(s.conditional_statement, attr)
      case s: StmtItemCaseContext => visitStmtItemCase(s.case_statement, attr)
      case s: StmtItemLoopContext => visitLoop_statement(s.loop_statement, attr)
      
      case s: StmtItemMacroContext => unsupportedComment(s, "TODO StmtItemMacro")
      case s: StmtItemProcAssertContext => unsupportedComment(s, "StmtItemProcAssert")
      
      case s: StmtItemSubCallContext => unsupportedStmt(s, "StmtItemSubCall")
      case s: StmtItemDisableContext => unsupportedStmt(s, "StmtItemDisable")
      case s: StmtItemEventContext => unsupportedStmt(s, "StmtItemEvent")
      case s: StmtItemJumpContext => unsupportedStmt(s, "StmtItemJump")
      case s: StmtItemParContext => unsupportedStmt(s, "StmtItemPar")
      case s: StmtItemWaitContext => unsupportedStmt(s, "StmtItemWait")
      case s: StmtItemRandseqContext => unsupportedStmt(s, "StmtItemRandseq")
      case s: StmtItemRandcaseContext => unsupportedStmt(s, "StmtItemRandcase")
      case s: StmtItemExpectContext => unsupportedStmt(s, "StmtItemExpect")
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitStatement(ctx: StatementContext): Statement = {
    ctx.identifier match {
      case null => 
      case s => unsupportedStmt(s, "identifier") 
    }
    
    val attr = visitAttributes(ctx.attribute_instance.asScala)
    visitStatement_item(ctx.statement_item, attr)
  }
  
  private def visitAlways_construct(ctx: Always_constructContext, attr: VerilogAttributes): Statement = {
    val kw = ctx.always_keyword
    val st = visitStatement(ctx.statement)
    (st, kw.KW_ALWAYS, kw.KW_ALWAYS_COMB, kw.KW_ALWAYS_LATCH, kw.KW_ALWAYS_FF) match {
      case (s: ClockRegion, null, null, null, a) => s // always ff must be clocked
      case (_, null, null, a, null) => // always latch must not be clocked
        if(st.isInstanceOf[ClockRegion]) unsupported.raiseIt(ctx, "Inconsistent use of always_latch keyword")
        st
      case (_, null, a, null, null) => // always comb must not be clocked
        if(st.isInstanceOf[ClockRegion]) unsupported.raiseIt(ctx, "Inconsistent use of always_latch keyword")
        st
      case (_, a, null, null, null) => st // Always can be anything
      case _ => unsupportedStmt(ctx, "always keyword does not match with following event control") ; st
    }
  }
  
  private def visitIf_generate_construct(ctx: If_generate_constructContext, attr: VerilogAttributes): Statement = {
    val pred = getExpression(ctx.constant_expression.expression)
    val (conseq, alt) = ctx.generate_item.asScala.map(visitGenerate_item) match {
      case Seq(c) => (c, EmptyStmt)
      case Seq(c, a) => (c, a)
    }
    IfGen(ctx.getSourceInterval, attr, pred, conseq, alt)
  }
  
  private def visitConditional_generate_construct(ctx: Conditional_generate_constructContext, attr: VerilogAttributes): Statement = {
    (ctx.if_generate_construct, ctx.case_generate_construct) match {
      case (i, null) => visitIf_generate_construct(i, attr)
      case (null, c) => unsupportedStmt(c, "case_generate_construct")
      case _ => throwParserError(ctx)
    }
    
  }
  
  private def getGenVarInit(ctx: Genvar_initializationContext): Expression = {
    val assign = getExpression(ctx.constant_expression.expression)
    NamedAssign(ctx.getSourceInterval, getRefText(ctx.identifier), assign, SourceFlow)
  }
  
  private def getRef(id: IdentifierContext): Reference = {
    unsupported.check(id)
    id.TICK_IDENTIFIER match {
      case null => Reference(id.getSourceInterval(), id.getText(), Seq())
      case _ => Reference(id.getSourceInterval(), id.getText().tail, Seq("`"))
    }
  }
  private def getRefText(id: IdentifierContext): String = {
    unsupported.check(id)
    id.TICK_IDENTIFIER match {
      case null => id.getText()
      case _ => 
        unsupported.raiseIt(id, "Unsupported tick (`) define in this context")
        id.getText().tail
    }
  }
  
  private def getGenVarIter(ctx: Genvar_iterationContext): Expression = {
    val i = ctx.getSourceInterval
    
    def getPrimop(op: Inc_or_dec_operatorContext, prefix: Boolean): PrimOp =
      (op.INCR, op.DECR) match {
      case (i, null) => Incr(op.getSourceInterval, prefix)
      case (null, d) => Decr(op.getSourceInterval, prefix)
      case _ => throwParserError(ctx)
    }
    
    ctx match {
      case g: GenIterPostfixContext => 
        g.inc_or_dec_operator match {
          case null =>
            val e = getExpression(g.genvar_expression.constant_expression.expression)
            NamedAssign(i, getRefText(g.identifier), e, UnknownFlow)
            
          case op => 
            DoPrim(i, getPrimop(g.inc_or_dec_operator, false), Seq(getRef(g.identifier)))
        }
      case g: GenIterPrefixContext  => 
        DoPrim(i, getPrimop(g.inc_or_dec_operator, true), Seq(getRef(g.identifier)))
    }
  }
  
  private def visitLoop_generate_construct(ctx: Loop_generate_constructContext, attr: VerilogAttributes): Statement = {
    val init = getGenVarInit(ctx.genvar_initialization)
    val stop = getExpression(ctx.genvar_expression.constant_expression.expression)
    val step = getGenVarIter(ctx.genvar_iteration)
    val stmt = visitGenerate_item(ctx.generate_item)
    
    ForGen(ctx.getSourceInterval, attr, init, stop, step, stmt)
  }
  
  private def getNamedParam(ctx: Named_parameter_assignmentContext): Assign = {
    val name = getRefText(ctx.identifier)
    val expr = getParamExpression(ctx.param_expression)
    NamedAssign(ctx.getSourceInterval,name, expr, SourceFlow)
  }
  
  private def visitParameter_value_assignment(ctx: Parameter_value_assignmentContext): Seq[Assign] = {
    ctx.list_of_parameter_value_assignments match {
      case null => Seq()
      case l =>
        val raw = l.param_expression.asScala
        val named = l.named_parameter_assignment.asScala
        (raw, named) match {
          case (Seq(), n) => n.map(getNamedParam)
          case (r, Seq()) => r.map(r => {
              NoNameAssign(r.getSourceInterval, getParamExpression(r), SourceFlow)
            })
          case _ => throwParserError(ctx)
        }
        
        
    }
  }
  
  private def getPortMap(ctx: List_of_port_connectionsContext): Seq[Assign] = {
    val ordered = ctx.ordered_port_connection.asScala.map {c =>
      forbidExpressionAttributes(c.attribute_instance.asScala)
      NoNameAssign(c.getSourceInterval,getExpression(c.expression), UnknownFlow)
    }
    
    val named = ctx.named_port_connection.asScala.map {c =>
      forbidExpressionAttributes(c.attribute_instance.asScala)
      val id = getRefText(c.identifier)
      val tokens = c.getSourceInterval()
      (c.MUL, c.identifier, c.expression) match {
        case (m, null, null) => AutoAssign(tokens)
        case (null, i, null) => NamedAssign(tokens, id, UndefinedExpression(), UnknownFlow)
        case (null, i, e) => NamedAssign(tokens, id, getExpression(e), UnknownFlow)
        case _ => throwParserError(c)
      }
    }
    
    (named, ordered) match {
      case (Seq(), Seq()) => Seq()
      case (s, Seq()) => s
      case (Seq(), s) => s
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitHierarchical_instance(ctx: Hierarchical_instanceContext, attr: VerilogAttributes, module: Reference, params: Seq[Assign] ) = {
    val name = ctx.name_of_instance.getText()
    val ports = getPortMap(ctx.list_of_port_connections)
    val i = ctx.getSourceInterval()
    DefInstance(i, attr, name, module, ports, params)
    
  }
  
  private def visitModule_or_interface_or_program_or_udp_instantiation(ctx: Module_or_interface_or_program_or_udp_instantiationContext, attr: VerilogAttributes): Statement = {
    val module = getRef(ctx.identifier)
    val params = ctx.parameter_value_assignment match {
      case null => Seq()
      case p    => visitParameter_value_assignment(p)
    }
    val insts = ctx.hierarchical_instance.asScala
      .map(i => visitHierarchical_instance(i, attr, module, params))
    getStmtOrSimpleBlock(ctx, insts)
  }
  
  private def getRawText(ctx: ParserRuleContext): String = {
    tokenStream.getText(ctx.getSourceInterval)
  }
  
  private def unsupportedComment(ctx: ParserRuleContext, str: String): Statement = {
    val txt = getRawText(ctx)
    unsupported.raiseWarn(ctx, s"Unsupported context ($str) converted to comments: ${txt}")
    Comment(ctx.getSourceInterval, txt) 
  }
  
  private def uselessToComment(ctx: ParserRuleContext): Statement = {
    Comment(ctx.getSourceInterval, getRawText(ctx)) 
  }
  
  private def visitCore_item_item(ctx: Core_item_itemContext, attr: VerilogAttributes): Statement = {
    ctx match {
      case i: CoreItemParamContext => visitCoreItemParam(i, attr)
      case i: CoreItemNetContext => visitNet_declaration(i.net_declaration, attr) 
      case i: CoreItemDataContext => visitData_declaration(i.data_declaration, attr) 
      case i: CoreItemContinuousContext => visitContinuous_assign(i.continuous_assign, attr)
      case i: CoreItemAlwaysContext => visitAlways_construct(i.always_construct, attr)
      
      case i: CoreItemCondGenContext => visitConditional_generate_construct(i.conditional_generate_construct, attr)
      case i: CoreItemLoopContext => visitLoop_generate_construct(i.loop_generate_construct, attr)
      case i: CoreItemInstanceContext => visitModule_or_interface_or_program_or_udp_instantiation(i.module_or_interface_or_program_or_udp_instantiation, attr)
      
      case i: CoreItemGenVarContext => uselessToComment(i)
      case i: CoreItemInitialContext => unsupportedComment(i, "TODO? CoreItemInitial")
      case i: CoreItemAssertContext => unsupportedComment(i, "CoreItemAssert")
      
      case i: CoreItemParamOverContext => unsupportedStmt(i, "CoreItemParamOver")
      case i: CoreItemGateContext => unsupportedStmt(i, "CoreItemGate")
      case i: CoreItemUdpContext => unsupportedStmt(i, "CoreItemUdp")
      case i: CoreItemTaskContext => unsupportedStmt(i, "CoreItemTask")
      case i: CoreItemFunctionContext => unsupportedStmt(i, "CoreItemFunction")
      case i: CoreItemCheckerContext => unsupportedStmt(i, "CoreItemChecker")
      case i: CoreItemDPIContext => unsupportedStmt(i, "CoreItemDPI")
      case i: CoreItemExternContext => unsupportedStmt(i, "CoreItemExtern")
      case i: CoreItemClassContext => unsupportedStmt(i, "CoreItemClass")
      case i: CoreItemIntfContext => unsupportedStmt(i, "CoreItemIntf")
      case i: CoreItemClassConsContext => unsupportedStmt(i, "CoreItemClassCons")
      case i: CoreItemCoverContext => unsupportedStmt(i, "CoreItemCover")
      case i: CoreItemPropertyContext => unsupportedStmt(i, "CoreItemProperty")
      case i: CoreItemSeqContext => unsupportedStmt(i, "CoreItemSeq")
      case i: CoreItemLetContext => unsupportedStmt(i, "CoreItemLet")
      case i: CoreItemClockContext => unsupportedStmt(i, "CoreItemClock")
      case i: CoreItemBindContext => unsupportedStmt(i, "CoreItemBind")
      case i: CoreItemNetAliasContext => unsupportedStmt(i, "CoreItemNetAlias")
      case i: CoreItemFinalContext => unsupportedStmt(i, "CoreItemFinal")
      case i: CoreItemElabContext => unsupportedStmt(i, "CoreItemElab")
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitGenerate_item_item(ctx: Generate_item_itemContext, attr: VerilogAttributes): Statement = {
    (ctx.core_item_item, ctx.extern_tf_declaration) match {
      case (i, null) => visitCore_item_item(i, attr)
      case (null, s) => unsupportedStmt(s, "Generate extern_tf_declaration")
      case _ => throwParserError(ctx)
    }
  }
  
  
  private def visitGenerate_begin_end_block(ctx: Generate_begin_end_blockContext): Statement = {
    val stmts = ctx.generate_item.asScala.map(visitGenerate_item)
    val name = ctx.identifier.asScala.map(_.getText()) match {
      case Seq() => ""
      case Seq(n) => n
      case Seq(n, n2) if (n == n2) => n  
      case s => unsupported.raiseIt(ctx, s"Ignored multiple identifier name ${s.tail.mkString("(",",",")")}"); s.head
    }
    SimpleBlock(ctx.getSourceInterval, stmts, name)
  }
  // generate_item:
  //     ( attribute_instance )* generate_item_item
  //     | KW_RAND data_declaration
  //     | generate_region
  //     | generate_begin_end_block
  private def visitGenerate_item(ctx: Generate_itemContext): Statement = {
    ctx match {
      case g: GenItemItemContext => 
      val attr = visitAttributes(g.attribute_instance.asScala)
      visitGenerate_item_item(g.generate_item_item, attr)
      case g: GenItemDataDeclContext => unsupportedStmt(g, "GenItemDataDecl")
      case g: GenItemGenRegContext => unsupportedStmt(g, "GenItemGenReg")
      case g: GenItemGenBlockContext => visitGenerate_begin_end_block(g.generate_begin_end_block)
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitGenerate_region(ctx: Generate_regionContext): Statement = {
    val s = ctx.generate_item.asScala.map(visitGenerate_item)
    SimpleBlock(ctx.getSourceInterval, s)
  }
  
  private def visitModule_item_item(ctx: Module_item_itemContext, attr: VerilogAttributes): Statement = {
    (ctx.core_item_item, ctx.specparam_declaration) match {
      case (i, null) => visitCore_item_item(i, attr)
      case (null, s) => unsupportedStmt(s, "Module specparam_declaration")
      case _ => throwParserError(ctx)
    }
  }
  
  private def visitModule_item(ctx: Module_itemContext): Statement = {
    ctx match {
      case i: ModuleGenRegContext => visitGenerate_region(i.generate_region)
      case i: ModuleItemItemContext =>
        val attr = visitAttributes(i.attribute_instance.asScala)
        visitModule_item_item(i.module_item_item, attr)
      case i: ModulePortDeclContext => visitNonansi_port_declaration(i.nonansi_port_declaration)
        
      case i: ModuleSpecBlockContext => unsupportedStmt(i, "ModuleSpecBlock")
      case i: ModuleProgDeclContext => unsupportedStmt(i, "ModuleProgDecl")
      case i: ModuleDeclContext => unsupportedStmt(i, "ModuleDecl")
      case i: ModuleIntfDeclContext => unsupportedStmt(i, "ModuleIntfDecl")
      case i: ModuleTimeDeclContext => unsupportedStmt(i, "ModuleTimeDecl")
      case _ => throwParserError(ctx)
    }
    
  }
  
  private def visitModule(ctx: Module_declarationContext) : DefModule = {
    unsupported.check(ctx)
    val h = ctx.module_header_common
    unsupported.check(h)
    val attr = visitAttributes(h.attribute_instance.asScala)
    val name = getRefText(h.identifier)
    val params = visitParams(h.parameter_port_list)
    
    // ports are considered directly as the first statements in all cases
    val (ports, declared) = visitPortsDeclaration(ctx.list_of_port_declarations)
    val body = SimpleBlock(ports ++ ctx.module_item.asScala.map(visitModule_item))

    val module = Module(ctx.getSourceInterval(), attr,name,params,body)
    
    // check nonansi ports against actual port declaration
    val inlineDeclared = module.ports.map(p => p.name).toSet
    declared.foreach(name => {
      if(!inlineDeclared.contains(name))
        warn(ctx, s"Missing inline declaration for advertised nonansi port $name (ignored)")
    })
    
    module
  }
  
}
