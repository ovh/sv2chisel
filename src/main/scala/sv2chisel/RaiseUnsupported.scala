// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import org.antlr.v4.runtime.{ParserRuleContext,CommonTokenStream, Token}
import org.antlr.v4.runtime.tree.{AbstractParseTreeVisitor, ParseTreeVisitor, TerminalNode}
import scala.collection.JavaConverters._
import scala.collection.mutable.{ArrayBuffer}

import sv2chisel.antlr._
import sv2chisel.ir.{UndefinedInterval, Interval}
import sv2017Parser._
import Utils.throwInternalError

// TODO : add selection ? error or warn ? 
// or collection of all errors for counts:
//  - gives better idea to the developper whether its verilog has a good chance to fit in
trait UnsupportedBehavior
case object DirectError extends UnsupportedBehavior
case object DelayedError extends UnsupportedBehavior
case object DirectWarning extends UnsupportedBehavior
case object DelayedWarning extends UnsupportedBehavior

class RaiseUnsupported(
    val mode: UnsupportedBehavior,
    val tokenStream: CommonTokenStream,
    val path: String
) extends ParserLogging {
  
  private val errors = ArrayBuffer[String]()
  private val warnings = ArrayBuffer[String]()
  
  private val throwError = mode match {
    case DelayedError | DirectError => true
    case _ => false 
  }
  
  def check(ctx: ParserRuleContext): Unit = {
    ctx match {
      case null => // nothing to do
      case c: IdentifierContext => checkIdentifier(c)
      case c: Module_declarationContext => checkModule(c)
      case c: CoreItemParamContext => checkCoreItemParam(c)
      case c: PrimaryTfCallContext => checkPrimaryTfCall(c)
      case c: PrimaryCallContext => checkPrimaryCall(c)
      case c: List_of_argumentsContext => checkTfCallArgs(c)
      case c: Net_declarationContext => checkNetDecl(c)
      case c: Module_header_commonContext => checkModuleHeader(c)
      case c: Net_or_var_data_typeContext => checkVarDataType(c)
      case c: List_of_port_declarationsContext => checkPortsDeclaration(c)
      case c: Continuous_assignContext => checkAssign(c)
      case c: Cond_predicateContext => checkCondExpression(c)
      case c: Data_typeContext => checkDataType(c)
      case c: Data_declarationContext => checkDataDecl(c)
      case c: Net_typeContext => checkNetType(c)
      case c: Variable_decl_assignmentContext => checkVarDeclAssign(c)
      case c: Variable_dimensionContext => checkVarDim(c)
      case c: Package_or_class_scoped_pathContext => checkPath(c)
      case c: Package_declarationContext => checkPackage(c)
      case c: Case_statementContext => checkCase(c)
      case c: Loop_statementContext => checkLoop(c)
      
      case c => Utils.throwInternalError(s"Unsupported ParserRuleContext ${c.getClass}")
    }
  }
  
  def raise(): Unit = {
    if (!warnings.isEmpty || !errors.isEmpty) {
      struct("##### ERROR REPORTING #####")
    }
    
    if(!warnings.isEmpty) struct("Warnings: ")
    for (s <- warnings) {
      warn(s)
    }
    
    if(!warnings.isEmpty) struct(if(throwError) "Errors: " else "Errors (downgraded to warnings): ")
    for (s <- errors) {
      if (throwError) {
        fatal(s)
      } else {
        critical(s)
      }
    }
    if (throwError && errors.length > 0) {
      Utils.throwInternalError(s"Found ${errors.length} issues with unsupported keywords or grammar structure.")
    }
    errors.clear()
    if (!warnings.isEmpty || !errors.isEmpty) {
      struct("#####       END       #####")
    }
  }
  
  def raiseFatal(str: String = ""): Unit = {
    raise()
    Utils.throwInternalError("Fatal error due to previous warnings/errors: " +str)
  }
  
  def raiseIt(ctx: ParserRuleContext, str: String): Unit = {
    raiseIt(ctx.getSourceInterval(), str)
  }
  
  def raiseIt(t: TerminalNode, str: String): Unit = {
    raiseIt(t.getSourceInterval(), str)
  }
  
  def raiseIt(i: Interval, str: String): Unit = {
    val withInfo = s"$str at ${getInfo(i)}" 
    mode match {
      case DirectError => Utils.throwInternalError(withInfo)
      case DelayedError => errors += withInfo
      case DirectWarning => logger.warn(withInfo)
      case DelayedWarning => errors += withInfo
    }
  }
  
  def raiseWarn(ctx: ParserRuleContext, str: String): Unit = {
    raiseWarn(ctx.getSourceInterval(), str)
  }
  def raiseWarn(t: TerminalNode, str: String): Unit = {
    raiseWarn(t.getSourceInterval(), str)
  }
  def raiseWarn(i: Interval, str: String): Unit = {
    warnings += s"$str at ${getInfo(i)}" 
  }

  def checkIdentifier(ctx: IdentifierContext): Unit = {
    ctx.KW_SAMPLE match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_RANDOMIZE match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_TYPE_OPTION match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_OPTION match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_STD match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
  }
  
  def checkModule(ctx: Module_declarationContext): Unit = {
    ctx.KW_EXTERN match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.DOT match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported module any port declaration (.*)")
    }
    ctx.timeunits_declaration match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported timeunits_declaration in module ${k.getText()}")
    }
    ctx.identifier match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported final identifier ${k.getText()}")
    }
  }
  
  def checkCase(ctx: Case_statementContext): Unit = {
    ctx.unique_priority match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported unique priority ${k.getText()}")
    }
    ctx.KW_INSIDE match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword `${k.getText()}`")
    }
    ctx.KW_MATCHES match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword `${k.getText()}`")
    }
  }
  
  def checkLoop(ctx: Loop_statementContext): Unit = {
    ctx.KW_FOREVER match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword `${k.getText()}`")
    }
    ctx.KW_REPEAT match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword `${k.getText()}`")
    }
    ctx.KW_WHILE match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword `${k.getText()}`")
    }
    ctx.KW_DO match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword `${k.getText()}`")
    }
    ctx.KW_FOREACH match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword `${k.getText()}`")
    }
  }
  
  def checkCondExpression(ctx: Cond_predicateContext): Unit = {
    ctx.KW_MATCHES.asScala match {
      case Seq() =>
      case s => raiseIt(ctx, s"Unsupported keyword match within context ${ctx.getText()}")
    }
    ctx.TRIPLE_AND.asScala match {
      case Seq() =>
      case s => raiseIt(ctx, s"Unsupported keyword within context ${ctx.getText()}")
    }
    ctx.pattern.asScala match {
      case Seq() =>
      case s => raiseIt(ctx, s"Unsupported pattern within context ${ctx.getText()}")
    }
  }
  
  def checkNetDecl(ctx: Net_declarationContext): Unit = {
    ctx.KW_INTERCONNECT match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_VECTORED match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_SCALARED match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.drive_strength match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${k.getText()}")
    }
    ctx.charge_strength match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${k.getText()}")
    }
    ctx.delay3 match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${k.getText()}")
    }
    ctx.delay_control match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${k.getText()}")
    }
    // weird grammar ...
    ctx.identifier.asScala match {
      case Seq() =>
      case Seq(id) =>
      case s => raiseIt(ctx, s"Unsupported identifier within context ${ctx.getText()}")
    }
    checkNetType(ctx.net_type)
  }
  def checkAssign(ctx: Continuous_assignContext): Unit = {
    ctx.drive_strength match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${k.getText()}")
    }
    ctx.delay3 match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${k.getText()}")
    }
    ctx.delay_control match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${k.getText()}")
    }
  }
  
  def checkVarDeclAssign(ctx: Variable_decl_assignmentContext): Unit = {
    ctx.class_new match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${ctx.getText()}")
    }
    ctx.dynamic_array_new match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${ctx.getText()}")
    }
  }
  
  def checkDataDecl(ctx: Data_declarationContext): Unit = {
    ctx.KW_CONST match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${ctx.getText()}")
    }
    ctx.KW_VAR match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported keyword ${ctx.getText()}")
    }
    ctx.lifetime match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${ctx.getText()}")
    }
  }
  
  def checkPackage(ctx: Package_declarationContext): Unit = {
    ctx.lifetime match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${ctx.getText()}")
    }
    ctx.timeunits_declaration match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${ctx.getText()}")
    }
  }
  
  def checkCoreItemParam(ctx: CoreItemParamContext): Unit = {
    ctx.default_clocking_or_dissable_construct match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported context ${ctx.getText()}")
    }
  }
  
  def checkModuleHeader(ctx: Module_header_commonContext): Unit = {
    ctx.lifetime match {
      case null =>
      case k => raiseIt(ctx, s"Unsupported lifetime ${k.getText()}")
    }
    ctx.package_import_declaration.asScala match {
      case Seq() =>
      case s => raiseIt(ctx, s"Unsupported package_import_declaration within context ${ctx.getText()}")
    }
  }
  
  def checkPrimaryCall(ctx: PrimaryCallContext): Unit = {
    ctx.array_method_name match {
      case null =>
      case _ => raiseIt(ctx, s"Unsupported array_method_name ${ctx.getText()}")
    }
    ctx.KW_WITH match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
  }
  
  def checkPrimaryTfCall(ctx: PrimaryTfCallContext): Unit = {
    ctx.clocking_event match {
      case null =>
      case _ => raiseIt(ctx, s"Unsupported clocking event ${ctx.getText()}")
    }
    ctx.data_type match {
      case null =>
      case d => 
      d.data_type_usual match {
        case null => raiseIt(ctx, s"Unsupported data type in function call ${d.getText()}")
        case dtp => dtp.package_or_class_scoped_path match {
          case null => raiseIt(ctx, s"Unsupported data type in function call ${d.getText()}")
          case p => 
        }
      }
    }
  }
  
  def checkTfCallArgs(ctx: List_of_argumentsContext): Unit = {
    ctx.list_of_arguments_named_item.asScala match {
      case Seq() =>
      case _ => raiseIt(ctx, s"Unsupported named arguments call ${ctx.getText()}")
    }
  }
  
  def checkPath(ctx: Package_or_class_scoped_pathContext): Unit = {
    // package_or_class_scoped_path:
    //    ( KW_LOCAL DOUBLE_COLON )? (
    //   		KW_DOLAR_ROOT
    //         | implicit_class_handle
    //         | KW_DOLAR_UNIT
    //         | package_or_class_scoped_path_item
    // 	) ( DOUBLE_COLON package_or_class_scoped_path_item)*;
    ctx.KW_LOCAL() match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_DOLAR_ROOT() match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_DOLAR_UNIT() match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.implicit_class_handle match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported implicit class handle ${k.getText()}")
    }
  }
  
  def checkDataType(ctx: Data_typeContext): Unit = {
    ctx.KW_CHANDLE() match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_VIRTUAL() match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_EVENT() match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
  }
  
  def checkNetType(ctx: Net_typeContext): Unit = {
    ctx match {
      case null =>
      case n => n.KW_WIRE() match {
        case null => raiseIt(ctx, s"Unsupported keyword ${n.getText()}")
        case _ => 
      }
    }
  }
  
  def checkVarDataType(ctx: Net_or_var_data_typeContext): Unit = {
    ctx.KW_INTERCONNECT() match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    ctx.KW_VAR() match {
      case null => 
      case k => raiseIt(ctx, s"Unsupported keyword ${k.getText()}")
    }
    checkNetType(ctx.net_type)
  }
  
  def checkVarDim(ctx: Variable_dimensionContext): Unit = {
    ctx.MUL() match {
      case null => 
      case _ => raiseIt(ctx, s"Unsupported '*' as variable dimension ${ctx.getText()}")
    }
    ctx.data_type match {
      case null => 
      case _ => raiseIt(ctx, s"Unsupported data type as variable dimension ${ctx.getText()}")
    }
  }
  
  private def checkPortsDeclaration(ctx: List_of_port_declarationsContext) {
    ctx match {
      case null => 
      case list => 
        list.nonansi_port.asScala match {
          case Seq() => 
          case l    => raiseIt(ctx, s"Unsupported non ANSI port declaration ${l.foreach(_.getText())}")
        }
        
    }
  }
  

  
}
