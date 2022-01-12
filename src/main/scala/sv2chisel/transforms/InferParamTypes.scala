// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashMap}

// NB: this pass is only useful for Module Parameters without defaults
// DefParam with values will get their type defined thanks to LegalizeExpression
// there are two ways of getting info about the param type:
// 1. usage in the module
// - if(pred) => pred must be boolean
// - DoPrim( BoolOp ) => args must be boolean
// - DoPrim( ArithOp ) => args must be int (or string)
// 2. nature of provided params in DefInstance
class InferParamTypes(val options: TranslationOptions) extends DefModuleBasedTransform {
  
  
  // NB: remote reference are available through remoteRefs but it does not contain remoteParams
  // Let's do a first pre-processing pass & record nature of parameters provided distantly 
  class UsageStore() {
    
    private val localDef = new HashMap[String, (FullType, Option[Expression])]()
    def registerDef(name: String, ftpe: FullType, expr: Option[Expression]): Unit = {
      ftpe.tpe match {
        case _:UnknownType => // skip 
        case _ => localDef += ((name, (ftpe, expr)))
      }
    }
    def get(name: String): Option[(FullType, Option[Expression])] = localDef.get(name)
    
    private val localUse = new HashMap[String, Seq[FullType]]()
    def localUse(name: String, ftpe: FullType): Unit = {
      (localUse.get(name), ftpe.tpe) match {
        case (_, _:UnknownType) => // skip
        case (Some(s), _) => localUse(name) = s :+ ftpe 
        case _ => localUse += ((name, Seq(ftpe)))
      }
    }
    
    // Arithmetic Type
    def localUseInt(ref: String): Unit = {
      localUse(ref, FullType(IntType(UndefinedInterval, NumberDecimal), SwExpressionKind))
    }
    def localUseUInt(ref: String): Unit = {
      localUse(ref, FullType(UIntType(UndefinedInterval, UnknownWidth(), NumberDecimal), HwExpressionKind))
    }
    def localUseNum(ref: String, kind: ExpressionKind): Unit = {
      kind match {
        case HwExpressionKind => localUseUInt(ref)
        case _ => localUseInt(ref)
      }
    }
    
    // Boolean types 
    def localUseSwBoolean(ref: String): Unit = {
      localUse(ref, FullType(BoolType(UndefinedInterval), SwExpressionKind))
    }
    def localUseHwBoolean(ref: String): Unit = {
      localUse(ref, FullType(BoolType(UndefinedInterval), HwExpressionKind))
    }
    def localUseBoolean(ref: String, kind: ExpressionKind): Unit = {
      localUse(ref, FullType(BoolType(UndefinedInterval), kind))
    }
    
    private val remoteUse = new HashMap[String, Seq[FullType]]()
    def registerRemote(name: String, ftpe: FullType): Unit = {
      (remoteUse.get(name), ftpe.tpe) match {
        case (_, _:UnknownType) => // skip
        case (Some(s), _) => remoteUse(name) = s :+ ftpe 
        case _ => remoteUse += ((name, Seq(ftpe)))
      }
    }
    
    private def reduceTypes(defExpr: Option[Expression])(a:(Option[FullType], String), b:(Option[FullType], String)): 
      (Option[FullType], String) = {
      (a, b) match {
        case ((None, sa), (None, sb)) => (None, s"$sa$sb")
        case ((None, sa), (Some(_), sb)) => (None, s"$sa$sb")
        case ((Some(_), sa), (None, sb)) => (None, s"$sa$sb")
        case ((Some(fta), sa), (Some(ftb), sb)) =>
          val kind = (fta.kind, ftb.kind) match {
            case (UnknownExpressionKind, UnknownExpressionKind) => SwExpressionKind
            case (UnknownExpressionKind, SwExpressionKind) => SwExpressionKind
            case (SwExpressionKind, UnknownExpressionKind) => SwExpressionKind
            case (SwExpressionKind, SwExpressionKind) => SwExpressionKind
            case _ => HwExpressionKind // mixed will require proper cast into Hw 
          }
          lazy val boolTpe = Some(FullType(BoolType(UndefinedInterval), kind))
          
          (fta.tpe, ftb.tpe) match {
            case (ta, tb) if(Utils.eq(ta, tb)) => (Some(FullType(ta, kind)), s"$sa$sb")
            case (s:StringType, _:IntType) => (Some(FullType(s, kind)), s"$sa$sb")
            case (_:IntType, s:StringType) => (Some(FullType(s, kind)), s"$sa$sb")
            case (_:BoolType, _:IntType) | (_:IntType, _:BoolType) =>
              defExpr match {
                case Some(v) => 
                  v match {
                    case n:Number =>
                      n.getInt match {
                        case Some(0) => (boolTpe, s"$sa$sb")
                        case Some(1) => (boolTpe, s"$sa$sb")
                        case _ => 
                          (None, s"Unable to reduce types Bool and Int with value = ${v.serialize}; $sa$sb")
                      }
                    case _ =>
                      (None, s"Unable to reduce types Bool and Int with value = ${v.serialize}; $sa$sb")
                  }
                case None => (boolTpe, s"Reducing Bool and Int as Bool ; $sa$sb")
              }
              
            case (ta, tb) => (None, s"Unable to reduce types ${ta.serialize} and ${tb.serialize} ; $sa$sb")
          }
      }
    }
    
    def extractType(uses: Seq[FullType], defExpr: Option[Expression]): (Option[FullType], String) = {
      uses match {
        case Seq() => (None, "No properly typed use recorded")
        case Seq(ftpe) => (Some(ftpe), "")
        case s => s.map(f => (Some(f),"")).reduce(reduceTypes(defExpr))
      }
    }
    
    def computeBestType(name: String): (Option[FullType], String) = {
      val remoteUses = remoteUse.getOrElse(name, Seq())
      val localUses = localUse.getOrElse(name, Seq())
      val allUses = remoteUses ++ localUses
      
      // first check local def for decent tpe
      localDef.get(name) match {
        case Some((ftpe, v)) => ftpe.tpe match {
          case _:UnknownType => extractType(allUses, v)
          case t => 
            extractType(allUses, v) match {
              case (Some(ext), _) if(Utils.eq(ext.tpe, t)) => (Some(ftpe), "")
              case (Some(ext), s) => (Some(ext), s"Converting from ${t.serialize} to ${ext.tpe.serialize} ; $s")
              case (_, s) => (Some(ftpe), s"defined as ${t.serialize} but cannot extract common use type: $s")
            }
        }
        case _ => extractType(allUses, None)
      }
    }
  }
  
  val modParamStore = new HashMap[String, UsageStore]() // one UsageStore per module name
  
  def getType(e: Expression, kind: ExpressionKind)(implicit store: UsageStore): Option[FullType] = {
    val numTpe = kind match {
      case HwExpressionKind => Some(FullType(UIntType(UndefinedInterval, UnknownWidth(), NumberDecimal), kind))
      case _ => Some(FullType(IntType(UndefinedInterval, NumberDecimal), SwExpressionKind))
    }
    
    e match {
      case r: Reference => 
        r.tpe match {
          case _: UnknownType =>
            (r.path.isEmpty, store.get(r.name), remoteRefs.get(r)) match {
              case (true, None, Some(ftpe)) => Some(ftpe)  // empty path : try local first, then remote
              case (true, Some((ftpe, _)), _) => Some(ftpe) // priority of local refs
              case (false, _, Some(ftpe)) => Some(ftpe) // ref with path cannot be local
              case _ => None
            }
          case _ => Some(FullType(r.tpe, r.kind))
        }
        
      
      case _: Number => numTpe // note: will be properly casted by legalize expression if required
      case _: StringLit => Some(FullType(StringType(UndefinedInterval, UnknownWidth()), kind))
      
      case d:DoPrim if(d.op.expectedArgs != d.args.size) => 
        fatal(d, s"Unexpected number of args (${d.args.size}) for operator ${d.op} (expected ${d.op.expectedArgs})")
        None
        
      case DoPrim(_, _: PrimOps.InlineIf, args, _, _) => args match {
        case Seq(cond@_, conseq, alt) => 
          (getType(conseq, kind), getType(alt, kind)) match {
            case (Some(ftpa), Some(ftpb)) =>
              (ftpa.tpe, ftpb.tpe) match {
                case (_:UnknownType, _:UnknownType) => None
                case (_, _:UnknownType) => Some(ftpa)
                case (_:UnknownType, _) => Some(ftpb)
                case (ta, tb) if (!Utils.eq(ta, tb)) => None
                case _ => Some(ftpa)
              }
            case (Some(ftpa), _) => Some(ftpa)
            case (_, Some(ftpb)) => Some(ftpb)
            case _ => None
              
          }
        case _ => None // impossible: args length check above
      }
      
      case DoPrim(_, _: PrimOps.Par, args, _, _) => getType(args.head, kind)
      
      case d: DoPrim => // no need for further recursivity here :)
        import PrimOps._
        d.op match {
          case _:BoolOp => Some(FullType(BoolType(UndefinedInterval), kind))
          case _:RedOp => Some(FullType(BoolType(UndefinedInterval), kind))
          case _:Add | _:Sub =>  
            // notable exception : String might be added with String or numTpe => require recursion
            // only allowing + and - on string, add * ?
            (getType(d.args(0), kind), getType(d.args(1), kind)) match {
              case (Some(ftpe@FullType(_:StringType,_,_,_,_,_)), _) => Some(ftpe)
              case (_, Some(ftpe@FullType(_:StringType,_,_,_,_,_))) => Some(ftpe)
              case _ => numTpe
            }
          case _:NumOp => numTpe 
          case _:ShiftOp => numTpe
          case _:BitOp => numTpe
          case _:UncharOp => None // impossible: InlineIf & Par cases treated above
        }
        
      case s: SubIndex => 
        getType(s.expr, kind) match {
          case Some(ftpe) =>
            ftpe.tpe match {
              case _:UIntType => Some(FullType(BoolType(UndefinedInterval), HwExpressionKind)) 
              // automated conversion to UInt to happen later
              case _:IntType => Some(FullType(BoolType(UndefinedInterval), HwExpressionKind)) 
              case v:VecType => 
                v.tpe match {
                  case Seq(t) => Some(FullType(t, ftpe.kind))
                  case _ =>
                    debug(e, s"Cannot infer type for SubIndex of Vec ${v.serialize} (expression ${s.expr.serialize})")
                    None  
                }
                
              case t => 
                debug(e, s"Cannot infer type for SubIndex of ${t.serialize} (expression ${s.expr.serialize})")
                None
            }
            
          case None => 
            debug(e, s"Cannot infer type for SubIndex of untyped expression ${s.expr.serialize}")
            None 
        }
        
      case s: SubRange => 
        getType(s.expr, kind) match {
          case Some(ftpe) =>
            ftpe.tpe match { 
              // automated conversion to UInt to happen later
              case _:IntType => Some(FullType(BoolType(UndefinedInterval), HwExpressionKind)) 
              case _ => Some(ftpe)
            }
            
          case None => 
            debug(e, s"Cannot infer type for SubRange of untyped expression ${s.expr.serialize}")
            None 
        }
      
      case _ =>  
        debug(e, s"Cannot infer type for expression ${e.serialize} (${e.getClass.getName})")
        None
    }
  }
  
  def recordTypeUsage(t: Type)(implicit store: UsageStore): Unit = {
    t.foreachWidth(w => {
      w.expr match {
        case r:Reference => store.localUseNum(r.name, SwExpressionKind) // REF.W => REF should be Int
        case _ => recordExprUsage(w.expr) 
      }
    })
  }
  
  // look for some pattern enabling us to conclude of some reference types
  def recordExprUsage(e: Expression)(implicit store: UsageStore): Unit = {
    e match {
      case DoPrim(_, _: PrimOps.InlineIf, Seq(r: Reference, _, _), _, _) if(r.path.isEmpty) => 
        store.localUseSwBoolean(r.name)
        
      case d: DoPrim => 
        import PrimOps._
        d.op match {
          case _:BoolArgs => 
            d.args.collect {case r: Reference if(r.path.isEmpty) => store.localUseBoolean(r.name,e.kind)}
            
          case _:NumArgs | _:StringOrNumArgs => 
            d.args.collect {case r: Reference if(r.path.isEmpty) => store.localUseNum(r.name,e.kind)}
            
          case _:AnyArgs => // cannot conclude
        }
        
      case _ => 
    }
    e.foreachExpr(recordExprUsage)
    e.foreachType(recordTypeUsage)
  }
  
  def recordStmtUsage(s: Statement)(implicit store: UsageStore): Statement = {
    s.foreachExpr(recordExprUsage)
    s.foreachType(recordTypeUsage)
    s.mapStmt(recordStmtUsage) match {
      
      case i: IfGen => 
        i.pred match {
          case r:Reference => store.localUseSwBoolean(r.name) // if(REF) => REF should be Boolean
          case _ => 
        }
        i
        
      case c: Conditionally => 
        c.pred match {
          case r:Reference => store.localUseHwBoolean(r.name) // when(REF) => REF should be Bool
          case _ => 
        }
        c
      
      
      case p: DefParam => 
        (p.tpe, p.value) match {
          case (_: UnknownType, Some(v)) => 
            getType(v, p.kind) match {
              case Some(ftpe) => 
                store.registerDef(p.name, ftpe, Some(v))
                p.copy(tpe = Utils.cleanTok(ftpe.tpe), kind = ftpe.kind)
              case _ => debug(p, s"Unable to infer proper type for defparam ${p.name}"); p 
            }
          case _ => store.registerDef(p.name, FullType(p.tpe, p.kind), p.value); p
        }
        
      case i:DefInstance => 
        val paramMap = currentProject.get.findModule(i.module.serialize) match {
          case Some(m) => 
            // re-using small part of existing code
            // NB: stream won't be set => providing local getInfo through ctx string 
            val tpeRefInst = new TypeReferences(options)
            val ctx = s"for instance `${i.name}` of module `${i.module.serialize}` ${getAtInfo(i)}"
            tpeRefInst.processAssignSeq(i.paramMap, m.params.map(p => (p.name, FullType(p.tpe, p.kind))), ctx)
          
          case _ => i.paramMap // defaulting to name only
        }
        
        val remoteStore = modParamStore.getOrElse(i.module.serialize, new UsageStore())
        paramMap.foreach(_ match {
          case r:RemoteLinked if(r.remoteName.isDefined) => 
            getType(r.expr, r.kind) match {
              case Some(ftpe) => remoteStore.registerRemote(r.remoteName.get, ftpe)
              case _ => debug(r, s"Unable to infer proper type for assign to param ${r.remoteName.get}") 
            }
          case p => 
            warn(p, s"Unable to match parameter ${p.serialize} with remote parameter of module ${i.module.serialize} at instance ${i.name}: type inference might be inaccurate")
        })
        modParamStore(i.module.serialize) = remoteStore
        i
        
      case st => st
    }
  }
  
  // first complete pass: creating UsageStore & filling it upon DefInstance & local usage 
  override val preprocessDescription = Some(((d: Description) => {
    d match {
      case m: DefModule => 
        implicit val store = modParamStore.getOrElse(m.name, new UsageStore())
        m.foreachParam(p => {
          (p.tpe, p.value) match {
            case (_: UnknownType, Some(v)) => 
              getType(v, p.kind) match {
                case Some(ftpe) => store.registerDef(p.name, ftpe, Some(v))
                case _ => debug(p, s"Unable to infer proper type for param ${p.name}") 
              }
            case _ => store.registerDef(p.name, FullType(p.tpe, p.kind), p.value)
          }
          p.foreachExpr(recordExprUsage)
        })
        modParamStore += ((m.name, store)) // store is not copied by value
        m.mapStmt(recordStmtUsage)
      case d => d
    }
  }))
  
  
  def processModule(m: DefModule): DefModule = {
    // Single local pass : Take decisions based on local store
    def processParam(p: DefParam, noDefault: Boolean = false)(implicit store: UsageStore): DefParam = {
      p.tpe match {
        case _: UnknownType => 
          store.computeBestType(p.name) match {
            case (Some(ftpe), s) => 
              if(!s.isEmpty) info(p, s"Type inference for parameter ${p.name} returned: $s")
              p.copy(tpe = Utils.cleanTok(ftpe.tpe), kind = ftpe.kind)
            case (_, s) => // to do add noDefault here (after bug fix)
              if (noDefault) {
                debug(p, s"Unable to retrieve type information for parameter ${p.name}: $s")
                p
              } else {
                warn(p, s"Unable to retrieve type information for parameter ${p.name}: $s - Defaulting to IntType")
                p.copy(tpe = IntType(UndefinedInterval, NumberDecimal), kind = SwExpressionKind)
              }
          }
        case _ =>
          // trying it anyway would be interesting ... => converting int to bool for example 
          store.computeBestType(p.name) match {
            case (Some(ftpe), s) => 
              (ftpe.tpe, p.tpe) match {
                case (n, o) if(Utils.eq(n, o)) => p
                case (n, o) =>
                  info(p, s"Converting parameter ${p.name} type from ${o.serialize} to ${n.serialize} ($s)")
                  p.copy(tpe = Utils.cleanTok(n))
              }
            case _ => p // nothing to do
          }
      }
    }
    
    def processStatement(s: Statement)(implicit store: UsageStore): Statement = {
      s match {
        case p: DefParam => processParam(p, noDefault = true) // explicit inacurate type cast will cause later issues 
        case _ => s.mapStmt(processStatement)
      }
    }
    
    modParamStore.get(m.name) match {
      case Some(store) => m.mapParam(processParam(_)(store)).mapStmt(processStatement(_)(store))
      case _ => 
        critical(m, s"Cannot found store for module ${m.name}")
        m
    }
    
  }
}