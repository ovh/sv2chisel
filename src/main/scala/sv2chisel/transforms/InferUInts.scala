// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._

import collection.mutable.{HashMap}

class InferUInts(val options: TranslationOptions) extends DescriptionBasedTransform {
  
  val storeCollection = HashMap[String, UsageStore]()
  
  /** helper class containing all the logic for registering usage & choose the final type **/
  class UsageStore(val name: String) {
    
    private val currentType = new HashMap[String, Type]()
    private val forwardStore = new HashMap[String, (Option[String], String)]() // ref -> store name
    
    def applyToStore(ref: String, fwd: (UsageStore, String) => Unit, apply: String => Unit): Unit = {
      currentType.get(ref) match {
        case Some(_) => apply(ref)
        case _ => forwardStore.get(ref) match {
          case Some((Some(st), fref)) => storeCollection.get(st).map(fwd(_, fref))
          case Some((None, fref)) => apply(fref)
          case _ => 
        }
      }
    }
    
    private val arithmeticUsageCount = new HashMap[String, Int]()
    private val stringAssignedCount = new HashMap[String, Int]()
    // simple apply or slice operator translation
    private val blockAccessCount = new HashMap[String, Int]()
    private val indexAccessCount = new HashMap[String, Int]()
    private val rangeAccessCount = new HashMap[String, Int]()
    // much harder conversion > will be avoided in all cases
    // see connect & update implicits within fpga-vac-chisel (PR 20)
    private val blockAssignCount = new HashMap[String, Int]()
    private val indexAssignCount = new HashMap[String, Int]()
    private val rangeAssignCount = new HashMap[String, Int]()
    
    def registerArithUsage(ref: String): Unit = {
      applyToStore(ref, (st, r) => st.registerArithUsage(r), r => {
        val count = arithmeticUsageCount.getOrElse(r, 0) + 1
        arithmeticUsageCount.update(r, count)
      })
    }
    def registerAssignedString(ref: String): Unit = {
      applyToStore(ref, (st, r) => st.registerAssignedString(r), r => {
        val count = stringAssignedCount.getOrElse(r, 0) + 1
        stringAssignedCount.update(r, count)
      })
    }
    def registerIndexAccess(ref: String): Unit = {
      applyToStore(ref, (st, r) => st.registerIndexAccess(r), r => {
        val count = indexAccessCount.getOrElse(r, 0) + 1
        indexAccessCount.update(r, count)
      })
    }
    def registerIndexAssign(ref: String): Unit = {
      applyToStore(ref, (st, r) => st.registerIndexAssign(r), r => {
        val count = indexAssignCount.getOrElse(r, 0) + 1
        indexAssignCount.update(r, count)
      })
    }
    def registerIndex(ref: String, assign: Boolean): Unit = {
      if (assign) {
        registerIndexAssign(ref)
      } else {
        registerIndexAccess(ref)
      }
    }
    def registerBlockAccess(ref: String): Unit = {
      applyToStore(ref, (st, r) => st.registerBlockAccess(r), r => {
        val count = blockAccessCount.getOrElse(r, 0) + 1
        blockAccessCount.update(r, count)
      })
    }
    def registerBlockAssign(ref: String): Unit = {
      applyToStore(ref, (st, r) => st.registerBlockAssign(r), r => {
        val count = blockAssignCount.getOrElse(r, 0) + 1
        blockAssignCount.update(r, count)
      })
    }
    def registerBlock(ref: String, assign: Boolean): Unit = {
      if (assign) {
        registerBlockAssign(ref)
      } else {
        registerBlockAccess(ref)
      }
    }
    
    def registerRangeAccess(ref: String): Unit = {
      applyToStore(ref, (st, r) => st.registerRangeAccess(r), r => {
        val count = rangeAccessCount.getOrElse(r, 0) + 1
        rangeAccessCount.update(r, count)
      })
    }
    def registerRangeAssign(ref: String): Unit = {
      applyToStore(ref, (st, r) => st.registerRangeAssign(r), r => {
        val count = rangeAssignCount.getOrElse(r, 0) + 1
        rangeAssignCount.update(r, count)
      })
    }
    def registerRange(ref: String, assign: Boolean): Unit = {
      if (assign) {
        registerRangeAssign(ref)
      } else {
        registerRangeAccess(ref)
      }
    }

    // Note : the following could be written without closing/ locking the store
    private var closed = false
    
    private def fillForwardStore(local: String, remote: String, tpe: Type, store: Option[String])(implicit refStore: RefStore): Unit = {
      forwardStore += local -> (store -> remote)
      tpe match {
        case v: VecType =>
          v.tpe match {
            case Seq(t) => fillForwardStore(s"$local[_]", s"$remote[_]", t, store)
            case _ => 
          }
        
        case b: BundleType =>
          b.fields.foreach(f => fillForwardStore(s"$local.${f.name}", s"$remote.${f.name}", f.tpe, store))
          
        case r:UserRefType => store match {
          case Some(s) => 
            storeCollection.get(s) match {
              case Some(st) => 
                st.getStore(remote) match {
                  case Some(sto) =>
                    sto.getDeclaredType(r.name) match {
                      case Some(tpe) => fillForwardStore(local, r.name, tpe, Some(sto.name))
                      case _ => critical(tpe, s"[Impossible] Missing declaration for ${r.name} in ${sto.name}") 
                    }
                  case _ => critical(tpe, s"Unable to retrieve remote type $local -> $remote  (${tpe.serialize})") 
                }
              case _ => critical(tpe, s"Unable to retrieve description $s store") 
            }
          case _ => 
            r.path match {
              case Seq(pkg) => 
                storeCollection.get(pkg).flatMap(_.getDeclaredType(r.name)) match {
                  case Some(tpe) => fillForwardStore(local, r.name, tpe, Some(pkg))
                  case _ => critical(tpe, s"Missing declaration for ${r.name} in ${pkg}") 
                }
              
              case _ => 
                getDeclaredType(r.name) match {
                  case Some(tpe) => fillForwardStore(local, r.name, tpe, None)
                  case _ => critical(tpe, s"Missing declaration for ${r.name} in ${name}") 
                }
            }
            
        }
        case _ => 
      }
      
    }
    
    def registerType(ref: String, tpe: Type)(implicit refStore: RefStore): Unit = {
      require(!closed, "Cannot register new type when Store is closed") // => user error
      tpe match {
        case v: VecType =>
          v.tpe match {
            case Seq(t) => registerType(s"$ref[_]", t)
            case _ => 
          }
          currentType.update(ref, tpe)
        
        case b: BundleType => // typically  DefType (name, BundleType)
          b.fields.foreach(f => {
            registerType(s"$ref.${f.name}", f.tpe)
          })
          currentType.update(ref, tpe)
          
        
        case r: UserRefType => 
          currentType.get(r.name) match {
            case Some(t) if(r.path.isEmpty) => fillForwardStore(ref, r.name, t, None)
            case _ => refStore.get(r).flatMap(_.defDescription) match {
              case Some(d) => 
                debug(tpe, s"Found remote ref type ${r.name} in description $d")
                storeCollection.get(d).flatMap(_.getDeclaredType(r.name)).map(fillForwardStore(ref, r.name, _, Some(d)))
              case _ => warn(tpe, s"Cannot find remote ref type ${r.serialize} in refStore: ${refStore.get(r)}")
            }
          }
          
        case _ => currentType.update(ref, tpe)
      }
      
    }
    
    def getStore(ref: String): Option[UsageStore] = getStore(Reference(UndefinedInterval, ref, Seq()))
    
    def getStore(ref: Reference): Option[UsageStore] = {
      currentType.contains(ref.name) match {
        case true if(ref.path.isEmpty) => Some(this)
        case _ => forwardStore.get(ref.name).flatMap { case (sn, _) => {
          sn match {
            case Some(n) => storeCollection.get(n)
            case _ => Some(this)
          }
        }}
      }
    } 
    def getDeclaredType(ref: String): Option[Type] = currentType.get(ref)
    
    def doConvertToUInt(ref: String): Boolean = {
      if(indexAssignCount.contains(ref) || rangeAssignCount.contains(ref)){
        trace(s"Ref $ref is assigned with index or range > no conversion to UInt")
        false
      } else {
        val bitRefs = indexAccessCount.getOrElse(ref, 0) + rangeAccessCount.getOrElse(ref, 0)
        val arithCount = arithmeticUsageCount.getOrElse(ref, 0) + blockAssignCount.getOrElse(ref, 0)
        trace(s"Ref $ref > bitRefs: $bitRefs ; arithCount: $arithCount")
        arithCount >= bitRefs
      }
    } 
    
    // note unsupported shall not raise error but just do nothing 
    private def uintLeafType(path: String, tpe: Type): Type = {
      tpe match {
        case v: VecType => 
          v.tpe match {
            case Seq(_: BoolType) => 
              // end of recursion let's return an UInt depending on
              if(stringAssignedCount.contains(path)){
                info(tpe, s"Converting $path to Vec of Char(UInt(8.W)) based on its usage in the module")
                v.asCharVecType
              } else if (doConvertToUInt(path)) {
                info(tpe, s"Converting $path to UInt based on its usage in the module")
                v.asUIntType 
              } else {
                v
              }

            case Seq(t) => v.mapType(_ => uintLeafType(s"$path[_]",t)) // walk the complex type
            case _ => v // unsupported mixed vec types
          }
        case b: BundleType => 
          b.copy(fields = b.fields.map(f => {
            f.copy(tpe = uintLeafType(s"$path.${f.name}", f.tpe))
          }))
        case _ => tpe // unsupported
      }
    }
    
    def getType(ref: String): Option[Type] = {
      closed = true
      // forward ref do not require local update (untyped reference)
      currentType.get(ref).map(uintLeafType(ref, _))
      // currentType.get(ref).map(t => Utils.cleanTok(uintLeafType(ref, t)))
    }
    
    def traceIt(): Unit = {
      trace(s"##### TRACING STORE FOR DESCRIPTION $name #####")
      trace("arithmeticUsageCount: " + arithmeticUsageCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("indexAccessCount: " + indexAccessCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("rangeAccessCount: " + rangeAccessCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("indexAssignCount: " + indexAssignCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("rangeAssignCount: " + rangeAssignCount.map(t => s"${t._1} -> ${t._2}").mkString("\n", "\n", "\n"))
      trace("currentType: " + currentType.map(t => s"${t._1} -> ${t._2.serialize}").mkString("\n", "\n", "\n"))
      trace("forwardStore: " + forwardStore.map { case (n, (st, r)) => s"$n -> ${st.getOrElse("local")}.$r" }.mkString("\n", "\n", "\n"))
      trace(s"##### End of tracing for $name #####")
    }
  }
  
  //FIRST PASS => fill local store
  override val preprocessDescription = Some(((d: Description) => {
    val name = d match {
      case n:HasName => n.name
      case _ => "<unnamed>" 
    }
    
    val store = new UsageStore(name)
    
    /** core function looking for reference in expressions 
      * assign : true if the expression is an assignement target
      * whole : false if this expression is a subreference
      * returns a serialized version of this expression down to root ref if any + the store associated to the ref def
      */
    def visitExpression(e: Expression, assign: Boolean, whole : Boolean = true): Option[(String, UsageStore)] = {
      e.foreachType(visitType)
      e match {
        case r: Reference =>
          val st = store.getStore(r)
          if (whole) st.map(_.registerBlock(r.serialize, assign))
          st.map(r.serialize -> _)
          
        case s: SubField => 
          visitExpression(s.expr, assign, false) match {
            case None => 
              debug(s, s"Unable to retrieve inner reference for subfield ${s.name} of ${s.expr.serialize}")
              None
            case Some((ref, st)) => Some(s"$ref.${s.name}" -> st)
          }
          
        case s: SubIndex => 
          visitExpression(s.expr, assign, false) match {
            case None => 
              debug(s, s"Unable to retrieve inner reference for subindex of ${s.expr.serialize}")
              None 
            case Some((ref, st)) => 
              trace(s, s"registering index ${if(assign) "assign" else "access"} for ${ref}")
              st.registerIndex(ref, assign)
              Some(s"$ref[_]" -> st)
          }
          
        case s: SubRange => 
          visitExpression(s.expr, assign, false) match {
            case None => None 
            case Some((ref, st)) => 
              store.registerRange(ref, assign)
              Some(s"$ref[_]" -> st)
          }
    
        case p: DoPrim =>
          if(assign) critical(p, "Should not try to assign to a primary op")
          p.args.foreach(a => {
            visitExpression(a, assign, whole) match {
              case None => //
              case Some((ref, st)) => st.registerArithUsage(ref)
            }
          })
          None
        case c: DoCast =>
          if(assign) critical(c, "Should not try to assign to a cast operator")
          visitExpression(c.expr, assign, whole)
          None
        case c: DoCall =>
          if(assign) critical(c, "Should not try to assign to a function call")
          c.args.foreach(visitExpression(_, assign, whole))
          None
        case c: Concat =>
          // could be both way 
          c.args.foreach(visitExpression(_, assign, whole))
          None
        case _: NamedAssign => None // TODO ??? 
        case _: NoNameAssign => None // TODO ???
        
        // Litteral & others
        case _ => None
      }
    }
    
    def visitType(t: Type): Unit = {
      t.foreachWidth(_.foreachExpr(visitExpression(_, false)))
      t.foreachType(visitType)
    }
    
    def visitStatement(s: Statement)(implicit refStore: RefStore): Unit = {
      s.foreachType(visitType)
      processImportStatement(s, refStore) match {
        case c: Connect => 
          visitExpression(c.loc, true)
          visitExpression(c.expr, false)
          
        // Localparams might be hardware
        case p: DefParam if(p.kind == HwExpressionKind) => 
          store.registerType(p.name, p.tpe)
          s.foreachExpr(visitExpression(_, false))
          
        case t: DefType => 
          store.registerType(t.name, t.tpe)
          visitType(t.tpe)
          
        case p: Port => 
          store.registerType(p.name, p.tpe)
          s.foreachExpr(visitExpression(_, false))
          
        case f: DefFunction => 
          store.registerType(f.name, f.tpe)
          s.foreachStmt(visitStatement)
          
        case l: DefLogic => 
          store.registerType(l.name, l.tpe)
          s.foreachExpr(visitExpression(_, false))
          
        case i: DefInstance => i.portMap.foreach(p => {
          val (flow, exp) = p match {
            case na: NamedAssign => (Some(na.flow), na.expr)
            case na: NoNameAssign => (Some(na.flow), na.expr)
            case _ => (None, UndefinedExpression())
          }
          
          flow match {
            case None => // nothing to do ; no warning
            case Some(SourceFlow) => visitExpression(exp, false)
            case Some(SinkFlow) => visitExpression(exp, true)
            case _ => 
              exp match {
                case _:UndefinedExpression => // useless to warn user
                case _ => 
                  warn(p, s"Unknown flow for port ${p.serialize} of instance ${i.name}. Treating it as potential Source which might prevent its conversion to UInt")
                  visitExpression(exp, true)
              }
          }
        })
        case _ => 
          s.foreachStmt(visitStatement)
          s.foreachExpr(visitExpression(_, false))
      }
    }
    
    d match {
      case named: HasName => 
        implicit val refStore = new RefStore()
        refStore ++= remoteRefs
        d.foreachStmt(visitStatement)
        store.traceIt()
        storeCollection += named.name -> store
      case _ =>
    }
    d
  }))
  
  // SECOND PASS => use store to update declaration AND SubIndex / SubRange Usage Expressions
  def processDescription(d: Description): Description = {
    
    def processStatement(s: Statement)(implicit store: UsageStore): Statement = {
      s match {
        case d: DefLogic => store.getType(d.name).map(t => d.copy(tpe = t)).getOrElse(d)
        case d: DefType => store.getType(d.name).map(t => d.copy(tpe = t)).getOrElse(d)
        case p: Port => store.getType(p.name).map(t => p.copy(tpe = t)).getOrElse(p)
        case p: DefParam if(p.kind == HwExpressionKind) => store.getType(p.name).map(t => p.copy(tpe = t)).getOrElse(p)
        case f: DefFunction => store.getType(f.name).map(t => f.copy(tpe = t)).getOrElse(f).mapStmt(processStatement)
        case _ => s.mapStmt(processStatement)
      }
    }
    d match {
      case named: HasName => 
        storeCollection.get(named.name) match {
          case Some(store) => d.mapStmt(processStatement(_)(store))
          case _ => 
            fatal(d, s"Unable to retrieve description ${named.name} from local (InferUInt) storage.")
            d
        }
      case _ => d
    }
  }
}