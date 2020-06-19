// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

/*
 All this transform is to be redone
 - SourceFlow = RHS = some element being read, used / assign from
 - SinkFlow = LHS = some element being written / assign towards

 To be done in the following way
 
 NOTE: important, references shall be managed as in inferuints:
       with virtual references such as vec[_], matrix[_][_] 
 
 NOTE: due to repeat, push warning into a Set, so they are all printed at once at 
       the end of the the transform, without duplicate
       could use a "done" dictionary to lookup for and do not process/print msg for "done" refs   
 
Repeat until updated map is empty
  - clear updated map
  - Keep for each reference R:  
    - Seq[A: assignations (R as SinkFlow)]
    - Seq[B usages (R as SourceFlow)]
    - Seq[C Unknown: R as potential Sink OR Flow] (only assigns in IOs of DefInstance)
   => NOTE: Pass#1 this is already a process path because assignations can be applied in place, as for source usage
   => NOTE: no need to count again / modify for ref in "DONE" 
    
  - Analyse results: (A, B, C) match
    - (Seq(), Seq(), Seq()) => warn unused reference
    - (Seq(), Seq(), Seq(e)) => warn cannot conclude anything but treat as if R is assigned by e  
    - (Seq(), Seq(), s) => warn cannot conclude anything and do not update anything
    - (Seq(), s, Seq(e)) => e is the place where R should be referenced as Sink (assigned by submodule output)
    - (Seq(e), _, e:_) if assumeValidVerilog => foreach elt in e => R should be treated as source in elt
    - (Seq(e), _, e:_) => warn cannot conclude anything
    - (s, Seq(), Seq()) => warn declared & assigned but left unconnected
    - (Seq(), s, Seq()) => warn undeclared ref
    - (Seq(e), s, Seq()) => // nothing to do 
    - (s, _, e:_) => if asssumeValidVerilog => warn multiple assigns but assume R as Source in each elt of e
    - (s, _, e:_) => debug nothing can be deduced for now for r flow in elmts of e
  
  - Apply update
    - this might be the tricky one => how to find the right usage ?
      - well it is actually dead simple: all Reference to R in updated map with Unknown FLow in DefInstance Assign are to be updated.
      
*/


package sv2chisel
package transforms

import sv2chisel.ir._
import sv2chisel.Utils._

import collection.mutable.{HashMap, HashSet, ArrayBuffer, LinkedHashMap}

class FlowReferences(
    val llOption: Option[logger.LogLevel.Value] = None, 
    val assumeValidVerilog: Boolean = true) extends DescriptionBasedTransform {
  private val ui = UndefinedInterval
  private val nv = NoVerilogAttribute
  
  class ModuleStore() {
    class ModulePorts(val name: String){
      val declaration = new LinkedHashMap[String, Assign]()
      val namedPorts = new LinkedHashMap[String, Assign]()
      val noNamePorts = new ArrayBuffer[Assign]()
      
      private def getPortsKind(p: Seq[Assign], expected: Option[Assign]): Assign = {
        (p, expected) match {
          case (Seq(), None) => throwInternalError("Unexpected empty port list")
          case (Seq(), Some(a)) => a
          case (s, None) => getPortsKind(s.tail, Some(s.head))
          case (s, Some(e)) =>
            val exp = (e, s.head) match {
              case (_: NamedAssign, _:NamedAssign) => e
              case (_: NoNameAssign, _:NoNameAssign) => e
              case _ => throwInternalError(s"Incompatible parameter list: got both $e and ${s.head} in same list.")
            }
            getPortsKind(s.tail, Some(exp))
        }
      }
      
      private def getSeqFlows(s: Seq[Assign], ports: Seq[Assign]): Seq[Flow] = {
        val missingEntries = ports.size - s.size
        val ref = s ++ Seq.fill(missingEntries)(NoNameAssign(ui, UndefinedExpression(), UnknownFlow))

        ref.zip(ports).map(t => {
          (t._1.flow, t._2.flow) match {
            case (UnknownFlow, f2) => f2 
            case (f1, UnknownFlow) => f1
            case (f1, f2) if (f1 == f2) => f1
            case (f1, f2) if (f1 != f2) => 
              critical(t._2, s"Unexpected direction for port ${t._2} where ${t._1} was expected. (Expression are given for context, only mismatching Flows are causing this message)")
              f1
          }
        })
      }
      
      private def getNamedFlows(s: LinkedHashMap[String, Assign], ports: Seq[Assign]): Seq[Flow] = {
        ports.map(p => {
          p match {
            case na: NamedAssign => 
              s.get(na.name) match {
                case Some(a) => a.flow
                case None => UnknownFlow
              }
            case _ => throwInternalError("Should not be here") 
          }
        })
      }
      
      def declare(ports: Seq[Port]): Unit = {
        ports match {
          case Seq() => info(s"Declaring actual port directions for module $name")
          case s => info(s.head, s"Declaring actual port directions for module $name")
        }
        
        val decl = ports.map(p => {
          val flow = p.direction match {
            case _:Input => SourceFlow // NOTE: the connected expr shall be a source flow  
            case _:Output => SinkFlow
            case _ => UnknownFlow
          }
          declaration += ((p.name, NamedAssign(ui, p.name, UndefinedExpression(), flow)))
        })
      }
      
      def registerInference(ports: Seq[Assign]) : Unit = {
        // do not risk error while the result won't be used anyway
        if(declaration.isEmpty){
          getPortsKind(ports, None) // just ensure they are all the same type 
          ports.zipWithIndex.map(t => {
            t._1 match {
              case na: NamedAssign => 
                (namedPorts.get(na.name), na.flow) match {
                  // nothing to update
                  case (_, UnknownFlow) => 
                  case (Some(a), f) if(a.flow == f) => 
                  
                  // DO UPDATE
                  case (Some(NamedAssign(_, _, _, UnknownFlow)), f) => namedPorts(na.name) = na
                  case (None, f) => namedPorts += ((na.name, na))
                  
                  // ERROR on defined flow override
                  case (Some(a), f) if(a.flow != f) => 
                    critical(na, s"Ignored attempt to update direction for port ${na.name} from ${a.flow} to $f.")
                }
              
              case na: NoNameAssign => 
                (noNamePorts(t._2), na.flow) match {
                  // nothing to update
                  case (_, UnknownFlow) => 
                  case (a, f) if(a.flow == f) =>
                  
                  // DO UPDATE
                  case (NoNameAssign(_, _, UnknownFlow), f) => 
                    if(t._2 < noNamePorts.size) {
                      noNamePorts(t._2) = na
                    } else { 
                      noNamePorts += na
                    }
                  
                  // ERROR on defined flow override
                  case (a, f) if(a.flow != f) => 
                    critical(na, s"Ignored attempt to update direction for port ${t._2} from ${a.flow} to $f.")
                }
              case _ => // do nothing
            }
          })
        }
      }
      
      def getExpectedFlows(ports: Seq[Assign]): Seq[Flow] = {
        //first look for actual module declaration
        val kind = getPortsKind(ports, None)
        (declaration.isEmpty, namedPorts.isEmpty, noNamePorts.isEmpty, kind) match {
          case (false, _, _, _: NamedAssign) => getNamedFlows(declaration, ports)
          case (false, _, _, _: NoNameAssign) => getSeqFlows(declaration.values.toSeq, ports)
          case (_, false, _, _: NamedAssign) => getNamedFlows(namedPorts, ports)
          case (_, false, _, _: NoNameAssign) => getSeqFlows(namedPorts.values.toSeq, ports)
          case (_, _, false, _: NamedAssign) => ports.map(p => UnknownFlow)
          case (_, _, false, _: NoNameAssign) => getSeqFlows(noNamePorts, ports)
          case _ => ports.map(p => UnknownFlow)
        }
      }
    }
    
    private val store = new HashMap[String, ModulePorts]
    
    def register(m: DefModule): Unit = {
      store.get(m.name) match {
        case Some(mp) => throwInternalError(s"Unexpected duplicated declaration for module ${m.name}")
        case None =>
          val mp = new ModulePorts(m.name)
          mp.declare(m.ports)
          store += ((m.name, mp))
      }
    }
    
    def registerInference(name: String, ports: Seq[Assign]) : Unit = {
      store.get(name) match {
        case Some(mp) => mp.registerInference(ports)
        case None => 
          val mp = new ModulePorts(name)
          mp.registerInference(ports)
          store += ((name, mp))
      }
    }
    
    def getExpectedFlows(name: String, ports: Seq[Assign]): Seq[Flow] = { // Assign or flow ?
      store.get(name) match {
        case Some(mp) => mp.getExpectedFlows(ports)
        case None => ports.map(p => UnknownFlow)
      }
    }
  }
  val moduleStore = new ModuleStore()
  
  override val preprocessDescription = Some(((d: Description) => {
    d match {
      case m: Module => moduleStore.register(m); m
      case d => d
    }
  }))
  
  def processDescription(d: Description): Description = {
    d match {
      case m: Module => processModule(m)
      case d => d
    }
  }
      
  def processModule(m: DefModule): DefModule = {
    val extAssigned = new HashMap[String, Boolean]()
    val inAssigned = new HashSet[String]()
    val implicitSources = new HashSet[String]()
    val wasInferredSink = new HashSet[String]()
    var newImplicitSources = false
    
    // Note: only useful for lhs refs (no doPrim operations except ignored concat)
    def getRef(e: Expression): Option[Reference] = {
      e match {
        case r: Reference => Some(r)
        case s: SubField => getRef(s.expr) // assuming all subfield with same dir
        case s: SubIndex => getRef(s.expr)
        case s: SubRange => getRef(s.expr)
        case c: Concat => None
        // most of it
        case _ => None
      }
    }
    
    // first pass => hashmap with assignation to references
    def visitConnect(c: Connect): Unit = {
      getRef(c.loc) match {
        case None =>
        case Some(r) => 
          val ref = r.serialize
          
          extAssigned.get(ref) match {
            case Some(b) => // nothing to do
            // registering as inAssigned => not as strong as extAssigned
            case None => inAssigned += ref
          }
      }
    }
    
    def visitDefParam(l: DefParam): Unit = {
      if (extAssigned.contains(l.name)) {
        warn(l, s"Multiple definitions of ${l.name}")
      } else {
        extAssigned += ((l.name, true))
      }
    }
    
    def visitStatement(s: Statement): Unit = {
      s match {
        case c: DefParam => visitDefParam(c)
        case c: Connect => visitConnect(c)
        case _ => s.foreachStmt(visitStatement)
      }
    }
    
    def visitPort(p: Port): Unit = {
      if (extAssigned.contains(p.name)) {
        warn(p, s"Multiple definitions of ${p.name}")
      } else {
        p.direction match {
          case _: Output => extAssigned += ((p.name, false))
          case _ => extAssigned += ((p.name, true))
        }
      }
    }
    
    m.foreachPort(visitPort)
    // TO DO : we are missing includes here ...
    m.foreachParam(visitDefParam)
    m.foreachStmt(visitStatement)
    
    def getFlow(r: Reference, expected: Option[Flow]): Flow = {
      val ref = r.serialize
      val inAssign = inAssigned.contains(ref)
      val imply = implicitSources.contains(ref)
      val sink = wasInferredSink.contains(ref) // assume file ordering
      
      (extAssigned.get(ref), inAssign, imply, sink, expected) match {
        case (None, false, false, false, None) => UnknownFlow
        
        // back-propagated sink/source over previously UnknownFlow
        case (None, false, false, false, Some(SinkFlow)) => wasInferredSink += ref; SinkFlow
        
        // reuse of elsewhere used as sink signal
        case (None, false, _, true, None) => SourceFlow
        
        // submodule output 
        case (None, false, true, false,  None) => 
          wasInferredSink += ref
          SinkFlow 
          
        case (None, true, true, _, None) => throwInternalError("Impossible by design")
        case (None, true, false, _, None) => SourceFlow
        // known inputs & params 
        case (Some(true), _, _, _, Some(SinkFlow)) => SinkFlow // basically all connects
        case (Some(true), _, _, _, _) => SourceFlow
        
        // known outputs
        case (Some(false), _, _, _, Some(f)) => f // outputs signal used in computations 
        case (Some(false), _, _, _, _) => SinkFlow
        
        // implicit source (from doprim, call or cast)
        case (None, false, false, _, Some(SourceFlow)) if (assumeValidVerilog) => 
          debug(r, s"Adding implicit source $ref")
          implicitSources += ((ref))
          newImplicitSources = true
          SourceFlow
          
        case (None, _, _, _, Some(f)) => f
      }
    }
    def reduceFlow(f1: Flow, f2: Flow): Flow = { 
      (f1, f2) match {
        case (SourceFlow, SourceFlow) => SourceFlow 
        case (SinkFlow, SinkFlow) => SinkFlow 
        case (SinkFlow, UnknownFlow) if(assumeValidVerilog) => SinkFlow 
        case (UnknownFlow, SinkFlow) if(assumeValidVerilog) => SinkFlow 
        case (SourceFlow, UnknownFlow) if(assumeValidVerilog) => SourceFlow 
        case (UnknownFlow, SourceFlow) if(assumeValidVerilog) => SourceFlow 
        case _ => UnknownFlow
      }
    }
    
    def backPropagateFlow(e: Expression, f: Flow): Expression = {
      e match {
        case r: Reference => 
          (r.flow, f) match {
            case (UnknownFlow, SinkFlow) => 
              wasInferredSink += (r.serialize)
              debug(e, s" > Recording that ${r.serialize} was used as sink")
            case _ =>
          }
          r.copy(flow = f)
        case s: SubField => 
          val expr = backPropagateFlow(s.expr, f)
          s.copy(expr = expr, flow = expr.flow)
          
        case s: SubIndex =>
          val expr = backPropagateFlow(s.expr, f)
          s.copy(expr = expr, flow = expr.flow)
          
        case s: SubRange => 
          val expr = backPropagateFlow(s.expr, f)
          s.copy(expr = expr, flow = expr.flow)
        
        case _ => e 
      }
    }
    
    // SECOND PASS => apply flow to expressions
    var instCount = 0
    def processExpression(e: Expression, expected: Option[Flow]): Expression = {
      e match {
        case u: UndefinedExpression => 
          // important for Port Emission (connecting DontCare to Undefined Inputs)
          expected match {
            case Some(f) => u.copy(flow = f)
            case _ => u
          }
          
        case r: Reference => r.copy(flow = getFlow(r, expected))
        case s: SubField => 
          // flows should be applied only to bundle fields not whole bundle
          // additionnaly direction should be involved ? (not sure it is useful for verilog)
          // Note that for verilog this should be fine => in any field of a struct is assigned it means the whole strcut should be assigned in the same place
          // Edge case are definitely not covered though
          // struct defined within module ; 1 field assigned from input port another assigned from submodule output
          val expr = processExpression(s.expr, expected)
          s.copy(expr = expr, flow = expr.flow)
          
        case s: SubIndex =>
          // above notice concerning bundle somehow apply also here ...
          // definitely need to introduce some kind of reference target management
          val expr = processExpression(s.expr, expected)
          s.copy(expr = expr, flow = expr.flow)
          
        case s: SubRange => 
          // above notice concerning bundle somehow apply also here ...
          // definitely need to introduce some kind of reference target management
          val expr = processExpression(s.expr, expected)
          s.copy(expr = expr, flow = expr.flow)
          
        case d: DoPrim => // all ops work the same
          // in valid verilog all operand of a primary operation are assumed to be SourceFlow
          // assuming valid verilog: use this information to back propagate info to reference
          val ar = d.args.map(processExpression(_, Some(SourceFlow)))
          ar.map(_.flow).reduce(reduceFlow) match {
            case SourceFlow => // fine 
            case SinkFlow => critical(d, s"Ignored SinkFlow infered for primary op : ${d.serialize}")
            case _ => warn(d, s"Unable to infer flow for ${d.serialize}. Note: Primary operator are always SourceFlow.")
          }
          d.copy(args= ar)
        
        case c: Concat =>
          val ar = c.args.map(processExpression(_, expected))
          val flow = ar.map(_.flow).reduce(reduceFlow)
          // back propagate over UnknownFlow if any 
          val backPropagatedArgs = (expected, flow) match {
            case (None, UnknownFlow) => ar
            case (None, SinkFlow) => ar.map(backPropagateFlow(_, SinkFlow))
            case _ => ar
          }
          c.copy(args= backPropagatedArgs, flow = flow)
          
        case a: NamedAssign => 
          val expr = processExpression(a.expr, expected)
          a.copy(expr = expr, flow = expr.flow)
          
        case a: NoNameAssign => 
          val expr = processExpression(a.expr, expected)
          a.copy(expr = expr, flow = expr.flow)
        
        case _ => e
      }
    }
    
    def processAssign(a: Assign, expected: Option[Flow]): Assign = {
      a match {
        case aa: AutoAssign => a
        case na: NamedAssign => 
          val expr = processExpression(na.expr, expected)
          na.copy(expr = expr, flow = expr.flow)
        case na: NoNameAssign => 
          val expr = processExpression(na.expr, expected)
          na.copy(expr = expr, flow = expr.flow)
      }
    }
    
    def processInstance(d: DefInstance): DefInstance = {
      instCount +=1
      val params = d.paramMap.map(processAssign(_, Some(SourceFlow)))
      // the whole trick to auto infer ports directions lies here 
      // we need not to erase what was decided previously
      val exp = moduleStore.getExpectedFlows(d.module.serialize, d.portMap)
      val ports = d.portMap.zip(exp).map(t => {
        (t._1.flow, t._2) match {
          case (UnknownFlow, UnknownFlow) => processAssign(t._1, None) 
          case (UnknownFlow, f) => processAssign(t._1, Some(f))
          case _ => t._1
        }
      })
      moduleStore.registerInference(d.module.serialize, ports)
      d.copy(paramMap = params, portMap = ports)
    }
    
    def processStatement(s: Statement): Statement = {
      s match {
        case c: Connect =>
          val loc = processExpression(c.loc, Some(SinkFlow))
          val expr = processExpression(c.expr, Some(SourceFlow))
          c.copy(loc = loc, expr = expr)
        
        case d: DefInstance => processInstance(d)  
        
        case _ => s.mapExpr(processExpression(_, Some(SourceFlow))).mapStmt(processStatement)
      }
    }
    
    var result: DefModule = m.mapStmt(processStatement)
    var maxIter = 3
    while((newImplicitSources || !wasInferredSink.isEmpty) && maxIter > 0){
      info(m, s"Running FlowReference Transform another time on module ${m.name}")
      newImplicitSources = false
      instCount = 0
      wasInferredSink.foreach(ref => extAssigned += ((ref, true)))
      wasInferredSink.clear()
      maxIter -= 1
      result = result.mapStmt(processStatement)
    }
    if (newImplicitSources || !wasInferredSink.isEmpty) {
      critical(m, "Unable to completely achieve FlowReference Transform within the 4 successives runs.")
    }
    
    def debugPrintInstances(s:Statement): Unit = {
      s match {
        case d: DefInstance => debug(d, d.serialize)
        case _ => s.foreachStmt(debugPrintInstances)
      }
    }
    if(instCount > 0){
      debug(s"### Detail of instances in module ${result.name}: ")
      result.foreachStmt(debugPrintInstances)
    } else {
      debug(s"### No Instances in module ${result.name}.")
    }
    result
  }
}