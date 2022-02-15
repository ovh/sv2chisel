// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel
package transforms

import sv2chisel.ir._
import collection.mutable.HashSet

class ToCamelCase(val options: TranslationOptions) extends DescriptionBasedTransform {
  implicit var srcFile = None
  implicit var stream = None
  
  private val ioTypes = new HashSet[String]()
  
  private def addUserRefType(u:UserRefType):Unit = {
    u.tpe match {
      case b:BundleType => 
        debug(b, s"Adding user type ${u.name}")
        ioTypes += u.name
        addBundleTypeRec(b)
      case _ =>
    }
  }
  
  private def addBundleTypeRec(b:BundleType):Unit = {
    b.fields.foreach(f => {
      f.tpe match {
        case u:UserRefType => addUserRefType(u)
        case b:BundleType => addBundleTypeRec(b)
        case _ => 
      }
    })
  }
  
  private def inspectTypeRec(tpe:Type): Unit = {
    tpe match {
      case u:UserRefType => addUserRefType(u)
      case b:BundleType => addBundleTypeRec(b)
      case v:VecType => inspectTypeRec(v.tpe.head)
      case _ =>
    }
  }
  
  override val preprocessDescription = Some(((d: Description) => {
    d match {
      case m: DefModule if(options.chiselizer.topLevelChiselGenerators.find(_.name == m.name).isDefined) => 
        debug(m, s"Inspection ports of module ${m.name}")
        m.foreachPort(p => inspectTypeRec(p.tpe))
      case _ => 
    }
    d
  }))
  
  // Rendering Goal:
  // class TreeFilterPktDataT extends Bundle {
  //   override def className = "tree_filter_pkt_data_t"
  //   val src_ip = Vec(64, Bool())
  //   def srcIp = src_ip
  
  implicit def stringToSnake(s: String) = new SnakeString(s)
  private def dName(s: String): Option[String] = if(s == s.toCamel) None else Some(s)
  private def dNameC(s: String): Option[String] = if(s == s.toCamelCap) None else Some(s)
  
  def processDescription(d: Description): Description = {
    d match {
      case m: Module if(options.chiselizer.toCamelCase) => 
        val params = m.params.map(p => p.copy(name = p.name.toCamel).mapExpr(processExpression(_)("")))
        m.copy(params = params, body = processStatement(m.body), name = m.name.toCamelCap, desiredName = dNameC(m.name))
        
      case m: ExtModule if(options.chiselizer.toCamelCase) => 
        val params = m.params.map(p => p.copy(name = p.name.toCamel).mapExpr(processExpression(_)("")))
        val paramMap = m.paramMap.map(na => na.copy(expr = processExpression(na.expr)(""))) // do not update name here
        val body = processStatement(m.body)(skipPorts = true) // ports have to be handled in Chiselizer 
        m.copy(params = params, paramMap = paramMap, name = m.name.toCamelCap, body = body, desiredName=dNameC(m.name))
        
      case p: DefPackage if(options.chiselizer.toCamelCase) => 
        p.copy(body = processStatement(p.body), name = p.name.toCamelPkg)
      
      case i: IsolatedStatement if(options.chiselizer.toCamelCase) => 
        i.mapStmt(processStatement)
      
      case d => d
    }
  }
  
  // Common functions
  
  def processExpression(e: Expression)(implicit instName: String): Expression = {
    def processRef(r: Reference): Reference = r.copy(name = r.name.toCamel, path = r.path.map(_.toCamelPkg))
    e match {
      case a: Assign => processAssign(a, preserveName=false)
      case _ =>
        e.mapExpr(processExpression).mapType(processType).mapWidth(_.mapExpr(processExpression)) match {
          case r: Reference if(r.path.headOption.map(_ == instName).getOrElse(false)) => 
            r.copy(name = r.name.toCamel, path = r.path.map(_.toCamel))
          case r: Reference => 
            r.tpe match {
              case _:EnumFieldType => r.copy(name = r.name.toCamelCap, path = r.path.map(_.toCamelCap))
              case _ => processRef(r)
            }
            
          case r: TypeInst => 
            val name = r.tpe match {
              case _:EnumType => r.name.map(_.toCamelCap)
              case _:BundleType => r.name.map(_.toCamelCap)
              case _ => r.name.map(_.toCamel)
            }
            r.copy(name = name, path = r.path.map(_.toCamelPkg))
          case s: SubField => s.copy(name = s.name.toCamel)
          case c: DoCall => c.copy(fun = processRef(c.fun))
          case exp => exp
        }
    }
  }
  
  def processAssign(a: Assign, preserveName: Boolean)(implicit instName: String): Assign = {
    // NB: not required to deal with remoteType at this stage
    a.mapExpr(processExpression) match {
      case na: NamedAssign if(preserveName) => na.copy(assignExpr = na.assignExpr.map(processExpression))
      case na: NamedAssign => na.copy(name = na.name.toCamel, assignExpr = na.assignExpr.map(processExpression))
      case na: NoNameAssign => na.copy(assignExpr = na.assignExpr.map(processExpression))
      case aa => aa
    }
  }
  
  def processType(t: Type)(implicit instName: String): Type = {
    t.mapType(processType).mapWidth(_.mapExpr(processExpression)) match {
      case u: UserRefType => 
        u.tpe match {
          case _:EnumType => u.copy(name = u.name.toCamelCap, path = u.path.map(_.toCamelPkg))
          case _:BundleType => u.copy(name = u.name.toCamelCap, path = u.path.map(_.toCamelPkg))
          case _ => u.copy(name = u.name.toCamel, path = u.path.map(_.toCamelPkg))
        }
      case b: BundleType => b.copy(fields = b.fields.map(f => f.copy(name = f.name.toCamel)))
      case e: EnumType => e.copy(fields = e.fields.map(f => f.copy(name = f.name.toCamelCap)))
      case tpe => tpe 
    }
  }
  
  def processStatement(s: Statement)(implicit skipPorts: Boolean = false): Statement = {
    implicit val instName = ""
    s match {
      // camelCase shall never be applied twice 
      case i: DefInstance => 
        val module = i.module.copy(name = i.module.name.toCamelCap, path = i.module.path.map(_.toCamelPkg))
        val paramMap = i.paramMap.map(p => processAssign(p, preserveName=false)(i.name))
        val preservePortName = currentProject.get.findModule(i.module.serialize) match {
          case Some(e:ExtModule) => !(new ChiselExtModule(e).requireWrapper)
          case _ => false
        }
        val portMap = i.portMap.map(p => processAssign(p, preserveName=preservePortName)(i.name))
        i.copy(name = i.name.toCamel, module = module, paramMap = paramMap, portMap = portMap)
      
      case t: DefType if(ioTypes.contains(t.name)) => 
        t.tpe match {
          case b:BundleType => 
            info(t, s"Special ioType transform for deftype ${t.name}")
            val tpe = b.copy(fields = b.fields.map(f => {
              f.copy(name = f.name.toCamel, desiredName = dName(f.name), tpe = processType(f.tpe))
            }))
            t.copy(name = t.name.toCamelCap, tpe = tpe, desiredName = dNameC(t.name))
          
          case _ => 
            warn(t, s"IGNORING Special ioType transform for non-bundle deftype ${t.name}")
            t.copy(name = t.name.toCamel).mapExpr(processExpression).mapType(processType)
        }
      
      case _ => 
        s.mapStmt(processStatement).mapExpr(processExpression).mapType(processType) match {
          case i: ImportPackages => i.copy(packages = i.packages.map(p => {
            p.copy(item = (if(p.item == "_") "_" else p.item.toCamel), path = p.path.toCamelPkg)
          }))
          case l: DefLogic => l.copy(name = l.name.toCamel)
          case p: DefParam => p.copy(name = p.name.toCamel)
          case t: DefType => 
            t.tpe match {
              case _:EnumType => t.copy(name = t.name.toCamelCap)
              case _:BundleType => t.copy(name = t.name.toCamelCap)
              case _ => t.copy(name = t.name.toCamel)
            }

          case f: DefFunction => f.copy(name = f.name.toCamel)
          case p: Port if(!skipPorts) => p.copy(name = p.name.toCamel, desiredName = dName(p.name))
          case st => st 
        }
    }
  }
}