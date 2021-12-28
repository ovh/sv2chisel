package sv2chisel.helpers.tools

import firrtl._
import firrtl.ir._
import firrtl.annotations._

import logger.LazyLogging

import scala.collection.mutable.{HashMap}

/** Apply WhenContext to Register instanciated within these contexts
 *
 *  {{{
 *  val res = Reg(Bool())
 *  when (en) {
 *    val r = Reg(Bool())
 *    r := true.B // get when(en) context thanks to this
 *    transform res := r // always get when(en) context as res is defined outside of WhenContext
 *  } .otherwise {
 *    val rb = Reg(Bool())
 *    rb := true.B // get when(~en) context thanks to this transform
 *    res := rb // always get when(~en) context as res is defined outside of WhenContext
 *  }
 *  }}}
 */
class DeepWhen extends Transform with DependencyAPIMigration with LazyLogging {

  override val prerequisites             = firrtl.stage.Forms.HighForm
  override val dependents                = Seq.empty
  override def invalidates(a: Transform) = false

  /** Stack intended to store the current WhenContext at the current point */
  class CondStack() {
    private var _stack = List.empty[Expression]

    /** Add element on top of the stack */
    def push(e: Expression): Unit = { _stack = e :: _stack }

    /** Remove & return last element above the stack */
    def pop(): Expression = {
      _stack match {
        case Nil    => throw new Error("Unexpected empty stack")
        case t :: q => _stack = q; t
      }
    }

    /** învert the logic expression currently on top of the stack */
    def invert(): Unit = {
      push(DoPrim(PrimOps.Not, Seq(pop()), Seq(), Utils.BoolType))
    }

    /** recursive helper for getFullCond function */
    private def getRec(e: Expression, l: List[Expression]): Expression =
      l match {
        case Nil    => e
        case t :: q => DoPrim(PrimOps.And, Seq(getRec(t, q), e), Seq(), Utils.BoolType)
      }

    /** Retrieve the And-concatened expression corresponding to the whole stack */
    def getFullCond(): Option[Expression] =
      _stack match {
        case Nil    => None
        case t :: q => Some(getRec(t, q))
      }
  }

  /** Transform entry point
   *
   *  @param cs
   *    the circuit
   *  @return
   *    updated circuit
   */
  def execute(cs: CircuitState): CircuitState = {
    val circuitTarget = CircuitTarget(cs.circuit.main)

    /** process given module statement after statement */
    def processModule(m: DefModule): DefModule = {
      val moduleTarget = circuitTarget.module(m.name)

      val condStack = new CondStack()
      val regStore  = HashMap[ReferenceTarget, Expression]()

      /** Stack current logic context before processing associated statement */
      def processConditionally(c: Conditionally): Conditionally = {
        condStack.push(c.pred)
        val conseq = c.conseq.mapStmt(processStatements)
        condStack.invert()
        val alt = c.alt.mapStmt(processStatements)
        condStack.pop()
        c.copy(conseq = conseq, alt = alt)
      }

      /** Add given register to registerStore if a WhenContext is available on Stack */
      def visitRegister(r: DefRegister): Unit = {
        condStack.getFullCond() match {
          case Some(e) => regStore += ((moduleTarget.ref(r.name), e))
          case None    =>
        }
      }

      /** Retrieve ReferenceTarget from complex assignations (LHS only) */
      def getRef(e: Expression): ReferenceTarget = {
        e match {
          case w: WRef      => moduleTarget.ref(w.name)
          case w: WSubField => getRef(w.expr)
          case w: WSubIndex => getRef(w.expr)
          case _            => throw new Error(s"Unexpected assignation to: ${e.serialize}")
        }
      }

      /** Wrap the given Connect if its location targets an element of the regStore */
      def processConnect(c: Connect): Statement = {
        val ref = getRef(c.loc)
        if (regStore.contains(ref)) {
          Conditionally(NoInfo, regStore(ref), c, EmptyStmt)
        } else {
          c
        }
      }

      /** pick up & process relevant Statements for this transform */
      def processStatements(stmt: Statement): Statement = {
        stmt match {
          case c: Conditionally => processConditionally(c)
          case r: DefRegister   => visitRegister(r); r
          case c: Connect       => processConnect(c)
          case _                =>
            // println(s"${stmt.getClass.getName}: ${stmt}")
            stmt.mapStmt(processStatements)
        }
      }
      m.mapStmt(processStatements)
    }
    cs.copy(circuit = cs.circuit.mapModule(processModule))
  }
}
