package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq

trait SolverGen {

  def code(A: Almap, b: AVector, F: Almap, g: AVector, c: AVector, cone: Cone, tol: AVector): Unit

  private var input: InputDesc = null
  private var variables: Seq[MemoryArgDesc] = null
  private var vidx: Int = 0
  private var prephase: Boolean = true
  private var solveracc: Solver = null

  case class SVariable(iidx: Int) {
    def apply(sar: IRPoly*): SVariableEntry = SVariableEntry(iidx, Seq(sar:_*))
  }

  case class SVariableEntry(iidx: Int, sidx: Seq[IRPoly]) {
    if(variables(iidx).dims.length != sidx.length) throw new IRValidationException()

    def :=(src: AVector) {
      if(!prephase) {
        solveracc = SolverSeq(solveracc, SolverWrite(src, iidx, sidx))
      }
    }
    def +=(src: AVector) {
      this := svariableentry2vectorimpl(this) + src
    }
    def -=(src: AVector) {
      this := svariableentry2vectorimpl(this) - src
    }
  }

  implicit def svariable2svariableentryimpl(s: SVariable): SVariableEntry = {
    if(variables(s.iidx).dims.length != 0) throw new IRValidationException()
    SVariableEntry(s.iidx, Seq())
  }

  implicit def svariable2vectorimpl(s: SVariable): AVector = svariableentry2vectorimpl(s())

  implicit def svariableentry2vectorimpl(s: SVariableEntry): AVector = {
    if(prephase) {
      // in prephase, all reads result in zero because the memory isn't defined
      AVectorCatFor(variables(s.iidx).size.substituteSeq(s.sidx), AVectorOne(input.promote))
    }
    else {
      AVectorRead(input, s.iidx, s.sidx)
    }
  }

  def v2m(v: AVector): Almap = AlmapVector(v)

  def hcat(as: Almap*): Almap = {
    if(as.length == 0) {
      throw new IRValidationException()
    }
    else {
      as.drop(1).foldLeft(as(0))((b,a) => AlmapHCat(b, a))
    }
  }
  def vcat(as: Almap*): Almap = {
    if(as.length == 0) {
      throw new IRValidationException()
    }
    else {
      as.drop(1).foldLeft(as(0))((b,a) => AlmapVCat(b, a))
    }
  }
  def cat(xs: AVector*): AVector = {
    if(xs.length == 0) {
      throw new IRValidationException()
    }
    else {
      xs.drop(1).foldLeft(xs(0))((b,a) => AVectorCat(b, a))
    }
  }
  def cat(xs: Cone*): Cone = {
    if(xs.length == 0) {
      throw new IRValidationException()
    }
    else {
      xs.drop(1).foldLeft(xs(0))((b,a) => ConeProduct(b, a))
    }
  }

  def slice(arg: AVector, at: IRPoly, size: IRPoly) = AVectorSlice(arg, at, size)

  def zeros(d: IRPoly, c: IRPoly): Almap = AlmapZero(input, d, c)
  def eye(d: IRPoly): Almap = AlmapIdentity(input, d)
  def zeros(l: IRPoly): AVector = AVectorZero(input, l)
  def ones(l: IRPoly): AVector = AVectorCatFor(l, AVectorOne(input.promote))
  def norm2(arg: AVector) = AVectorNorm2(arg)
  def norm_inf(arg: AVector) = AVectorNormInf(arg)
  def sqrt(arg: AVector) = AVectorSqrt(arg)
  def dot(arg1: AVector, arg2: AVector) = AVectorDot(arg1, arg2)

  def zerocone(d: IRPoly): Cone = ConeFor(d, ConeZero(input.arity + 1))
  def freecone(d: IRPoly): Cone = ConeFor(d, ConeFree(input.arity + 1))

  implicit def int2irpolyimpl(i: Int): IRPoly = IRPoly.const(i, input.arity) 

  implicit def double2vector(x: Double): AVector = 
    AVectorScaleConstant(AVectorOne(input), x)

  def scalar: SVariable = vector(IRPoly.const(1, input.arity))
  def vector(len: IRPoly): SVariable = {
    if(prephase) {
      variables :+= MemoryArgDesc(Seq(), len)
      SVariable(variables.length - 1)
    }
    else {
      if(variables(vidx) != MemoryArgDesc(Seq(), len)) throw new IRValidationException()
      vidx += 1
      SVariable(vidx - 1)
    }
  }

  def converge(condition: AVector, itermax: Int)(body: =>Unit) {
    if(condition.size != IRPoly.const(1, input.arity)) throw new IRValidationException()
    if(prephase) {
      body
    }
    else {
      val cursolver: Solver = solveracc
      solveracc = SolverNull(input)
      body
      solveracc = SolverSeq(cursolver, SolverConverge(condition, itermax, solveracc))
    }
  }

  def converge(condition: AVector)(body: =>Unit) {
    converge(condition, -1)(body)
  }

  def gen(problem: Problem): Solver = {
    if(!problem.isMemoryless) throw new IRValidationException()

    val A: Almap = problem.affineAlmap
    val b: AVector = problem.affineOffset
    val F: Almap = problem.conicAlmap
    val g: AVector = problem.conicOffset
    val c: AVector = problem.objective
    val cone: Cone = problem.conicCone

    input = problem.input
    variables = Seq()
    prephase = true
    solveracc = null

    code(A, b, F, g, c, cone, AVectorTolerance(problem.input))

    if(variables(0) != MemoryArgDesc(Seq(), problem.varSize)) throw new IRValidationException()
    input = InputDesc(problem.arity, problem.input.args, variables)
    prephase = false
    vidx = 0
    solveracc = SolverNull(input)

    code(A.addMemory(variables), b.addMemory(variables), F.addMemory(variables), g.addMemory(variables), c.addMemory(variables), cone, AVectorTolerance(input))

    if(vidx != variables.length) throw new IRValidationException()

    val rv = solveracc

    input = null
    variables = null
    prephase = true
    solveracc = null
    
    rv
  }
}
