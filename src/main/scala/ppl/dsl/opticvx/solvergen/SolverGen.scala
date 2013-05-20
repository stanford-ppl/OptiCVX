package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq

trait SolverGen {

  def code(A: Almap, b: AVector, F: Almap, g: AVector, c: AVector, cone: Cone, tol: AVector): AVector

  private var input: InputDesc = null

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

  def elemmpy(arg1: AVector, arg2: AVector): AVector = {
    if(arg1.size != arg2.size) throw new IRValidationException()
    val idx = IRPoly.param(arg1.arity, arg1.arity + 1)
    val ione = IRPoly.const(1, arg1.arity + 1)
    AVectorCatFor(arg1.size, AVectorMpy(AVectorSlice(arg1.promote, idx, ione), AVectorSlice(arg2.promote, idx, ione)))
  }

  def elemdiv(arg1: AVector, arg2: AVector): AVector = {
    if(arg1.size != arg2.size) throw new IRValidationException()
    val idx = IRPoly.param(arg1.arity, arg1.arity + 1)
    val ione = IRPoly.const(1, arg1.arity + 1)
    AVectorCatFor(arg1.size, AVectorDiv(AVectorSlice(arg1.promote, idx, ione), AVectorSlice(arg2.promote, idx, ione)))
  }

  def diag(arg: AVector): Almap = AlmapDiagVector(arg)

  def zerocone(d: IRPoly): Cone = ConeFor(d, ConeZero(input.arity + 1))
  def freecone(d: IRPoly): Cone = ConeFor(d, ConeFree(input.arity + 1))

  implicit def int2irpolyimpl(i: Int): IRPoly = IRPoly.const(i, input.arity) 

  implicit def double2vector(x: Double): AVector = 
    AVectorScaleConstant(AVectorOne(input), x)

  /*
  def converge(arg: AVector, itermax: Int)(body: AVector => (AVector, AVector)): AVector = {
    val oldinput = input
    input = InputDesc(input.arity, input.args, input.memory :+ arg.size)
    val (cond, bx) = body(AVectorRead(input, oldinput.memory.length))
    input = oldinput
    AVectorConverge(arg, cond, bx)
  }

  def converge(arg: AVector)(body: AVector => (AVector, AVector)): AVector = {
    converge(arg, -1)(body)
  }
  */

  class SVar(val init: AVector) {
    private var value: AVector = init
    def :=(x: AVector) {
      value = x
    }
    def get: AVector = value
  }
  implicit protected def svar2avector(s: SVar): AVector = s.get

  trait Converge {
    private var svars: Seq[SVar] = Seq()
    protected def state(init: AVector): SVar = {
      if(init.input != input) throw new IRValidationException()
      val rv = new SVar(init)
      svars = svars :+ rv
      rv
    }
    def pr[T](x: HasInput[T]): T = {
      val isv: IRPoly = svars.foldLeft(IRPoly.const(0, input.arity))((a, u) => a + u.init.size)
      if(x.input.pushMemory(isv) != input) throw new IRValidationException()
      x.pushMemory(isv)
    }
    def body: AVector
    val itermax: Int = -1
    def run {
      val isv: IRPoly = svars.foldLeft(IRPoly.const(0, input.arity))((a, u) => a + u.init.size)
      val pmx: AVector = AVectorRead(input.pushMemory(isv), input.memory.length)
      var ilx: IRPoly = IRPoly.const(0, input.arity)
      for(s <- svars) {
        s := AVectorSlice(pmx, ilx, s.init.size)
        ilx = ilx + s.init.size
      }
      val oldinput = input
      input = input.pushMemory(isv)
      val cond = body
      input = oldinput
      val bsx: AVector = AVectorCat(svars map (s => s.get))
      val isx: AVector = AVectorCat(svars map (s => s.init))
      val ccx: AVector = AVectorConverge(isx, cond, bsx, itermax)
      ilx = IRPoly.const(0, input.arity)
      for(s <- svars) {
        s := AVectorSlice(ccx, ilx, s.init.size)
        ilx = ilx + s.init.size
      }
    }
  }

  def gen(problem: Problem): AVector = {
    if(!problem.isMemoryless) throw new IRValidationException()

    val A: Almap = problem.affineAlmap
    val b: AVector = problem.affineOffset
    val F: Almap = problem.conicAlmap
    val g: AVector = problem.conicOffset
    val c: AVector = problem.objective
    val cone: Cone = problem.conicCone

    input = problem.input

    val rv = code(A, b, F, g, c, cone, AVectorTolerance(problem.input))

    if(rv.input != problem.input) throw new IRValidationException()
    if(rv.size != problem.varSize) throw new IRValidationException()
    
    rv.simplify
  }
}
