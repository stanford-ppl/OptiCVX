package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualProjections extends SolverGen with SolverGenUtil {


  def code(Ai: Almap, bi: AVector, Fi: Almap, gi: AVector, ci: AVector, cone: Cone, tol: AVector) {
    
    val x = vector(Ai.domain)
    val s_t = vector(Ai.codomain + Fi.codomain)
    //val xs_t = vector(Ai.domain + Ai.codomain + Fi.codomain)
    val s = vector(Ai.codomain + Fi.codomain)
    val y = vector(Ai.codomain + Fi.codomain)

    val A: Almap = AlmapVCat(-Ai, -Fi)
    //val P: Almap = AlmapHCat(A, AlmapIdentity(Ai.input, Ai.codomain + Fi.codomain))
    val b = vector(A.codomain)
    b := AVectorCat(bi, gi)
    val c = vector(A.domain)
    c := ci
    val K: Cone = cat(zerocone(Ai.codomain), cone)

    val acn = vector(A.domain)
    acn := {
      val i = IRPoly.param(A.arity, A.arity + 1)
      val ei = AVectorZero(A.input.promote, i) ++ AVectorOne(A.input.promote) ++ AVectorZero(A.input.promote, A.domain.promote - i - IRPoly.const(1, A.arity + 1))
      AVectorCatFor(A.domain, AVectorDiv(AVectorOne(A.input.promote), AVectorOne(A.input.promote) + AVectorNorm2(A.promote * ei)))
    }
    val M: Almap = AlmapDiagVector(acn)
    //val M: Almap = AlmapIdentity(A.input, A.domain)

    //val Pproj = new LSQRProject(P, b)
    val Aproj = new CGProject(A, M, b)
    Aproj.tol = 1e-6
    Aproj.itermax = 10

    val cond = scalar
    val cond1 = scalar
    val cond2 = scalar
    val cond3 = scalar

    x := zeros(x.size)
    s := zeros(s.size)
    y := zeros(y.size)
    cond := 1.0

    val Ax = vector(A.codomain)
    val ATy = vector(A.domain)
    val z = vector(x.size)

    converge(cond - tol) {
      // xs_t := Pproj.proj(cat(x - c, s + y), tol)
      // x := slice(xs_t, 0, Ai.domain)
      // s_t := slice(xs_t, Ai.domain, Ai.codomain + Fi.codomain) * 1.8 + s * (1.0 - 1.8)
      x := Aproj.proj(x - c, s + y, x)
      Ax := A * x
      s_t := b - Ax
      s_t := s_t * 1.8 + s * (1.0 - 1.8)
      s := K.project(s_t - y)
      y := y + s - s_t
      ATy := A.T * y
      cond1 := norm_inf(Ax + s - b)
      cond2 := norm_inf(ATy + c)
      cond3 := norm_inf(dot(c, x) + dot(b, y))
      cond := norm_inf(cat(cond1, cond2, cond3))
    }
  }
}

