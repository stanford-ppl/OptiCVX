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
    val b: AVector = AVectorCat(bi, gi)
    val c: AVector = ci
    val K: Cone = cat(zerocone(Ai.codomain), cone)

    //val Pproj = new LSQRProject(P, b)
    val Aproj = new CGProject(A, AlmapIdentity(A.input, A.domain), b)
    Aproj.tol = tol

    val cond = scalar
    val cond1 = scalar
    val cond2 = scalar
    val cond3 = scalar

    x := zeros(x.size)
    s := zeros(s.size)
    y := zeros(y.size)

    converge(cond - tol) {
      // xs_t := Pproj.proj(cat(x - c, s + y), tol)
      // x := slice(xs_t, 0, Ai.domain)
      // s_t := slice(xs_t, Ai.domain, Ai.codomain + Fi.codomain) * 1.8 + s * (1.0 - 1.8)
      x := Aproj.proj(x - c, s + y, x)
      s_t := b - A * x
      s := K.project(s_t - y)
      y := y + s - s_t
      cond1 := norm_inf(A*x + s - b)
      cond2 := norm_inf(A.T * y + c)
      cond3 := norm_inf(dot(c, x) + dot(b, y))
      cond := norm_inf(cat(cond1, cond2, cond3))
    }
  }
}

