package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualProjections extends SolverGen with SolverGenUtil {


  def code(Ai: Almap, bi: AVector, Fi: Almap, gi: AVector, ci: AVector, cone: Cone, tol: AVector): AVector = {

    val A: Almap = AlmapVCat(-Ai, -Fi)
    //val P: Almap = AlmapHCat(A, AlmapIdentity(Ai.input, Ai.codomain + Fi.codomain))
    val b = AVectorCat(bi, gi)
    val c = ci
    val K: Cone = cat(zerocone(Ai.codomain), cone)

    val acn: AVector = {
      val i = IRPoly.param(A.arity, A.arity + 1)
      val ei = AVectorZero(A.input.promote, i) ++ AVectorOne(A.input.promote) ++ AVectorZero(A.input.promote, A.domain.promote - i - IRPoly.const(1, A.arity + 1))
      AVectorCatFor(A.domain, AVectorDiv(AVectorOne(A.input.promote), AVectorOne(A.input.promote) + AVectorNorm2(A.promote * ei)))
    }
    val M: Almap = AlmapDiagVector(acn)
    //val M: Almap = AlmapIdentity(A.input, A.domain)

    //val Pproj = new LSQRProject(P, b)

    object PDPConverge extends Converge {
      val x = state(zeros(A.domain))
      val s = state(zeros(A.codomain))
      val y = state(zeros(A.codomain))

      def body: AVector = {
        val Aproj = new CGProject(pr(A), pr(M), pr(b))
        Aproj.tol = 1e-6
        Aproj.itermax = 10
        x := Aproj.proj(x - pr(c), s + y, x)
        val prA = pr(A)
        val Ax = pr(A) * x
        val s_t = pr(b) - Ax
        val s_t_ovp = s_t * 1.8 + s * (1.0 - 1.8)
        s := K.project(s_t_ovp - y)
        y := y + s - s_t_ovp
        val ATy = pr(A.T) * y
        val cond1 = norm_inf(Ax + s - pr(b))
        val cond2 = norm_inf(ATy + pr(c))
        val cond3 = norm_inf(dot(pr(c), x) + dot(pr(b), y))

        norm_inf(cat(cond1, cond2, cond3)) - pr(tol)
      }
    }

    PDPConverge.run

    PDPConverge.x
  }
}


