package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualProjections extends SolverGen with SolverGenUtil {


  def code(Ai: Almap, bi: AVector, Fi: Almap, gi: AVector, ci: AVector, cone: Cone, tol: AVector): AVector = {

    var A: Almap = AlmapVCat(-Ai, -Fi)
    //val P: Almap = AlmapHCat(A, AlmapIdentity(Ai.input, Ai.codomain + Fi.codomain))
    var b: AVector = AVectorCat(bi, gi)
    var c: AVector = ci
    val K: Cone = cat(zerocone(Ai.codomain), cone)

    val avDd: AVector = {
      val i = IRPoly.param(A.arity, A.arity + 1)
      val ei = AVectorZero(A.input.promote, i) ++ AVectorOne(A.input.promote) ++ AVectorZero(A.input.promote, A.domain.promote - i - IRPoly.const(1, A.arity + 1))
      AVectorCatFor(A.domain, AVectorPow(AVectorDiv(AVectorNormOne(A.promote * ei), AVectorFromInt(A.input, A.domain).promote), AVectorDiv(AVectorFromInt(A.input, A.domain), AVectorFromInt(A.input, A.domain + A.codomain)).promote))
    }
    val avDpl: AVector = {
      val i = IRPoly.param(A.arity, A.arity + 1)
      val ei = AVectorZero(A.input.promote, i) ++ AVectorOne(A.input.promote) ++ AVectorZero(A.input.promote, A.codomain.promote - i - IRPoly.const(1, A.arity + 1))
      AVectorCatFor(A.codomain, AVectorPow(AVectorDiv(AVectorNormOne(A.T.promote * ei), AVectorFromInt(A.input, A.codomain).promote), AVectorDiv(AVectorFromInt(A.input, A.codomain), AVectorFromInt(A.input, A.domain + A.codomain)).promote))
    }
    val avDp: AVector = K.average_under(avDpl)
    val Dd: Almap = AlmapDiagVectorInv(avDd)
    val Ddinv: Almap = AlmapDiagVector(avDd)
    val Dp: Almap = AlmapDiagVectorInv(avDp)
    val Dpinv: Almap = AlmapDiagVector(avDp)

    A = Dp * A * Dd
    c = Dd * c
    b = Dp * b

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
        val prA = pr(A)
        val Aproj = new CGProject(prA, pr(M), pr(b))
        Aproj.tol = 1e-4
        Aproj.itermax = 20
        x := Aproj.proj(x - pr(c), s + y, x)
        val Ax = prA * x
        val s_t = pr(b) - Ax
        val s_t_ovp = s_t * 1.8 + s * (1.0 - 1.8)
        s := K.project(s_t_ovp - y)
        y := y + s - s_t_ovp
        val ATy = prA.T * y
        val cond1 = norm_inf(pr(Dpinv)*(Ax + s - pr(b)))
        val cond2 = norm_inf(pr(Ddinv)*(ATy + pr(c)))
        val cond3 = norm_inf(dot(pr(c), x) + dot(pr(b), y))

        norm_inf(cat(cond1, cond2, cond3)) - pr(tol)
      }
    }

    PDPConverge.run

    Dd * PDPConverge.x
  }
}


