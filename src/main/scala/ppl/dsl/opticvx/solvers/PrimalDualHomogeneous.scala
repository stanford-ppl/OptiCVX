package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualHomogeneous extends SolverGenHomogeneous with SolverGenUtil {

  def hcode(A: Almap, K: Cone, tol: AVector): AVector = {
    object PDHConverge extends Converge {
      val x = state(K.central_vector(A.input))
      val u = state(zeros(A.domain))

      def body: AVector = {
        val Aproj = new LSQRProject(pr(A), zeros(A.codomain))
        val z = Aproj.proj(x - u, pr(tol))
        x := K.project(z + u)
        u := u + z - x
        norm_inf(pr(A) * x) - pr(tol)
      }
    }

    PDHConverge.run

    PDHConverge.x
  }

}

