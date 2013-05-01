package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq

/*
object PrimalDualHomogeneous extends SolverGenHomogeneous with SolverGenUtil {


  def hcode(A: Almap, K: Cone, tol: AVector): AVector = {
    val x = vector(A.domain)
    val z = vector(A.domain)
    val u = vector(A.domain)
    val cond = scalar

    val Aproj = new LSQRProject(A, zeros(A.codomain))

    x := K.central_vector(A.input)
    u := zeros(u.size)

    converge(cond - tol) {
      z := Aproj.proj(x - u, tol)
      x := K.project(z + u)
      u := u + z - x
      cond := norm_inf(A*x)
    }

    return x
  }
}
*/
