package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualHomogeneousEx extends SolverGenHomogeneous with SolverGenUtil {


  def hcode(A: Almap, K: Cone, tol: AVector): AVector = {
    val x = vector(A.domain)
    x := K.central_vector(A.input)

    val u = vector(A.domain)
    val cond = scalar

    val nms = 10

    val z = for(i <- 0 until nms) yield vector(A.domain)
    for(i <- 0 until nms) z(i) := zeros(A.domain)

    converge(cond - tol) {
      // recreate the vectors
      z(0) := A.T * A * x
      for(i <- 1 until nms) {
        z(0) := z(0) - z(i) * dot(z(0),z(i))
      }
      z(0) := z(0) / sqrt(norm2(z(0)))
      // perform the projection
      converge(cond - tol) {
        for(i <- 1 until nms) {
          x := x - z(i) * dot(x, z(i))
        }
        u := K.project(x)
        cond := sqrt(norm2(u - x))
        x := u
      }
      // update the vectors
      for(i <- 1 until nms) {
        z(i) := z(i - 1)
      }
      cond := norm_inf(A*x)
    }

    return x
  }

}

