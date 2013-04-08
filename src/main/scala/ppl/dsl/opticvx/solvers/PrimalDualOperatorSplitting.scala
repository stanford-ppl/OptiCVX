package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualOperatorSplitting extends SolverGenUtil {


  def code(A: Almap, b: AVector, F: Almap, g: AVector, c: AVector, cone: Cone, tol: AVector) {
    val varSize = A.domain
    val affineCstrtSize = A.codomain
    val coneSize = F.codomain

    val x_out = vector(varSize)

    // Mx + v = 0

    val v = vector(varSize + affineCstrtSize + coneSize + 1)
    v := cat(c, b, g, zeros(1))

    val bm = v2m(b)
    val cm = v2m(c)
    val gm = v2m(g)

    val M = vcat(
      hcat(zeros(A.domain, A.domain), -A.T, -F.T, zeros(F.codomain, A.domain)),
      hcat(A, zeros(A.codomain + F.codomain + F.codomain, A.codomain)),
      hcat(F, zeros(A.codomain + F.codomain, F.codomain), -eye(F.codomain)),
      hcat(cm.T, bm.T, gm.T, zeros(F.codomain, 1)))

    val K = cat(freecone(A.domain + A.codomain), cone.conj, cone)
    
    val x = vector(varSize + affineCstrtSize + coneSize + coneSize)
    val z = vector(varSize + affineCstrtSize + coneSize + coneSize)
    val u = vector(varSize + affineCstrtSize + coneSize + coneSize)
    val cond = scalar

    val Mproj = new LSQRProject(M, v)

    x := K.central_vector(A.input) //cat(zeros(varSize + affineCstrtSize + coneSize + coneSize), ones(2))
    u := cat(zeros(varSize + affineCstrtSize + coneSize + coneSize))

    converge(cond - tol) {
      z := Mproj.proj(x - u, -1)
      x := K.project(z + u)
      u := u + z - x
      cond := norm_inf(M*x + v)
    }

    x_out := slice(x, 0, varSize)
  }
}

