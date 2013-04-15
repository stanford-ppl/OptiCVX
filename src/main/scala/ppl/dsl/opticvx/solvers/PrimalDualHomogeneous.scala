package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object PrimalDualHomogeneous extends SolverGenUtil {


  def code(A: Almap, b: AVector, F: Almap, g: AVector, c: AVector, cone: Cone, tol: AVector) {
    val varSize = A.domain
    val affineCstrtSize = A.codomain
    val coneSize = F.codomain

    val x_out = vector(varSize)

    // Mx + v = 0

    val bm = v2m(b)
    val cm = v2m(c)
    val gm = v2m(g)

    val M = vcat(
      hcat(zeros(A.domain, A.domain), -A.T, -F.T, zeros(F.codomain, A.domain), cm, zeros(1, A.domain)),
      hcat(A, zeros(A.codomain + F.codomain + F.codomain, A.codomain), bm, zeros(1, A.codomain)),
      hcat(F, zeros(A.codomain + F.codomain, F.codomain), -eye(F.codomain), gm, zeros(1, F.codomain)),
      hcat(-cm.T, -bm.T, -gm.T, zeros(F.codomain + 1, 1), -eye(1)))

    val K = cat(freecone(A.domain + A.codomain), cone.conj, cone, ConeNonNegative(cone.arity), ConeNonNegative(cone.arity))
    
    val x = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)
    val z = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)
    val u = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)
    val cond = scalar

    val Mproj = new LSQRProject(M, zeros(M.codomain))

    x := K.central_vector(A.input)
    u := zeros(u.size)

    converge(cond - tol) {
      z := Mproj.proj(x - u, tol)
      x := K.project(z + u)
      u := u + z - x
      cond := norm_inf(M*x) / sqrt(norm2(x))
    }

    x_out := slice(x, 0, varSize) / slice(x, varSize + affineCstrtSize + coneSize + coneSize, 1)
  }
}

