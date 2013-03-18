package ppl.dsl.opticvx.solvers

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvergen._
import scala.collection.immutable.Seq


object AlternatingProjections extends SolverGenUtil {


  def code(Ai: Almap, bi: AVector, Fi: Almap, gi: AVector, ci: AVector, cone: Cone) {
    val varSize = Ai.domain
    val affineCstrtSize = Ai.codomain
    val coneSize = Fi.codomain

    val x_out = vector(varSize)

    val normbi = scalar
    normbi := ones(1) / sqrt(norm2(bi))
    val normgi = scalar
    normgi := ones(1) / sqrt(norm2(gi))
    val normci = scalar
    normci := ones(1) / sqrt(norm2(ci))

    val A = AlmapScaleVector(Ai, normbi)
    val b = bi * normbi
    val F = AlmapScaleVector(Fi, normgi)
    val g = gi * normgi
    val c = ci * normci

    val bm = v2m(b)
    val cm = v2m(c)
    val gm = v2m(g)

    val x = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)
    val y = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)
    val p = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)
    val q = vector(varSize + affineCstrtSize + coneSize + coneSize + 2)

    val M = vcat(
      hcat(zeros(A.domain, A.domain), -A.T, -F.T, zeros(F.codomain, A.domain), cm, zeros(1, A.domain)),
      hcat(A, zeros(A.codomain + F.codomain + F.codomain, A.codomain), bm, zeros(1, A.codomain)),
      hcat(F, zeros(A.codomain + F.codomain, F.codomain), -eye(F.codomain), gm, zeros(1, F.codomain)),
      hcat(-cm.T, -bm.T, -gm.T, zeros(F.codomain + 1, 1), -eye(1)))
    val K = cat(freecone(A.domain + A.codomain), cone.conj, cone, ConeNonNegative(cone.arity), ConeNonNegative(cone.arity))
    
    val Mproj = new LSQRProject(M, zeros(M.codomain))

    x := K.central_vector(A.input) //cat(zeros(varSize + affineCstrtSize + coneSize + coneSize), ones(2))
    y := cat(zeros(varSize + affineCstrtSize + coneSize + coneSize + 2))
    p := cat(zeros(varSize + affineCstrtSize + coneSize + coneSize + 2))
    q := cat(zeros(varSize + affineCstrtSize + coneSize + coneSize + 2))

    converge(sqrt(norm2(M*x))) {
      //converge(Mproj.residual) {
      //  y := Mproj.proj(y)
      //}
      y := Mproj.proj(x + p, 10)
      p := x + p - y
      x := K.project(y + q)
      q := y + q - x
    }

    x_out := slice(x, 0, varSize) / slice(x, varSize + affineCstrtSize + coneSize + coneSize, 1)
  }
}

