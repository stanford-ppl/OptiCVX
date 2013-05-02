package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq


trait SolverGenUtil {
  self: SolverGen =>

  class OrthoNullProjectorPartial(val A: Almap) {
    private var u: AVector = null
    private var v: AVector = null
    private var w: AVector = null
    private var norm2u: AVector = null
    private var norm2v: AVector = null
    private var udotx: AVector = null
    private var vdotx: AVector = null
    private var udotv: AVector = null
    private var deteq: AVector = null
    private var alpha: AVector = null
    private var beta: AVector = null
    private var initdone: Boolean = false

    def residual: AVector = {
      if(initdone != true) throw new IRValidationException()
      sqrt(vdotx)
    }

    def proj_init(x: AVector): AVector = {
      if(initdone != false) throw new IRValidationException()
      initdone = true

      w = A * x
      u = A.T * w
      norm2u = norm2(u)
      udotx = dot(u, x) //here, we cheat to assign the appropriate residual
      alpha = udotx/norm2u
      x - u*alpha
    }

    def proj(x: AVector): AVector = {
      if(initdone != true) throw new IRValidationException()

      w = A * x
      v = A.T * w
      norm2u = norm2(u)
      norm2v = norm2(v)
      udotx = dot(u, x)
      vdotx = dot(v, x)
      udotv = dot(u, v)
      deteq = (norm2u * norm2v) - (udotv * udotv)
      alpha = ((norm2v * udotx) - (udotv * vdotx))/deteq
      beta = ((norm2u * vdotx) - (udotv * udotx))/deteq
      u = u*alpha + v*beta
      x - u
    }
  }

  class LSQR(val A: Almap) {

    def solve(b: AVector): AVector = solve(b, 1e-3, -1)

    def solve(b: AVector, tol: AVector): AVector = solve(b, tol, -1)

    def solve(b: AVector, itermax: Int): AVector = solve(b, 1e-3, itermax)

    def solve(b: AVector, tol: AVector, imax: Int): AVector = {

      object LSQRConverge extends Converge {
        val betau = state(b)
        val beta = state(sqrt(norm2(betau)))
        val alphav = state(A.T*u)
        val alpha = state(sqrt(norm2(alphav)))
        val w = state(v)
        val x = state(zeros(A.domain))
        val phibar = state(beta)
        val rhobar = state(alpha)
        override val itermax = imax
        def u: AVector = betau / beta
        def v: AVector = alphav / alpha

        def body: AVector = {
          betau := pr(A)*v - u*alpha
          beta := sqrt(norm2(betau))
          alphav := pr(A.T)*u - v*beta
          alpha := sqrt(norm2(alphav))

          val rho = sqrt(norm2(rhobar) + norm2(beta))
          val c = rhobar / rho
          val s = beta / rho
          val theta = s*alpha
          rhobar := -c*alpha
          val phi = c*phibar
          phibar := s*phibar

          x := x + w*(phi/rho)
          w := v - w*(theta/rho)

          phibar - pr(tol)
        }
      }

      LSQRConverge.run

      LSQRConverge.x
    }
  }

  
  //projects onto Ax = b
  class LSQRProject(val A: Almap, val b: AVector) {
    private val lsqr = new LSQR(A)

    def proj(x: AVector): AVector = proj(x, 1e-3, -1)

    def proj(x: AVector, tol: AVector): AVector = proj(x, tol, -1)

    def proj(x: AVector, itermax: Int): AVector = proj(x, 1e-3, itermax)

    def proj(x: AVector, tol: AVector, itermax: Int): AVector = {
      x - lsqr.solve(A*x - b, tol, itermax)
    }
  }

  
  //projects (u,v) onto Ax + s = b with preconditioner matrix M
  class CGProject(val A: Almap, val Minv: Almap, b: AVector) {
    var tol: AVector = 1e-3
    var itermax: Int = -1

    def proj(u: AVector, v: AVector, x0: AVector): AVector = {
      val imax = itermax

      object CGConverge extends Converge {
        val x = state(x0)
        val r = state(u - x - A.T * (A * x - b + v))
        val q = state(r)
        val p = state(q)
        val qr = state(dot(q, r))
        override val itermax = imax

        def body: AVector = {
          val Ap = pr(A)*p
          val ATAp = pr(A.T)*Ap
          val alpha = qr / (norm2(p) + norm2(Ap))
          x := x + (p * alpha)
          r := r - ((p + ATAp) * alpha)
          q := pr(Minv) * r
          val qr_next = dot(q, r)
          val beta = qr_next / qr
          qr := qr_next
          p := q + (p * beta)
          sqrt(norm2(r)) - pr(tol)
        }
      }

      CGConverge.run

      CGConverge.x
    }

  }
  
  

}



