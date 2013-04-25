package ppl.dsl.opticvx.solvergen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq


trait SolverGenUtil {
  self: SolverGen =>

  class OrthoNullProjectorPartial(val A: Almap) {
    private val u: SVariable = vector(A.domain)
    private val v: SVariable = vector(A.domain)
    private val w: SVariable = vector(A.codomain)
    private val norm2u: SVariable = scalar
    private val norm2v: SVariable = scalar
    private val udotx: SVariable = scalar
    private val vdotx: SVariable = scalar
    private val udotv: SVariable = scalar
    private val deteq: SVariable = scalar
    private val alpha: SVariable = scalar
    private val beta: SVariable = scalar
    private var initdone: Boolean = false

    def residual: AVector = {
      if(initdone != true) throw new IRValidationException()
      sqrt(vdotx)
    }

    def proj_init(x: AVector): AVector = {
      if(initdone != false) throw new IRValidationException()
      initdone = true

      w := A * x
      u := A.T * w
      norm2u := norm2(u)
      udotx := dot(u, x) //here, we cheat to assign the appropriate residual
      alpha := udotx/norm2u
      x - u*alpha
    }

    def proj(x: AVector): AVector = {
      if(initdone != true) throw new IRValidationException()

      w := A * x
      v := A.T * w
      norm2u := norm2(u)
      norm2v := norm2(v)
      udotx := dot(u, x)
      vdotx := dot(v, x)
      udotv := dot(u, v)
      deteq := (norm2u * norm2v) - (udotv * udotv)
      alpha := ((norm2v * udotx) - (udotv * vdotx))/deteq
      beta := ((norm2u * vdotx) - (udotv * udotx))/deteq
      u := u*alpha + v*beta
      x - u
    }
  }

  class LSQR(val A: Almap) {
    private val beta = scalar
    private val betau = vector(A.codomain)
    private val u = betau/beta
    private val alpha = scalar
    private val alphav = vector(A.domain)
    private val v = alphav/alpha
    private val w = vector(A.domain)
    private val x = vector(A.domain)
    private val theta = scalar
    private val phi = scalar
    private val phibar = scalar
    private val rho = scalar
    private val rhobar = scalar
    private val c = scalar
    private val s = scalar

    def solve(b: AVector): AVector = solve(b, 1e-3, -1)

    def solve(b: AVector, tol: AVector): AVector = solve(b, tol, -1)

    def solve(b: AVector, itermax: Int): AVector = solve(b, 1e-3, itermax)

    def solve(b: AVector, tol: AVector, itermax: Int): AVector = {

      //initialization phase
      betau := b
      beta := sqrt(norm2(betau))
      alphav := A.T*u
      alpha := sqrt(norm2(alphav))
      w := v
      x := zeros(A.domain)
      phibar := beta
      rhobar := alpha

      converge(phibar - tol, itermax) {
        //solution phase
        betau := A*v - u*alpha
        beta := sqrt(norm2(betau))
        alphav := A.T*u - v*beta
        alpha := sqrt(norm2(alphav))

        rho := sqrt(norm2(rhobar) + norm2(beta))
        c := rhobar / rho
        s := beta / rho
        theta := s*alpha
        rhobar := -c*alpha
        phi := c*phibar
        phibar := s*phibar

        x := x + w*(phi/rho)
        w := v - w*(theta/rho)
      }

      x
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
    private val x = vector(A.domain)
    private val r = vector(A.domain)
    private val q = vector(A.domain)
    private val p = vector(A.domain)
    private val Ap = vector(A.codomain)
    private val alpha = scalar
    private val beta = scalar
    private val qr = scalar
    private val qr_next = scalar
    private val cond = scalar

    var tol: AVector = 1e-3
    var itermax: Int = -1

    def proj(u: AVector, v: AVector, x0: AVector): AVector = {
      x := x0
      r := u - x - A.T * (A * x - b + v)
      q := Minv * r
      p := q
      qr := dot(q, r)
      cond := sqrt(norm2(r))

      converge(cond - tol) {
        Ap := A*p
        alpha := dot(q, r) / (norm2(p) + norm2(Ap))
        x := x + (p * alpha)
        r := r - ((p - A.T * Ap) * alpha)
        q := Minv * r
        qr_next := dot(q, r)
        beta := qr_next / qr
        qr := qr_next
        p := q + (p * beta)
        cond := sqrt(norm2(r))
      }

      x
    }

  }

  

}
