package apps

import java.io._
import scala.collection.immutable.Seq
import ppl.dsl.opticvx.dcp._
import scala.util.Random

object DCPPortfolioApp extends DCPOps with DCPLibrary {
  
  def tictoc[T](t: =>T): T = {
    val start = System.currentTimeMillis()
    val r: T = t
    val elapsed = System.currentTimeMillis() - start
    println("elapsed time: %.3f seconds".format(elapsed * 0.001))
    r
  }

  def main(args: Array[String]) {
    val m = cvxparam
    val n = cvxparam

    val F = cvxinput
    val D = cvxinput
    val mu = cvxinput
    val W = cvxinput

    val x = cvxexpr
    val y = cvxexpr

    println("defining a problem...")
    val prob = tictoc(problem(
      params(m, n),
      given(inputmatrix(m,n) -> F, inputmatrixdiag(n) -> D, inputvector(n) -> mu, inputscalar -> W),
      over(vector(n) -> x), 
      let(),
      where(
        x.sum == W,
        cfor(n) {i => x(i) >= 0.0}
      ),
      maximize(
        (mu.T * x) - (square(norm(F.T * x)) + square(norm(D * x)))
      )
    ))
    /* generate a solver */
    println("generating the solver...")
    val solver = tictoc(prob.gen(PrimalDualProjections))
    println("generating problem data...")
    val rand = new scala.util.Random(42)
    val m_in: Int = 30
    val n_in: Int = 1000
    val gamma_in: Double = 10
    val F_in: Seq[Seq[Double]] = for(i <- 0 until n_in) yield for (j <- 0 until m_in) yield scala.math.sqrt(gamma_in) * rand.nextGaussian()
    val D_in: Seq[Double] = for(i <- 0 until n_in) yield scala.math.sqrt(2*gamma_in*rand.nextDouble())
    val mu_in: Seq[Double] = for(i <- 0 until n_in) yield scala.math.exp(rand.nextGaussian())
    val W_in: Double = 1.0 //n_in.toDouble
    val tol: Double = 1e-4
    /* generate code for the solver */
    println("compiling solver code...")
    val codeobj = tictoc(solver.compile(SolverRuntimeCGen))
    /* run the generated code */
    println("solving the problem...")
    val soln = tictoc(codeobj.solve(m_in, n_in)(F_in, definitematrixdiag(D_in), mu_in, W_in)(tol))
    /* print out the results */
    println("converged in " + soln.num_iterations + " iterations")
    println("converged in " + soln.timer + " seconds")
    println("x = " + soln.resolve(x).map(d => "%+1.3f" format d).mkString("[", ", ", "]"))
    
  }
}
