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

    val x = cvxexpr

    println("defining a problem...")
    val prob = tictoc(problem(
      params(m, n),
      given(inputmatrix(m,n) -> F, inputfor(n)(i => inputscalar) -> D, inputvector(n) -> mu),
      over(vector(n) -> x), 
      let(),
      where(
        x.sum == 1.0,
        cfor(n) {i => x(i) >= 0.0}
      ),
      maximize(
        (mu.T * x) - (square(norm(F.T * x)) + square(norm(xfor(n) {i => D.at(i) * x(i)})))
      )
    ))
    /* generate a solver */
    println("generating the solver...")
    val solver = tictoc(prob.gen(PrimalDualOperatorSplitting))
    /* generate code for the solver in C */
    println("generating C solver code...")
    val ccodeobj = tictoc(solver.cgen())
    /* compile the code using gcc */
    println("compiling C solver...")
    val csolver = tictoc(ccodeobj.compile())
    /* run the generated C code */
    println("solving the problem in C...")
    val rand = new scala.util.Random(42)
    val m_in: Int = 30
    val n_in: Int = 1000
    val F_in: Seq[Seq[Double]] = for(i <- 0 until n_in) yield for (j <- 0 until m_in) yield 0.1*rand.nextGaussian()
    val D_in: Seq[Double] = for(i <- 0 until n_in) yield 0.1*rand.nextGaussian()
    val mu_in: Seq[Double] = for(i <- 0 until n_in) yield rand.nextGaussian()
    val tol: Double = 1e-4
    val csoln = tictoc(csolver.solve(m_in, n_in)(F_in, inputscalarseq(D_in), mu_in)(tol))
    /* print out the results */
    println("converged in " + csoln.num_iterations + " iterations")
    println("x = " + csoln.resolve(x).map(d => "%+1.3f" format d).mkString("[", ", ", "]"))
    
  }
}
