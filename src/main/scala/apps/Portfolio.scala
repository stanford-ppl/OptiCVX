package apps

import java.io._
import scala.collection.immutable.Seq
import ppl.dsl.opticvx.dcp._

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
    val gamma = cvxinput

    val x = cvxexpr

    println("defining a problem...")
    val prob = tictoc(problem(
      params(m, n),
      given(inputmatrix(n,m) -> F, inputfor(n)(i => inputscalar) -> D, inputvector(n) -> mu, inputscalar -> gamma),
      over(vector(n) -> x), 
      let(),
      where(
        x.sum == 1.0,
        cfor(n) {i => x(i) >= 0.0}
      ),
      minimize(
        mu * x - gamma * (square(norm(F.T * x)) + sumfor(n) {i => D.at(i) * square(x(i))})
      )
    ))
    /* generate a solver */
    println("generating the solver...")
    val solver = tictoc(prob.gen(PrimalDualHomogeneous))
    /* generate code for the solver in C */
    println("generating C solver code...")
    val ccodeobj = tictoc(solver.cgen())
    /* compile the code using gcc */
    println("compiling C solver...")
    val csolver = tictoc(ccodeobj.compile())
    /* run the generated C code */
    // println("solving the problem in C...")
    // val csoln = tictoc(csolver.solve(n_in)(a_in)(tol))
    // /* print out the results */
    // println("converged in " + csoln.num_iterations + " iterations")
    // println("x = " + csoln.resolve(x).map(d => "%1.6g" format d).mkString("[", ", ", "]"))
    // println("y = " + csoln.resolve(y).map(d => "%1.6g" format d).mkString("[", ", ", "]"))
    
  }
}
