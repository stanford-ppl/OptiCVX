package apps

import java.io._
import scala.collection.immutable.Seq
import ppl.dsl.opticvx.dcp._

object DCPOpsTestApp extends DCPOps with DCPLibrary {

  def tictoc[T](t: =>T): T = {
    val start = System.currentTimeMillis()
    val r: T = t
    val elapsed = System.currentTimeMillis() - start
    println("elapsed time: %.3f seconds".format(elapsed * 0.001))
    r
  }

  def main(args: Array[String]) {
    /* now, we solve a problem */
    /* first, declare the symbols */
    /* n is a integer input parameter */
    val n = cvxparam
    /* a is an input */
    val a = cvxinput
    /* x and y are problem variables */
    val x = cvxexpr
    val y = cvxexpr
    val z = cvxexpr
    /* define the problem */
    println("defining a problem...")
    val prob = tictoc(problem(
      /* here, we bind an integer to the parameter n */
      /* this just shows how parameters work; we could've just as easily used 6 in place of n in the code below */
      params(n),
      /* this problem has one vector input, a */
      given(inputvector(n) -> a),
      /* this problem has two optimization variables, x and y */
      over(vector(n) -> x, vector(n) -> y), 
      /* we bind z to an expression */
      let(),
      /* our constraints */
      where(
        cfor(n) {i => x(i) <= sqrt(a(i))},
        cfor(n) {i => y(i) >= square(a(i))}
      ),
      /* the objective */
      minimize(
        y.sum - x.sum
      )
    ))
    /* generate a solver */
    println("generating the solver...")
    val solver = tictoc(prob.gen(PrimalDualProjections))
    /* define variables to store the inputs we'll pass to the solver */
    val n_in: Int = 3
    val a_in: Seq[Double] = Seq(1.0, 2.0, 3.0)
    val tol: Double = 1e-3
    /* generate code for the solver */
    println("compile solver code...")
    val codeobj = tictoc(solver.compile(SolverRuntimeDefinite))
    /* run the generated code */
    println("solving the problem...")
    val soln = tictoc(codeobj.solve(n_in)(a_in)(tol))
    /* print out the results */
    println("converged in " + soln.num_iterations + " iterations")
    println("x = " + soln.resolve(x).map(d => "%1.6g" format d).mkString("[", ", ", "]"))
    println("y = " + soln.resolve(y).map(d => "%1.6g" format d).mkString("[", ", ", "]"))
    
  }
}
