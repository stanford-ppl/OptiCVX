package apps

import java.io._
import scala.collection.immutable.Seq
import ppl.dsl.opticvx.dcp._

object DCPOpsTestApp extends DCPOps {
  
  /* here, we define the square function */
  val square = {
    /* first, we declare the symbols that are to be associated with this function */
    /* this is necessary so that the scala typing works out */
    val x = cvxexpr
    val t = cvxexpr
    /* next, define the function proper */
    cvxfun(
      /* this function maps a scalar to a scalar, so it needs no parameters */
      params(),
      /* similarly, this function takes no input */
      given(),
      /* square(...) has a single argument, x */
      args(scalar -> x),
      /* square(...) is always positive */
      sign(positive),
      /* square(...) is increasing when x is positive and decreasing when x is negative */
      /* so, its tonicity is simply the sign of x */
      tonicity(x.sign),
      /* square(...) is convex */
      vexity(positive),
      /* square(...) has one dependent variable, t */
      over(scalar -> t),
      /* we bind no variables to expressions for this function */
      /* this can be used for convenience in function definitions */
      /* this can also be used to get the values of expressions when solving a problem */
      let(),
      /* the only constraint is a single second-order-cone constraint */
      where(
        in_secondorder_cone(cat(2*x, t-1), t+1)
      ),
      /* the objective (and output of the function) is to maximize t */
      minimize(t)
    )
  }

  /* next, we define a square root function */
  val sqrt = {
    /* declare the function symbols */
    val x = cvxexpr
    val t = cvxexpr
    /* next, define the function proper */
    cvxfun(
      /* this function maps a scalar to a scalar, so it needs no parameters */
      params(),
      /* similarly, this function takes no input */
      given(),
      /* sqrt(...) has a single argument, x */
      args(scalar -> x),
      /* sqrt(...) is always positive */
      sign(positive),
      /* sqrt(...) is increasing */
      tonicity(positive),
      /* sqrt(...) is concave */
      vexity(negative),
      /* sqrt(...) has one dependent variable, t */
      over(scalar -> t),
      /* we bind no variables to expressions for this function */
      let(),
      /* the only constraint is a single second-order-cone constraint */
      where(
        in_secondorder_cone(cat(2*t, x-1), x+1)
      ),
      /* the objective (and output of the function) is to maximize t */
      maximize(t)
    )
  }

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
      /* this problem has no inputs */
      given(),
      /* this problem has two optimization variables, x and y */
      over(vector(n) -> x, vector(n) -> y), 
      /* we bind z to an expression */
      let(),
      /* our constraints */
      where(
        x(0) == 0.0,
        cfor(n-1) {i => x(i + 1) == x(i) + 1.0},
        cfor(n) {i => y(i) >= square(x(i))}
      ),
      /* the objective */
      minimize(
        y.sum
      )
    ))
    /* generate a solver */
    println("generating the solver...")
    val solver = tictoc(prob.gen(PrimalDualOperatorSplitting))
    /* solve the problem */
    println("solving the problem in Scala...")
    val soln = tictoc(solver.solve_definite(5)())
    /* print out the results */
    println("x = " + soln.resolve(x).map(d => "%1.3f" format d).mkString("[", ", ", "]"))
    println("y = " + soln.resolve(y).map(d => "%1.3f" format d).mkString("[", ", ", "]"))

    /* generate code for the solver in C */
    println("generating C solver code...")
    val ccodeobj = tictoc(solver.solve_cgen())
    /* add code to print out some of the variables in the C program */
    /* this is necessary since I haven't added another way for the C
     * solver to communicate results back to the Scala program */
    ccodeobj.resolve_output_and_print(x, "x", "%.3f")
    ccodeobj.resolve_output_and_print(y, "y", "%.3f")
    /* compile the code using gcc */
    println("compiling C solver...")
    val csolver = tictoc(ccodeobj.compile())
    /* run the generated C code */
    println("solving the problem in C...")
    tictoc(csolver.run(5))
  }
}
