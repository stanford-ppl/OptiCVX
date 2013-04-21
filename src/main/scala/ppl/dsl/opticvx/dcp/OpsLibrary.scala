package ppl.dsl.opticvx.dcp


trait DCPLibrary {
  self: DCPOps =>

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

  /* next, we define a norm function */
  val norm = {
    /* declare the function symbols */
    val n = cvxparam
    val x = cvxexpr
    val t = cvxexpr
    /* next, define the function proper */
    cvxfun(
      /* this function maps a scalar to a scalar, so it needs no parameters */
      params(n),
      /* similarly, this function takes no input */
      given(),
      /* sqrt(...) has a single argument, x */
      args(vector(n) -> x),
      /* norm(...) is always positive */
      sign(positive),
      /* norm(...) has no monotonicity */
      tonicity(none),
      /* norm(...) is convex */
      vexity(positive),
      /* sqrt(...) has one dependent variable, t */
      over(scalar -> t),
      /* we bind no variables to expressions for this function */
      let(),
      /* the only constraint is a single second-order-cone constraint */
      where(
        in_secondorder_cone(x, t)
      ),
      /* the objective (and output of the function) is to minimize t */
      minimize(t)
    )
  }

}
