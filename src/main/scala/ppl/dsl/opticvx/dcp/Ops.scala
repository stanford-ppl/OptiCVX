package ppl.dsl.opticvx.dcp


trait DCPOps extends DCPOpsSolve {

  def introspect(x: CvxExpr): CvxExpr = {
    println("[\033[34mintrospect\033[0m] " + x.fx.vexity.reduce.toString)
    x
  }

}
