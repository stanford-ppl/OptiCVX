package ppl.dsl.opticvx.la


sealed trait VectorExpr

case class VVar(id: Int) extends VectorExpr
case class VConst(d: Double) extends VectorExpr
case class VVLambda(t: VectorType, id: Int, body: VectorExpr) extends VectorExpr
case class VILambda(t: IntType, id: Int, body: VectorExpr) extends VectorExpr
case class VVApp(fx: VectorExpr, arg: VectorExpr) extends VectorExpr
case class VIApp(fx: VectorExpr, arg: IntExpr) extends VectorExpr

