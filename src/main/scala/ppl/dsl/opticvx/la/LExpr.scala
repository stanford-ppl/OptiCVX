package ppl.dsl.opticvx.la


sealed trait LExpr

object EVar {
  private var free_idx: Int = 0
  def free: EVar = {
    free_idx += 1
    EVar(free_idx)
  }
}

case class EVar(id: Int) extends LExpr
case class EConst(d: Double) extends LExpr
case class ELambda(t: LType, id: Int, body: LExpr) extends LExpr
case class EILambda(id: Int, body: LExpr) extends LExpr
case class EApp(fx: LExpr, arg: LExpr) extends LExpr
case class EIApp(fx: LExpr, arg: LIExpr) extends LExpr

trait EPrimitive extends LExpr {
  val ltype: LType
}

object EPAdd extends EPrimitive {
  val ltype: LType = tilambda(n => TVector(n) --> (TVector(n) --> TVector(n)))
}
object EPNeg extends EPrimitive {
  val ltype: LType = tilambda(n => TVector(n) --> TVector(n))
}
object EPCat extends EPrimitive {
  val ltype: LType = tilambda(m => tilambda(n => TVector(m) --> (TVector(n) --> TVector(m+n))))
}
object EPSlice extends EPrimitive {
  val ltype: LType = tilambda(n => tilambda(i => tilambda(m => TVector(n) --> TVector(m))))
}
