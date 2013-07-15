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
  val ltype: LType = tilambda(v => TVector(v) --> (TVector(v) --> TVector(v)))
}
object EPNeg extends EPrimitive {
  val ltype: LType = tilambda(v => TVector(v) --> TVector(v))
}


