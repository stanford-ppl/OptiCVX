package ppl.dsl.opticvx.la


sealed trait LType {
  def -->(r: LType): TArr = TArr(this, r)
}

case class TVector(len: LIExpr) extends LType
case class TArr(l: LType, r: LType) extends LType
case class TILambda(id: Int, body: LType) extends LType
case class TIApp(fx: LType, arg: LIExpr) extends LType

object tilambda {
  def apply(f: LIExpr => LType): LType = {
    val v = IVar.free
    f(v)
  }
}
