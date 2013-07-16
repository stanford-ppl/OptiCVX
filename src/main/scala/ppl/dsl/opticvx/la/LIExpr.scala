package ppl.dsl.opticvx.la

import ppl.dsl.opticvx.common.IRValidationException
import scala.collection.immutable.Seq


object IVar {
  //def apply(id: Int): LIExpr = IPoly(id, Seq(IConst(0), IConst(1)))
  private var free_idx: Int = 0
  def free: IVar = {
    free_idx += 1
    IVar(free_idx)
  }
}

sealed trait LIExpr {
  override def toString: String = PrettyPrint.pprint(this)
  def +(r: LIExpr): LIExpr = IAdd(this, r)
}

case class IConst(p: Int) extends LIExpr
case class IVar(id: Int) extends LIExpr
case class IAdd(a: LIExpr, b: LIExpr) extends LIExpr
case class IMult(a: LIExpr, b: LIExpr) extends LIExpr
case class ISxp(a: LIExpr, e: Int) extends LIExpr
