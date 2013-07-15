package ppl.dsl.opticvx.la

import scala.collection.immutable.Seq


object IVar {
  //def apply(id: Int): LIExpr = IPoly(id, Seq(IConst(0), IConst(1)))
  private var free_idx: Int = 0
  def free: LIExpr = {
    free_idx += 1
    IPoly(free_idx, Seq(IConst(0), IConst(1)))
  }
}

sealed trait LIExpr

case class IConst(p: Int) extends LIExpr
case class IPoly(id: Int, cs: Seq[LIExpr]) extends LIExpr
