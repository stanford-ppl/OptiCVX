package ppl.dsl.opticvx.la

import scala.collection.immutable.Seq


sealed trait IntExpr

case class IConst(d: Int) extends IntExpr
case class IPoly(id: Int, cs: Seq[IntExpr]) extends IntExpr
case class ILambda(id: Int, body: IntExpr) extends IntExpr
case class IApp(fx: IntExpr, arg: IntExpr) extends IntExpr
