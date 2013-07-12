package ppl.dsl.opticvx.la


sealed trait VectorType

object TScalar extends VectorType
case class TArr(l: VectorType, r: VectorType) extends VectorType
case class TMux(len: IntExpr, st: IntExpr) extends VectorType

