package ppl.dsl.opticvx.la


object EPAdd extends EPrimitive {
  val name: String = "(+)"
  override val ltype: LType = tilam(n => TVector(n) --> (TVector(n) --> TVector(n)))
}
object EPNeg extends EPrimitive {
  val name: String = "(-)"
  override val ltype: LType = tilam(n => TVector(n) --> TVector(n))
}
object EPCat extends EPrimitive {
  val name: String = "cat"
  override val ltype: LType = tilam(m => tilam(n => TVector(m) --> (TVector(n) --> TVector(m+n))))
}
object EPSlice extends EPrimitive {
  val name: String = "slice"
  override val ltype: LType = tilam(n => tilam(i => tilam(m => TVector(n) --> TVector(m))))
}
