package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

trait DCPOpsGlobal {

  var globalArity: Int = -1
  var globalInputSize: InputDesc = null
  var globalArgSize: Seq[IRPoly] = null
  var globalSignumArity: Int = -1

  def globalArityPromote() {
    if ((globalInputSize != null)||(globalArgSize != null)) {
      if (globalInputSize.arity != globalArity) throw new IRValidationException()
      for(a <- globalArgSize) {
        if(a.arity != globalArity) throw new IRValidationException()
      }
      globalInputSize = globalInputSize.promote
      globalArgSize = globalArgSize map (x => x.promote)
    }
    globalArity += 1
  }

  def globalArityDemote() {
    if ((globalInputSize != null)||(globalArgSize != null)) {
      if (globalInputSize.arity != globalArity) throw new IRValidationException()
      for(a <- globalArgSize) {
        if(a.arity != globalArity) throw new IRValidationException()
      }
      globalInputSize = globalInputSize.demote
      globalArgSize = globalArgSize map (x => x.demote)
    }
    globalArity -= 1
  }

  implicit def int2irpoly(i: Int): IRPoly = IRPoly.const(1, globalArity)

  def scalar: IRPoly = IRPoly.const(1, globalArity)
  def vector(size: IRPoly): IRPoly = {
    if(size.arity != globalArity) throw new IRValidationException()
    size
  }

}