
package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


case class Multi[T <: HasArity[T]](val dims: Seq[IRPoly], val body: T) extends HasArity[Multi[T]] {
  val arity: Int = body.arity - dims.length
  for(i <- 0 until dims.length) {
    if(dims(i).arity != arity + i) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): Multi[T] = Multi(
    for(i <- 0 until dims.length) yield dims(i).arityOp(op.promoteBy(i)),
    body.arityOp(op.promoteBy(dims.length)))

  def extend[U <: HasArity[U]](fx: T => U): Multi[U] = Multi(dims, fx(body))

}

case class MultiW[T <: HasInput[T]](val dims: Seq[IRPoly], val body: T) extends HasInput[MultiW[T]] {
  val arity: Int = body.arity - dims.length
  val input: InputDesc = body.input.demoteBy(dims.length)
  for(i <- 0 until dims.length) {
    if(dims(i).arity != arity + i) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): MultiW[T] = MultiW(
    for(i <- 0 until dims.length) yield dims(i).arityOp(op.promoteBy(i)),
    body.arityOp(op.promoteBy(dims.length)))

  def inputOp(op: InputOp): MultiW[T] = MultiW(dims, body.inputOp(op.promoteBy(dims.length)))

  def extend[U <: HasArity[U]](fx: T => U): Multi[U] = Multi(dims, fx(body))
  def extendw[U <: HasInput[U]](fx: T => U): MultiW[U] = MultiW(dims, fx(body))
}

