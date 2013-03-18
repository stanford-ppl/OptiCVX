package ppl.dsl.opticvx.common

import scala.collection.immutable.Seq

//Here, xa is the arity of each of the xs.  We need this upfront in case
//the xs is an empty sequence, in order to construct an expression of appropriate
//arity.
case class ArityOp(val arity: Int, val xs: Seq[IRPoly]) extends HasArity[ArityOp] {
  if(arity < 0) throw new IRValidationException()
  for(x <- xs) {
    if(x.arity != arity) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): ArityOp = ArityOp(op.arity, xs map (x => x.arityOp(op)))
}

trait HasArity[T] {
  val arity: Int

  // this will be invariant at index idx if and only if
  // substituting two different variables at that index produces
  // the same result
  def invariantAt(idx: Int): Boolean = {
    if((idx < 0)||(idx >= arity)) throw new IRValidationException()
    val op1 = ArityOp(
      arity + 1, 
      for(i <- 0 until arity) yield IRPoly.param(i, arity + 1)
    )
    val op2 = ArityOp(
      arity + 1, 
      for(i <- 0 until arity) yield IRPoly.param(if (i == idx) arity else i, arity + 1)
    )
    val sub1 = arityOp(op1)
    val sub2 = arityOp(op2)
    return sub1 == sub2
  }

  def demote: T = removeParam(arity - 1)
  def promote: T = addParam(arity)
  
  //def promoteBy(len: Int): T = arityOp(ArityOpPromoteBy(len))
  def promoteBy(len: Int): T = {
    val op = ArityOp(
      arity + len,
      for(i <- 0 until arity) yield IRPoly.param(i, arity + len))
    arityOp(op)
  }

  // promote this value to a particular arity
  def promoteTo(len: Int): T = {
    if(arity > len) throw new IRValidationException()
    promoteBy(len - arity)
  }

  //def removeParam(idx: Int): T = arityOp(ArityOpRemoveParam(idx))
  def removeParam(idx: Int): T = {
    // no well-defined way to remove this parameter if the expression is not
    // invariant in the parameter
    if(!invariantAt(idx)) throw new IRValidationException()
    val op = ArityOp(
      arity - 1,
      for(i <- 0 until arity) yield {
        if(i < idx) IRPoly.param(i, arity - 1)
        else if(i == idx) IRPoly.const(0, arity - 1)
        else IRPoly.param(i - 1, arity - 1)
      })
    arityOp(op)
  }

  //def addParam(idx: Int): T = arityOp(ArityOpAddParam(idx))
  def addParam(idx: Int): T = {
    if((idx < 0)||(idx > arity)) throw new IRValidationException()
    val op = ArityOp(
      arity + 1,
      for(i <- 0 until arity) yield {
        if(i < idx) IRPoly.param(i, arity + 1)
        else IRPoly.param(i + 1, arity + 1)
      })
    arityOp(op)
  }

  //def substituteAt(idx: Int, irpoly: IRPoly): T = arityOp(ArityOpSubstituteAt(idx, irpoly))
  def substituteAt(idx: Int, irpoly: IRPoly): T = {
    if((idx < 0)||(idx >= arity)) throw new IRValidationException()
    if(irpoly.arity + 1 != arity) throw new IRValidationException()
    val op = ArityOp(
      arity - 1,
      for(i <- 0 until arity) yield {
        if(i < idx) IRPoly.param(i, arity - 1)
        else if(i == idx) irpoly
        else IRPoly.param(i - 1, arity - 1)
      })
    arityOp(op)
  }

  // substitutes the given IRPolys for the last arguments to this class
  def substituteSeq(irps: Seq[IRPoly]): T = {
    if(irps.length == 0) {
      val op = ArityOp(
        arity,
        for(i <- 0 until arity) yield IRPoly.param(i, arity))
      arityOp(op)
    }
    else {
      val newarity: Int = irps(0).arity
      if(newarity + irps.length != arity) throw new IRValidationException()
      for(i <- irps) {
        if(i.arity != newarity) throw new IRValidationException()
      }
      val op = ArityOp(
        newarity,
        (for(i <- 0 until arity) yield IRPoly.param(i, arity)) ++ irps)
      arityOp(op)
    }
  }

  def arityOp(op: ArityOp): T
}
