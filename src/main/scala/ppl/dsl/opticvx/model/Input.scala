
package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


case class InputDesc(val arity: Int, val args: Seq[Multi[AlmapShape]], val memory: Seq[Multi[IRPoly]]) extends HasArity[InputDesc] {
  for(a <- args) {
    if(a.arity != arity) throw new IRValidationException()
  }
  for(m <- memory) {
    if(m.arity != arity) throw new IRValidationException()
  }

  def arityOp(op: ArityOp): InputDesc = {
    if(op.xs.length != arity) throw new IRValidationException()
    InputDesc(op.arity, args map (a => a.arityOp(op)), memory map (a => a.arityOp(op)))
  }
}

case class InputOp(val input: InputDesc, val xs: Seq[Almap], val ms: Seq[AVector]) extends HasInput[InputOp] {
  val arity = input.arity
  for(x <- xs) {
    if(x.input != input) throw new IRValidationException()
  }
  for(m <- ms) {
    if(m.input != input) throw new IRValidationException()
  }

  def inputOp(op: InputOp): InputOp = InputOp(op.input, xs map (x => x.inputOp(op)), ms map (m => m.inputOp(op)))

  def arityOp(op: ArityOp): InputOp = InputOp(input.arityOp(op), xs map (x => x.arityOp(op)), ms map (m => m.arityOp(op)))
}

trait HasInput[T] extends HasArity[T] {
  val input: InputDesc

  def inputOp(op: InputOp): T

  def pushMemory(arg: Multi[IRPoly]): T = {
    if(arg.arity != arity) throw new IRValidationException()
    val newinput = InputDesc(arity, input.args, input.memory :+ arg)
    val op = InputOp(
      newinput,
      for(i <- 0 until input.args.length) yield 
        AlmapInput(
          newinput.promoteBy(input.args(i).dims.length),
          i,
          for(j <- 0 until input.args(i).dims.length) yield
            IRPoly.param(arity + j, arity + input.args(i).dims.length)),
      for(i <- 0 until input.memory.length) yield
        AVectorRead(
          newinput.promoteBy(input.memory(i).dims.length),
          i,
          for(j <- 0 until input.memory(i).dims.length) yield
            IRPoly.param(arity + j, arity + input.memory(i).dims.length)))
    inputOp(op)
  }

  def addMemory(args: Seq[Multi[IRPoly]]): T = {
    // TODO: Make this function more robust
    if(!isMemoryless) throw new IRValidationException()
    val newinput = InputDesc(arity, input.args, input.memory ++ args)
    val op = InputOp(
      newinput,
      for(i <- 0 until input.args.length) yield 
        AlmapInput(
          newinput.promoteBy(input.args(i).dims.length),
          i,
          for(j <- 0 until input.args(i).dims.length) yield
            IRPoly.param(arity + j, arity + input.args(i).dims.length)),
      Seq())
    inputOp(op)
  }

  def isMemoryless: Boolean = (input.memory == Seq())
}