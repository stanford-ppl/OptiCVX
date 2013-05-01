
package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq


case class InputDesc(val arity: Int, val args: Seq[Multi[AlmapShape]], val memory: Seq[IRPoly]) extends HasArity[InputDesc] {
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

  def inputparam(idx: Int): MultiW[Almap] = {
    if((idx < 0)||(idx >= args.length)) throw new IRValidationException()
    val m = args(idx)
    MultiW(m.dims, AlmapInput(this.promoteBy(m.dims.length), idx, for (i <- 0 until m.dims.length) yield IRPoly.param(arity + i, arity + m.dims.length)))
  }

  def memoryparam(idx: Int): AVector = {
    if((idx < 0)||(idx >= memory.length)) throw new IRValidationException()
    val m = memory(idx)
    AVectorRead(this, idx)
  }

  def pushMemory(arg: IRPoly): InputDesc = InputDesc(arity, args, memory :+ arg)
}

case class InputOp(val input: InputDesc, val xs: Seq[MultiW[Almap]], val ms: Seq[AVector]) extends HasInput[InputOp] {
  val arity = input.arity
  for(x <- xs) {
    if(x.input != input) throw new IRValidationException()
  }
  for(m <- ms) {
    if(m.input != input) throw new IRValidationException()
  }

  def inputOp(op: InputOp): InputOp = InputOp(op.input, xs map (x => x.inputOp(op)), ms map (m => m.inputOp(op)))

  def arityOp(op: ArityOp): InputOp = InputOp(input.arityOp(op), xs map (x => x.arityOp(op)), ms map (m => m.arityOp(op)))

  def pushMemoryLeft(arg: IRPoly): InputOp = {
    InputOp(input.pushMemory(arg), xs map (x => x.pushMemory(arg)), (ms map (m => m.pushMemory(arg))) :+ AVectorRead(input.pushMemory(arg), input.memory.length))
  }
}

trait HasInput[T] extends HasArity[T] {
  val input: InputDesc

  def inputOp(op: InputOp): T

  def pushMemory(arg: IRPoly): T = {
    if(arg.arity != arity) throw new IRValidationException()
    val newinput = InputDesc(arity, input.args, input.memory :+ arg)
    val op = InputOp(
      newinput,
      for(i <- 0 until input.args.length) yield newinput.inputparam(i),
      for(i <- 0 until input.memory.length) yield newinput.memoryparam(i))
    inputOp(op)
  }

  def addMemory(args: Seq[IRPoly]): T = {
    // TODO: Make this function more robust
    if(!isMemoryless) throw new IRValidationException()
    val newinput = InputDesc(arity, input.args, args)
    val op = InputOp(
      newinput,
      for(i <- 0 until input.args.length) yield newinput.inputparam(i),
      Seq())
    inputOp(op)
  }

  def memoryInvariantAt(idx: Int): Boolean = {
    if((idx < 0)||(idx >= input.memory.length)) throw new IRValidationException()
    val op = InputOp(
      input,
      for(i <- 0 until input.args.length) yield input.inputparam(i),
      for(i <- 0 until input.memory.length) yield if(i != idx) input.memoryparam(i) else AVectorZero(input, input.memory(i)))
    val vop = this.inputOp(op)
    vop == this
  }

  def popMemory: T = {
    val newinput = InputDesc(arity, input.args, input.memory.dropRight(1))
    val op = InputOp(
      newinput,
      for(i <- 0 until input.args.length) yield newinput.inputparam(i),
      for(i <- 0 until input.memory.length) yield if(i != input.memory.length - 1) newinput.memoryparam(i) else AVectorZero(newinput, input.memory(i)))
    this.inputOp(op)
  }

  def isMemoryless: Boolean = (input.memory == Seq())
}