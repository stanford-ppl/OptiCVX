package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.common._
import scala.collection.immutable._

object AVector {
  // def input(at: IRPoly, len: IRPoly): AVector = {
  //   if(at.arity != len.arity) throw new IRValidationException()
  //   AVectorCatFor(len, AVectorScaleInput(AVectorOne(at.arity + 1), at.promote + at.next))
  // }
  def const(c: Double, input: InputDesc): AVector = {
    AVectorScaleConstant(AVectorOne(input), c)
  }

  val arityOpMemoMap = new scala.collection.mutable.HashMap[Tuple2[AVector, ArityOp], AVector]()
  val inputOpMemoMap = new scala.collection.mutable.HashMap[Tuple2[AVector, InputOp], AVector]()
  val simplifyMemoMap = new scala.collection.mutable.HashMap[AVector, AVector]()
}

trait AVector extends HasInput[AVector] {
  self: ScalaObject with Product =>

  val size: IRPoly

  lazy val cachedHashCode: Int = scala.runtime.ScalaRunTime._hashCode(this)
  override def hashCode(): Int = cachedHashCode

  //this is a fast, incorrect equals function
  // override def equals(x: Any): Boolean = (x match {
  //     case u: AVector => (cachedHashCode == u.cachedHashCode)&&(x.getClass == this.getClass)
  //     case _ => false
  //   })

  def arityVerify() {
    if(size.arity != arity) throw new IRValidationException()
    if(input.arity != arity) throw new IRValidationException()
    //if(!(size.isNonNegative)) throw new IRValidationException()
  }

  def +(u: AVector) = AVectorSum(Seq(this, u))
  def -(u: AVector) = AVectorSum(Seq(this, AVectorNeg(u)))
  def unary_-() = AVectorNeg(this)
  def ++(u: AVector) = AVectorCat(this, u)
  def *(u: AVector) = AVectorMpy(this, u)
  def /(u: AVector) = AVectorDiv(this, u)
  def apply(at: IRPoly, size: IRPoly) = AVectorSlice(this, at, size)
  def is0: Boolean
  def isPure: Boolean

  final def simplify: AVector = {
    val rv = AVector.simplifyMemoMap.getOrElseUpdate(this, this.simplifySub)
    if(rv.size != size) {
      println(this.getClass.getName)
      println(rv.size)
      println(size)
      println(rv)
      println(this)
      throw new IRValidationException()
    }
    rv
  }

  def simplifySub: AVector

  final def arityOp(op: ArityOp): AVector = {
    AVector.arityOpMemoMap.getOrElseUpdate((this, op), arityOpSub(op))
  }
  final def inputOp(op: InputOp): AVector = {
    val rv = AVector.inputOpMemoMap.getOrElseUpdate((this, op), inputOpSub(op))
    if(rv.size != size) {
      println(this.getClass.getName)
      println(rv.getClass.getName)
      println(rv.size)
      println(size)
      throw new IRValidationException()
    }
    rv
  }

  def arityOpSub(op: ArityOp): AVector
  def inputOpSub(op: InputOp): AVector

  private lazy val invariantAtSeq: Seq[Boolean] = 
    for(i <- 0 until arity) yield invariantAtSub(i)
  override def invariantAt(idx: Int): Boolean = invariantAtSeq(idx)
  private lazy val memoryInvariantAtSeq: Seq[Boolean] = 
    for(i <- 0 until input.memory.size) yield memoryInvariantAtSub(i)
  override def memoryInvariantAt(idx: Int): Boolean = memoryInvariantAtSeq(idx)

  def invariantAtSub(idx: Int): Boolean
  def memoryInvariantAtSub(idx: Int): Boolean

  //override def promote: AVector = AVectorPromoted(this)
  //override def pushMemory(memsize: IRPoly): AVector = AVectorMemoryPushed(this, memsize)
}

case class AVectorZero(val input: InputDesc, val size: IRPoly) extends AVector {
  val arity: Int = size.arity
  arityVerify()
  def arityOpSub(op: ArityOp): AVector = AVectorZero(input.arityOp(op), size.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorZero(op.input, size)
  def is0: Boolean = true
  def isPure: Boolean = true

  def simplifySub: AVector = this

  def invariantAtSub(idx: Int): Boolean = size.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = true

  override def toString: String = "zero(" + size.toString + ")"
}

object AVectorOne {
  def apply(input: InputDesc): AVectorConst = AVectorConst(input, Seq(1.0))
}
case class AVectorConst(val input: InputDesc, val data: Seq[Double]) extends AVector {
  val arity: Int = input.arity
  val size: IRPoly = IRPoly.const(data.size, arity)
  arityVerify()
  def arityOpSub(op: ArityOp): AVector = {
    if(op.xs.length != arity) throw new IRValidationException()
    AVectorConst(input.arityOp(op), data)
  }
  def inputOpSub(op: InputOp): AVector = AVectorConst(op.input, data)
  def is0: Boolean = false
  def isPure: Boolean = true

  def simplifySub: AVector = {
    if(data forall (a => a == 0)) {
      AVectorZero(input, size)
    }
    else {
      this
    }
  }

  def invariantAtSub(idx: Int): Boolean = true
  def memoryInvariantAtSub(idx: Int): Boolean = true

  override def toString: String = "const(...)"
}

object AVectorSum {
  def apply(arg1: AVector, arg2: AVector): AVectorSum = AVectorSum(Seq(arg1, arg2))
}
case class AVectorSum(val args: Seq[AVector]) extends AVector {
  val arity: Int = args(0).arity
  val input: InputDesc = args(0).input
  val size: IRPoly = args(0).size

  for(a <- args.drop(1)) {
    if(a.arity != args(0).arity) throw new IRValidationException()
    if(a.input != args(0).input) throw new IRValidationException()
    if(a.size != args(0).size) {
      println(a.size)
      println(args(0).size)
      throw new IRValidationException()
    }
  }

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = {
    if(op.xs.length != arity) throw new IRValidationException()
    AVectorSum(args map (a => a.arityOp(op)))
  }
  def inputOpSub(op: InputOp): AVector = AVectorSum(args map (a => a.inputOp(op)))

  def is0: Boolean = args forall (a => a.is0)
  def isPure: Boolean = args forall (a => a.isPure)

  def simplifySub: AVector = {
    val sa = args map (a => a.simplify)
    val saf = sa flatMap (a => {
      if(a.is0) {
        Seq()
      }
      else if(a.isInstanceOf[AVectorSum]) {
        a.asInstanceOf[AVectorSum].args
      }
      else {
        Seq(a)
      }
    })
    if(saf.length == 0) {
      AVectorZero(input, size)
    }
    else if(saf.length == 1) {
      saf(0)
    }
    else if(saf forall (s => s.isInstanceOf[AVectorCat])) {
      val safc: Seq[AVectorCat] = saf map (s => s.asInstanceOf[AVectorCat])
      if(safc.drop(1) forall (s => (s.args map (a => a.size)) == (safc(0).args map (a => a.size)))) {
        AVectorCat(for(i <- 0 until safc(0).args.length) yield AVectorSum(safc map (s => s.args(i)))).simplify
      }
      else {
        AVectorSum(saf)
      }
    }
    else {
      AVectorSum(saf)
    }
  }

  def invariantAtSub(idx: Int): Boolean = {
    args forall (a => a.invariantAt(idx))
  }
  def memoryInvariantAtSub(idx: Int): Boolean = {
    args forall (a => a.memoryInvariantAt(idx))
  }

  override def toString: String = "sum(" + args.drop(1).foldLeft(args(0).toString)((a,x) => a + ", " + x.toString) + ")"
}

case class AVectorNeg(val arg: AVector) extends AVector {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input
  val size: IRPoly = arg.size

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorNeg(arg.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorNeg(arg.inputOp(op))

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure

  def simplifySub: AVector = {
    val sa = arg.simplify
    if(sa.is0) {
      AVectorZero(input, size)
    }
    else if(sa.isInstanceOf[AVectorConst]) {
      AVectorConst(input, for(u <- sa.asInstanceOf[AVectorConst].data) yield -u)
    }
    else if(sa.isInstanceOf[AVectorScaleConstant]) {
      val vsa = sa.asInstanceOf[AVectorScaleConstant]
      AVectorScaleConstant(vsa.arg, -vsa.scale)
    }
    else if(sa.isInstanceOf[AVectorNeg]) {
      val vsa = sa.asInstanceOf[AVectorNeg]
      vsa.arg
    }
    else if(sa.isInstanceOf[AVectorCat]) {
      val vsa = sa.asInstanceOf[AVectorCat]
      AVectorCat(vsa.args map (a => AVectorNeg(a))).simplify
    }
    else {
      AVectorNeg(sa)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)

  override def toString: String = "neg(" + arg.toString + ")"
}

case class AVectorScaleConstant(val arg: AVector, val scale: Double) extends AVector {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input
  val size: IRPoly = arg.size

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorScaleConstant(arg.arityOp(op), scale)
  def inputOpSub(op: InputOp): AVector = AVectorScaleConstant(arg.inputOp(op), scale)

  def is0: Boolean = arg.is0 || (scale == 0.0)
  def isPure: Boolean = arg.isPure

  def simplifySub: AVector = {
    val sa = arg.simplify
    if(sa.is0) {
      AVectorZero(input, size)
    }
    else if(scale == 0.0) {
      AVectorZero(input, size)
    }
    else if(scale == 1.0) {
      sa
    }
    else if(scale == -1.0) {
      AVectorNeg(sa).simplify
    }
    else if(sa.isInstanceOf[AVectorConst]) {
      AVectorConst(input, for(u <- sa.asInstanceOf[AVectorConst].data) yield u * scale)
    }
    else if(sa.isInstanceOf[AVectorScaleConstant]) {
      val vsa = sa.asInstanceOf[AVectorScaleConstant]
      AVectorScaleConstant(vsa.arg, vsa.scale * scale)
    }
    else if(sa.isInstanceOf[AVectorNeg]) {
      val vsa = sa.asInstanceOf[AVectorNeg]
      AVectorScaleConstant(vsa.arg, -scale)
    }
    else if(sa.isInstanceOf[AVectorCat]) {
      val vsa = sa.asInstanceOf[AVectorCat]
      AVectorCat(vsa.args map (a => AVectorScaleConstant(a, scale))).simplify
    }
    else {
      AVectorScaleConstant(sa, scale)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)

  override def toString: String = "scale(" + arg.toString + ", " + scale.toString + ")"
}

object AVectorCat {
  def apply(arg1: AVector, arg2: AVector): AVectorCat = AVectorCat(Seq(arg1, arg2))
}
case class AVectorCat(val args: Seq[AVector]) extends AVector {
  val arity: Int = args(0).arity
  val input: InputDesc = args(0).input
  val size: IRPoly = args.drop(1).foldLeft(args(0).size)((a,u) => a + u.size)

  for(a <- args.drop(1)) {
    if(a.arity != args(0).arity) throw new IRValidationException()
    if(a.input != args(0).input) throw new IRValidationException()
  }

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorCat(args map (a => a.arityOp(op)))
  def inputOpSub(op: InputOp): AVector = AVectorCat(args map (a => a.inputOp(op)))

  def is0: Boolean = args forall (a => a.is0)
  def isPure: Boolean = args forall (a => a.isPure)

  def simplifySub: AVector = {
    val sa = args map (a => a.simplify)
    val saf = sa flatMap (a => {
      if(a.size == IRPoly.const(0, arity)) {
        Seq()
      }
      else if(a.isInstanceOf[AVectorCat]) {
        a.asInstanceOf[AVectorCat].args
      }
      else if((a.isInstanceOf[AVectorZero])&&(a.asInstanceOf[AVectorZero].size.isConst)) {
        for(i <- 0 until a.asInstanceOf[AVectorZero].size.asConst) yield AVectorZero(input, IRPoly.const(1, arity))
      }
      else {
        Seq(a)
      }
    })
    if(saf.length == 0) {
      AVectorZero(input, size)
    }
    else {
      val salf = saf.drop(1).foldLeft(Seq(saf(0))) {(a,u) => 
        if((u.is0)&&(a.last.is0)) {
          a.dropRight(1) :+ AVectorZero(input, a.last.size + u.size)
        }
        else if((u.isInstanceOf[AVectorConst])&&(a.last.isInstanceOf[AVectorConst])) {
          a.dropRight(1) :+ AVectorConst(input, a.last.asInstanceOf[AVectorConst].data ++ u.asInstanceOf[AVectorConst].data)
        }
        else if((u.is0)&&(u.size.isConst)&&(a.last.isInstanceOf[AVectorConst])) {
          a.dropRight(1) :+ AVectorConst(input, a.last.asInstanceOf[AVectorConst].data ++ (for(i <- 0 until u.size.asConst) yield 0.0))
        }
        else if((a.last.is0)&&(a.last.size.isConst)&&(u.isInstanceOf[AVectorConst])) {
          a.dropRight(1) :+ AVectorConst(input, (for(i <- 0 until a.last.size.asConst) yield 0.0) ++ u.asInstanceOf[AVectorConst].data)
        }
        else {
          a :+ u
        }
      }
      if(salf.length == 1) {
        salf(0)
      }
      else {
        //here, we put saf instead of salf so that we can open ourselves up to additional transformations
        AVectorCat(saf)
      }
    }
  }

  def invariantAtSub(idx: Int): Boolean = {
    args forall (a => a.invariantAt(idx))
  }
  def memoryInvariantAtSub(idx: Int): Boolean = {
    args forall (a => a.memoryInvariantAt(idx))
  }

  override def toString: String = "cat(" + args.drop(1).foldLeft(args(0).toString)((a,x) => a + ", " + x.toString) + ")"
}

case class AVectorCatFor(val len: IRPoly, val arg: AVector) extends AVector {
  val arity: Int = len.arity
  val input: InputDesc = arg.input.demote
  val size: IRPoly = arg.size.sum(arity).substituteAt(arity, len)

  if(len.arity + 1 != arg.arity) throw new IRValidationException()

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = {
    if(op.xs.length != arity) throw new IRValidationException()
    AVectorCatFor(len.arityOp(op), arg.arityOp(op.leftPromote))
  }
  def inputOpSub(op: InputOp): AVector = AVectorCatFor(len, arg.inputOp(op.promote))

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure  

  def simplifySub: AVector = {
    val sb = arg.simplify
    if(sb.is0) {
      AVectorZero(input, size)
    }
    else if(sb.size == IRPoly.const(0, arity)) {
      AVectorZero(input, size)
    }
    else {
      AVectorCatFor(len, sb)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx) && len.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)

  override def toString: String = "catfor(" + len.toString + ": " + arg.toString + ")"
}

case class AVectorSlice(val arg: AVector, val at: IRPoly, val size: IRPoly) extends AVector {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input

  if(size.arity != arity) throw new IRValidationException()
  if(at.arity != arity) throw new IRValidationException()

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = {
    if(op.xs.length != arity) throw new IRValidationException()
    AVectorSlice(arg.arityOp(op), at.arityOp(op), size.arityOp(op))
  }
  def inputOpSub(op: InputOp): AVector = AVectorSlice(arg.inputOp(op), at, size)

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure

  def simplifySub: AVector = {
    val sb = arg.simplify
    if(sb.is0) {
      AVectorZero(input, size)
    }
    else if(size == IRPoly.const(0, arity)) {
      AVectorZero(input, size)
    }
    else if(sb.isInstanceOf[AVectorConst]&&at.isConst&&size.isConst) {
      AVectorConst(input, sb.asInstanceOf[AVectorConst].data.slice(at.asConst, at.asConst + size.asConst))
    }
    else {
      AVectorSlice(sb, at, size)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx) && at.invariantAt(idx) && size.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)

  override def toString: String = "slice(" + arg.toString + ", " + at.toString + ", " + size.toString + ")"
}

case class AVectorSumFor(val len: IRPoly, val arg: AVector) extends AVector {
  val arity: Int = len.arity
  val input: InputDesc = arg.input.demote
  val size: IRPoly = arg.size.demote

  if(arg.arity != len.arity + 1) throw new IRValidationException()

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorSumFor(len.arityOp(op), arg.arityOp(op.leftPromote))
  def inputOpSub(op: InputOp): AVector = AVectorSumFor(len, arg.inputOp(op.promote))
  
  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure

  def simplifySub: AVector = {
    val sb = arg.simplify
    if(sb.is0) {
      AVectorZero(input, size)
    }
    else if(sb.size == IRPoly.const(0, arity)) {
      AVectorZero(input, size)
    }
    else {
      AVectorSumFor(len, sb)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx) && len.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)

  override def toString: String = "sumfor(" + len.toString + ": " + arg.toString + ")"
}

case class AVectorMpyInput(val arg: AVector, val iidx: Int, val sidx: Seq[IRPoly]) extends AVector {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input
  val size: IRPoly = input.args(iidx).body.codomain.substituteSeq(sidx)

  if(arg.size != input.args(iidx).body.domain.substituteSeq(sidx)) throw new IRValidationException()

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorMpyInput(arg.arityOp(op), iidx, sidx map (s => s.arityOp(op)))
  def inputOpSub(op: InputOp): AVector = {
    if(op.xs.length != input.args.length) throw new IRValidationException()
    for(i <- 0 until input.args.length) {
      if(op.xs(i).body.arity != input.args(i).body.arity) throw new IRValidationException()
    }
    op.xs(iidx).body.substituteSeq(sidx).mmpy(arg.inputOp(op))
  }

  def is0: Boolean = false
  def isPure: Boolean = false

  def simplifySub: AVector = {
    val sa = arg.simplify
    if(sa.is0) {
      AVectorZero(input, size)
    }
    else {
      AVectorMpyInput(sa, iidx, sidx)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx) && (sidx forall (a => a.invariantAt(idx)))
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)

  override def toString: String = "mpyinput(" + arg.toString + ", " + iidx.toString + ", " + sidx.toString + ")"
}

case class AVectorMpyInputT(val arg: AVector, val iidx: Int, val sidx: Seq[IRPoly]) extends AVector {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input
  val size: IRPoly = input.args(iidx).body.domain.substituteSeq(sidx)

  if(arg.size != input.args(iidx).body.codomain.substituteSeq(sidx)) throw new IRValidationException()

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorMpyInputT(arg.arityOp(op), iidx, sidx map (s => s.arityOp(op)))
  def inputOpSub(op: InputOp): AVector = {
    if(op.xs.length != input.args.length) throw new IRValidationException()
    for(i <- 0 until input.args.length) {
      if(op.xs(i).body.arity != input.args(i).body.arity) throw new IRValidationException()
    }
    op.xs(iidx).body.substituteSeq(sidx).T.mmpy(arg.inputOp(op))
  }

  def is0: Boolean = false
  def isPure: Boolean = false

  def simplifySub: AVector = {
    val sa = arg.simplify
    if(sa.is0) {
      AVectorZero(input, size)
    }
    else {
      AVectorMpyInputT(sa, iidx, sidx)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx) && (sidx forall (a => a.invariantAt(idx)))
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)

  override def toString: String = "mpyinputT(" + arg.toString + ", " + iidx.toString + ", " + sidx.toString + ")"
}

case class AVectorRead(val input: InputDesc, val iidx: Int) extends AVector {
  val arity: Int = input.arity
  val size: IRPoly = input.memory(iidx)

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorRead(input.arityOp(op), iidx)
  def inputOpSub(op: InputOp): AVector = {
    if(op.ms.length != input.memory.length) throw new IRValidationException()
    for(i <- 0 until input.memory.length) {
      if(op.ms(i).arity != input.memory(i).arity) throw new IRValidationException()
    }
    op.ms(iidx)
  }

  def is0: Boolean = false
  def isPure: Boolean = false

  def simplifySub: AVector = this

  def invariantAtSub(idx: Int): Boolean = input.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = (idx != iidx)

  override def toString: String = "read(@" + iidx.toString + ")"
}

case class AVectorDot(val arg1: AVector, val arg2: AVector) extends AVector {
  val input: InputDesc = arg1.input
  val arity: Int = input.arity
  val size: IRPoly = IRPoly.const(1, arity)

  if(arg1.size != arg2.size) throw new IRValidationException()
  if(arg1.input != arg2.input) throw new IRValidationException()

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorDot(arg1.arityOp(op), arg2.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorDot(arg1.inputOp(op), arg2.inputOp(op))

  def is0: Boolean = arg1.is0 || arg2.is0
  def isPure: Boolean = arg1.isPure && arg2.isPure

  def simplifySub: AVector = {
    val sa1 = arg1.simplify
    val sa2 = arg2.simplify
    if(sa1.is0 || sa2.is0) {
      AVectorZero(input, size)
    }
    else {
      AVectorDot(sa1, sa2)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg1.invariantAt(idx) && arg2.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg1.memoryInvariantAt(idx) && arg2.memoryInvariantAt(idx)

  override def toString: String = "dot(" + arg1.toString + ", " + arg2.toString + ")"
}

case class AVectorMpy(val arg: AVector, val scale: AVector) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size
  val input: InputDesc = arg.input

  arityVerify()

  if(scale.size != IRPoly.const(1, arity)) throw new IRValidationException()
  if(arg.input != scale.input) throw new IRValidationException()

  def arityOpSub(op: ArityOp): AVector = AVectorMpy(arg.arityOp(op), scale.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorMpy(arg.inputOp(op), scale.inputOp(op))

  def is0: Boolean = arg.is0 || scale.is0
  def isPure: Boolean = arg.isPure && scale.isPure

  def simplifySub: AVector = {
    val sa = arg.simplify
    val ss = scale.simplify
    if(sa.is0 || ss.is0) {
      AVectorZero(input, size)
    }
    else {
      AVectorMpy(sa, ss)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx) && scale.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx) && scale.memoryInvariantAt(idx)
}

case class AVectorDiv(val arg: AVector, val scale: AVector) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = arg.size
  val input: InputDesc = arg.input

  arityVerify()

  if(scale.size != IRPoly.const(1, arity)) throw new IRValidationException()
  if(arg.input != scale.input) throw new IRValidationException()
  if(scale.is0) throw new IRValidationException()

  def arityOpSub(op: ArityOp): AVector = AVectorDiv(arg.arityOp(op), scale.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorDiv(arg.inputOp(op), scale.inputOp(op))

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure && scale.isPure

  def simplifySub: AVector = {
    val sa = arg.simplify
    val ss = scale.simplify
    if(sa.is0) {
      AVectorZero(input, size)
    }
    else {
      AVectorDiv(sa, ss)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx) && scale.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx) && scale.memoryInvariantAt(idx)
}


case class AVectorSqrt(val arg: AVector) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = IRPoly.const(1, arity)
  val input: InputDesc = arg.input

  arityVerify()

  if(arg.size != IRPoly.const(1, arity)) throw new IRValidationException()

  def arityOpSub(op: ArityOp): AVector = AVectorSqrt(arg.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorSqrt(arg.inputOp(op))

  def is0: Boolean = arg.is0
  def isPure: Boolean = arg.isPure

  def simplifySub: AVector = {
    val sa = arg.simplify
    if(sa.is0) {
      AVectorZero(input, size)
    }
    else {
      AVectorSqrt(sa)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)
}

case class AVectorNorm2(val arg: AVector) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = IRPoly.const(1, arity)
  val input: InputDesc = arg.input

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorNorm2(arg.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorNorm2(arg.inputOp(op))

  def is0: Boolean = false //arg.is0
  def isPure: Boolean = arg.isPure

  def simplifySub: AVector = {
    val sa = arg.simplify
    AVectorNorm2(sa)
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)
}

//infinity norm
case class AVectorNormInf(val arg: AVector) extends AVector {
  val arity: Int = arg.arity
  val size: IRPoly = IRPoly.const(1, arity)
  val input: InputDesc = arg.input

  arityVerify()

  def arityOpSub(op: ArityOp): AVector = AVectorNormInf(arg.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorNormInf(arg.inputOp(op))

  def is0: Boolean = false //arg.is0
  def isPure: Boolean = arg.isPure

  def simplifySub: AVector = {
    val sa = arg.simplify
    AVectorNormInf(sa)
  }

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx)
}

case class AVectorMax(val arg1: AVector, val arg2: AVector) extends AVector {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size
  val input: InputDesc = arg1.input

  arityVerify()

  if(arg1.input != arg2.input) throw new IRValidationException()
  if(arg1.size != arg2.size) throw new IRValidationException()

  def arityOpSub(op: ArityOp): AVector = AVectorMax(arg1.arityOp(op), arg2.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorMax(arg1.inputOp(op), arg2.inputOp(op))

  def is0: Boolean = arg1.is0 && arg2.is0
  def isPure: Boolean = arg1.isPure && arg2.is0

  def simplifySub: AVector = {
    val sa1 = arg1.simplify
    val sa2 = arg2.simplify
    if(sa1.is0 && sa2.is0) {
      AVectorZero(input, size)
    }
    else {
      AVectorMax(sa1, sa2)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg1.invariantAt(idx) && arg2.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg1.memoryInvariantAt(idx) && arg2.memoryInvariantAt(idx)
}

case class AVectorMin(val arg1: AVector, val arg2: AVector) extends AVector {
  val arity: Int = arg1.arity
  val size: IRPoly = arg1.size
  val input: InputDesc = arg1.input

  arityVerify()

  if(arg1.input != arg2.input) throw new IRValidationException()
  if(arg1.size != arg2.size) throw new IRValidationException()

  def arityOpSub(op: ArityOp): AVector = AVectorMin(arg1.arityOp(op), arg2.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorMin(arg1.inputOp(op), arg2.inputOp(op))

  def is0: Boolean = arg1.is0 && arg2.is0
  def isPure: Boolean = arg1.isPure && arg2.is0

  def simplifySub: AVector = {
    val sa1 = arg1.simplify
    val sa2 = arg2.simplify
    if(sa1.is0 && sa2.is0) {
      AVectorZero(input, size)
    }
    else {
      AVectorMin(sa1, sa2)
    }
  }

  def invariantAtSub(idx: Int): Boolean = arg1.invariantAt(idx) && arg2.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg1.memoryInvariantAt(idx) && arg2.memoryInvariantAt(idx)
}

case class AVectorTolerance(val input: InputDesc) extends AVector {
  val arity: Int = input.arity
  val size: IRPoly = IRPoly.const(1, arity)

  def arityOpSub(op: ArityOp): AVector = AVectorTolerance(input.arityOp(op))
  def inputOpSub(op: InputOp): AVector = AVectorTolerance(op.input)

  def is0: Boolean = false
  def isPure: Boolean = false

  def simplifySub: AVector = this

  def invariantAtSub(idx: Int): Boolean = true
  def memoryInvariantAtSub(idx: Int): Boolean = true
}

case class AVectorConverge(val arg: AVector, val cond: AVector, val body: AVector, val itermax: Int) extends AVector {
  val arity: Int = arg.arity
  val input: InputDesc = arg.input
  val size: IRPoly = body.size

  if(body.input != arg.input.pushMemory(arg.size)) throw new IRValidationException()
  if(body.size != arg.size) throw new IRValidationException()
  if(cond.input != arg.input.pushMemory(arg.size))
  if(cond.size != IRPoly.const(1, arity)) throw new IRValidationException()

  def arityOpSub(op: ArityOp): AVector = {
    AVectorConverge(arg.arityOp(op), cond.arityOp(op), body.arityOp(op), itermax)
  }
  def inputOpSub(op: InputOp): AVector = {
    AVectorConverge(arg.inputOp(op), cond.inputOp(op.pushMemoryLeft(arg.size)), body.inputOp(op.pushMemoryLeft(arg.size)), itermax)
  }

  def is0: Boolean = body.is0
  def isPure: Boolean = arg.isPure && body.isPure && cond.isPure

  def simplifySub: AVector = AVectorConverge(arg.simplify, cond.simplify, body.simplify, itermax)

  def invariantAtSub(idx: Int): Boolean = arg.invariantAt(idx) && cond.invariantAt(idx) && body.invariantAt(idx)
  def memoryInvariantAtSub(idx: Int): Boolean = arg.memoryInvariantAt(idx) && cond.memoryInvariantAt(idx) && body.memoryInvariantAt(idx)
}

// case class AVectorPromoted(val arg: AVector) extends AVector {
//   val arity: Int = arg.arity + 1
//   val input: InputDesc = arg.input.promote
//   val size: IRPoly = arg.size.promote

//   def arityOpSub(op: ArityOp): AVector = {
//     val dop: ArityOp = ArityOp(op.arity, op.xs.dropRight(1))
//     arg.arityOp(dop)
//   }
//   def inputOpSub(op: InputOp): AVector = AVectorPromoted(arg.inputOp(op.demote))

//   def is0: Boolean = arg.is0
//   def isPure: Boolean = arg.isPure

//   def simplifySub: AVector = AVectorPromoted(arg.simplify)
// }

// case class AVectorMemoryPushed(val arg: AVector, val memsize: IRPoly) extends AVector {
//   val arity: Int = arg.arity
//   val input: InputDesc = arg.input.pushMemory(memsize)
//   val size: IRPoly = arg.size

//   if(memsize.arity != arity) throw new IRValidationException()

//   def arityOpSub(op: ArityOp): AVector = AVectorMemoryPushed(arg.arityOp(op), memsize.arityOp(op))
//   def inputOpSub(op: InputOp): AVector = {
//     val dop: InputOp = InputOp(op.input, op.xs, op.ms.dropRight(1))
//     arg.inputOp(dop)
//   }

//   def is0: Boolean = arg.is0
//   def isPure: Boolean = arg.isPure

//   def simplifySub: AVector = AVectorMemoryPushed(arg.simplify, memsize)
// }

