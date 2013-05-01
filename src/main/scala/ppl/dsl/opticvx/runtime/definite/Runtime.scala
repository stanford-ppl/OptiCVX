package ppl.dsl.opticvx.runtime.definite

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq

import ppl.dsl.opticvx.solvers._


object SolverRuntimeDefinite extends SolverRuntime {
  def compile(v: AVector): SolverCompiled = new SolverCompiledDefinite(v)
}

class SolverCompiledDefinite(v: AVector) extends SolverCompiled {
  def eval(params: Seq[Int], inputs: Seq[MultiSeq[DMatrix]], memory: Seq[Seq[Double]], tolerance: Double): SolverResult = {
    val evaler = new SolverEvalDefinite(params, inputs, memory, tolerance, null)
    val start = System.currentTimeMillis()
    val data = evaler.eval(v)
    val elapsed = System.currentTimeMillis() - start
    SolverResult(data, evaler.iterationct, elapsed * 0.001)
  }
}

class SolverEvalDefinite(params: Seq[Int], inputs: Seq[MultiSeq[DMatrix]], memory: Seq[Seq[Double]], tolerance: Double, inherit: SolverEvalDefinite) {
  var iterationct: Int = 0
  val memocache = new scala.collection.mutable.HashMap[AVector, Seq[Double]]()

  def memolookup(v: AVector): Seq[Double] = {
    val rv = memocache.getOrElse(v, null)
    if((rv == null)&&(inherit!=null)) {
      inherit.memolookup(v)
    }
    else {
      rv
    }
  }

  def eval(v: AVector): Seq[Double] = {
    val memo = memolookup(v)
    if(memo != null) return memo

    v match {
      case AVectorZero(input, size) =>
        for(i <- 0 until size.eval(params)(IntLikeInt)) yield 0.0
      case AVectorConst(input, data) =>
        data
      case AVectorSum(args) => {
        val avs = args map (a => eval(a))
        for(i <- 0 until avs(0).length) yield avs.foldLeft(0.0)((a,x) => a + x(i))
      }
      case AVectorNeg(arg) => 
        eval(arg) map ((a) => -a)
      case AVectorScaleConstant(arg, scale) =>
        eval(arg) map ((a) => a * scale)
      case AVectorCat(args) => {
        val avs = args map (a => eval(a))
        avs.foldLeft(Seq[Double]())((a,b) => a ++ b)
      }
      case AVectorCatFor(len, arg) => {
        var rv: Seq[Double] = Seq()
        for(j <- 0 until len.eval(params)(IntLikeInt)) {
          val ev = new SolverEvalDefinite(params :+ j, inputs, memory, tolerance, this)
          rv ++ ev.eval(arg)
        }
        rv
      }
      case AVectorSlice(arg, at, size) => {
        val vs = eval(arg)
        vs.slice(at.eval(params)(IntLikeInt), (at+size).eval(params)(IntLikeInt))
      }
      case AVectorSumFor(len, arg) => {
        var rv: Seq[Double] = for(i <- 0 until v.size.eval(params)(IntLikeInt)) yield 0.0
        for(j <- 0 until len.eval(params)(IntLikeInt)) {
          val ev = new SolverEvalDefinite(params :+ j, inputs, memory, tolerance, this)
          val evv = ev.eval(arg)
          rv = for(i <- 0 until rv.length) yield rv(i) + evv(i)
        }
        rv
      }
      case AVectorMpyInput(arg, iidx, sidx) => {
        val vs = eval(arg)
        val m = inputs(iidx).apply(sidx map (s => s.eval(params)(IntLikeInt)))
        m.mmpy(vs)
      }
      case AVectorMpyInputT(arg, iidx, sidx) => {
        val vs = eval(arg)
        val m = inputs(iidx).apply(sidx map (s => s.eval(params)(IntLikeInt)))
        m.mmpyT(vs)
      }
      case _ =>
        throw new IRValidationException()
    }
  }
}

/*
class SolverRuntimeDefinite(val tol: Double) extends SolverRuntime[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]] {
  //INTEGER OPERATIONS
  def intlikei: IntLike[Int] = IntLikeInt

  //VECTOR OPERATIONS
  //base objects
  def size(arg: Seq[Double]): Int = arg.length
  def zero(size: Int): Seq[Double] = for(i <- 0 until size) yield 0.0
  def const(data: Seq[Double]): Seq[Double] = data
  //linear operators
  def sum(args: Seq[Seq[Double]]): Seq[Double] = {
    for(a <- args) {
      if(a.length != args(0).length) throw new IRValidationException()
    }
    for(i <- 0 until args(0).length) yield args.foldLeft(0.0)((a,x) => a + x(i))
  }
  def sumfor(len: Int, size: Int, arg: (Int => Seq[Double])): Seq[Double] = {
    var rv: Seq[Double] = for (i <- 0 until size) yield 0.0
    for(j <- 0 until len) {
      val aj = arg(j)
      rv = for (i <- 0 until size) yield rv(i) + aj(i)
    }
    rv
  }
  def neg(arg: Seq[Double]): Seq[Double] = {
    for(a <- arg) yield -a
  }
  def scaleconstant(arg: Seq[Double], scale: Double): Seq[Double] = {
    for(a <- arg) yield a*scale
  }
  def cat(args: Seq[Seq[Double]]): Seq[Double] = {
    args.foldLeft(Seq[Double]())((a,b) => a ++ b)
  }
  def catfor(len: Int, size: Int, arg: (Int => Seq[Double])): Seq[Double] = {
    var rv: Seq[Double] = Seq()
    for(j <- 0 until len) {
      rv = rv ++ arg(j)
    }
    rv
  }
  def slice(arg: Seq[Double], at: Int, size: Int): Seq[Double] = {
    arg.slice(at, at + size)
  }
  //nonlinear operators
  def dot(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    if(arg1.length != arg2.length) throw new IRValidationException()
    Seq((0 until arg1.length).foldLeft(0.0)((a, i) => a + arg1(i)*arg2(i)))
  }
  def mpy(arg: Seq[Double], scale: Seq[Double]): Seq[Double] = {
    if(scale.length != 1) throw new IRValidationException()
    for (a <- arg) yield a * scale(0)
  }
  def div(arg: Seq[Double], scale: Seq[Double]): Seq[Double] = {
    if(scale.length != 1) throw new IRValidationException()
    for (a <- arg) yield a / scale(0)
  }
  def norm2(arg: Seq[Double]): Seq[Double] = {
    Seq(arg.foldLeft(1e-300)((a, x) => a + x*x))
  }
  def norm_inf(arg: Seq[Double]): Seq[Double] = {
    Seq(arg.foldLeft(1e-300)((a, x) => scala.math.max(a, scala.math.abs(x))))
  }
  def sqrt(arg: Seq[Double]): Seq[Double] = {
    if(arg.length != 1) throw new IRValidationException()
    Seq(scala.math.sqrt(arg(0)))
  }
  def max(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    if(arg1.length != arg2.length) throw new IRValidationException()
    for (i <- 0 until arg1.length) yield scala.math.max(arg1(i), arg2(i))
  }
  def min(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    if(arg1.length != arg2.length) throw new IRValidationException()
    for (i <- 0 until arg1.length) yield scala.math.min(arg1(i), arg2(i))
  }
  def tolerance(): Seq[Double] = Seq(tol)


  def matrixmpy(m: MatrixDefinite, osize: Int, x: Seq[Double]): Seq[Double] = m.mmpy(x)
  def matrixmpytranspose(m: MatrixDefinite, osize: Int, x: Seq[Double]): Seq[Double] = m.mmpyT(x)

  def matrixget(mats: MultiSeq[MatrixDefinite], at: Seq[Int]): MatrixDefinite = mats(at)

  def vectorget(vecs: MultiSeq[Seq[Double]], size: Int, at: Seq[Int]): Seq[Double] = vecs(at)
  def vectorset(src: Seq[Double], vecs: MultiSeq[Seq[Double]], at: Seq[Int]): MultiSeq[Seq[Double]] = {
    if(vecs(at).length != src.length) throw new IRValidationException()
    vecs.updated(at, src)
  }
  // def vectorput(src: Seq[Double]): MultiSeq[Seq[Double]] = MultiSeqA0(src)

  //case class MemAllocDesc(val dims: Seq[I], val size: Seq[I] => I)
  def memoryallocfor(dim: Int, ar: Int, body: Int => MultiSeq[Seq[Double]]): MultiSeq[Seq[Double]] = {
    MultiSeqN(ar, for(i <- 0 until dim) yield body(i))
  }

  def memoryalloc(size: Int): MultiSeq[Seq[Double]] = {
    MultiSeqA0(for(i <- 0 until size) yield 0.0)
  }

  var converge_iter_count: Int = 0
  var converge_loop_depth: Int = 0
  def converge(memory: Seq[MultiSeq[Seq[Double]]], itermax: Int, body: (Seq[MultiSeq[Seq[Double]]]) => (Seq[MultiSeq[Seq[Double]]], Seq[Double])): Seq[MultiSeq[Seq[Double]]] = {
    var m = memory
    var cond: Boolean = true
    var i = 0
    while(cond && ((itermax == -1)||(i < itermax))) {
      val old_iter_count: Int = converge_iter_count
      converge_loop_depth += 1
      val (nm, v) = body(m)
      converge_loop_depth -= 1
      if(v.length != 1) throw new IRValidationException()
      cond = (v(0) > 0.0)
      m = nm
      if(old_iter_count == converge_iter_count) {
        converge_iter_count += 1
      }
      //println("  " * converge_loop_depth + "cond: " + v(0).toString)
      i += 1
    }
    m
  }
  def runfor(len: Int, memory: Seq[MultiSeq[Seq[Double]]], body: (Int, Seq[MultiSeq[Seq[Double]]]) => Seq[MultiSeq[Seq[Double]]]): Seq[MultiSeq[Seq[Double]]] = {
    var m = memory
    for(i <- 0 until len) {
      m = body(i, m)
    }
    m
  }
}
*/