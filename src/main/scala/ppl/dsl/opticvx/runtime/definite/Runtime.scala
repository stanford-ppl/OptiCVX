package ppl.dsl.opticvx.runtime.definite

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq

import ppl.dsl.opticvx.solvers._

// TODO: MatrixDefinite class has opposite domain/codomain order from other classes
case class MatrixDefinite(m: Int, n: Int, data: Seq[Double]) {
  if(data.length != m*n) throw new IRValidationException()
  def getat(i: Int, j: Int): Double = {
    if((i < 0)||(i >= m)||(j < 0)||(j >= n)) throw new IRValidationException()
    data(i + m*j)
  }
  def mmpy(x: Seq[Double]) = {
    if(x.length != n) throw new IRValidationException()
    for (i <- 0 until m) yield {
      (0 until n).foldLeft(0.0)((a, j) => a + getat(i, j)*x(j))
    }
  }
  def mmpyT(x: Seq[Double]) = {
    if(x.length != m) throw new IRValidationException()
    for (i <- 0 until n) yield {
      (0 until m).foldLeft(0.0)((a, j) => a + getat(j, i)*x(j))
    }
  }
  def formatc: String = {
    var rv: String = ""
    for(i <- 0 until data.length) {
      if(i == 0) {
        rv += "[ "
      }
      else if(i % n == 0) {
        rv += "; "
      }
      else {
        rv += ", "
      }
      rv += data(i).toString
    }
    rv += " ]"
    rv
  }
}

class SolverRuntimeDefinite(val tol: Double) extends SolverRuntime[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]] {
  //INTEGER OPERATIONS
  def intlikei: IntLike[Int] = IntLikeInt

  //VECTOR OPERATIONS
  //base objects
  def size(arg: Seq[Double]): Int = arg.length
  def zero(size: Int): Seq[Double] = for(i <- 0 until size) yield 0.0
  def one: Seq[Double] = Seq(1.0)
  //linear operators
  def sum(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    if(arg1.length != arg2.length) throw new IRValidationException()
    for(i <- 0 until arg1.length) yield arg1(i) + arg2(i)
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
  def cat(arg1: Seq[Double], arg2: Seq[Double]): Seq[Double] = {
    arg1 ++ arg2
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
      if(converge_loop_depth == 0) {
        println(v(0))
      }
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
