package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import ppl.dsl.opticvx.solvers._


case class MatrixDefinite(m: Int, n: Int, data: Seq[Double]) {
  if(data.length != m*n) throw new IRValidationException()
  def getat(i: Int, j: Int): Double = {
    if((i < 0)||(i >= m)||(j < 0)||(j >= n)) throw new IRValidationException()
    data(i + m*j)
  }
  def mmpy(x: Seq[Double]) = {
    for (i <- 0 until m) yield {
      (0 until m).foldLeft(0.0)((a, j) => a + getat(i, j)*x(j))
    }
  }
  def mmpyT(x: Seq[Double]) = {
    for (i <- 0 until m) yield {
      (0 until m).foldLeft(0.0)((a, j) => a + getat(j, i)*x(j))
    }
  }
}

object SolverRuntimeDefinite extends SolverRuntime[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]] {
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
  def catfor(len: Int, arg: (Int => Seq[Double])): Seq[Double] = {
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


  def matrixmpy(m: MatrixDefinite, x: Seq[Double]): Seq[Double] = m.mmpy(x)
  def matrixmpytranspose(m: MatrixDefinite, x: Seq[Double]): Seq[Double] = m.mmpyT(x)

  def matrixget(mats: MultiSeq[MatrixDefinite], at: Seq[Int]): MatrixDefinite = mats(at)

  def vectorget(vecs: MultiSeq[Seq[Double]], at: Seq[Int]): Seq[Double] = vecs(at)
  def vectorset(src: Seq[Double], vecs: MultiSeq[Seq[Double]], at: Seq[Int]): MultiSeq[Seq[Double]] = {
    if(vecs(at).length != src.length) throw new IRValidationException()
    vecs.updated(at, src)
  }

  //case class MemAllocDesc(val dims: Seq[I], val size: Seq[I] => I)
  def memoryalloc(dims: Seq[Int], size: Seq[Int] => Int): MultiSeq[Seq[Double]] = {
    for(d <- desc) {

    }
  }

  var converge_iter_count: Int = -1
  def converge(memory: Seq[MultiSeq[Seq[Double]]], itermax: Int, body: (Seq[MultiSeq[Seq[Double]]]) => (Seq[MultiSeq[Seq[Double]]], Seq[Double])): Seq[MultiSeq[Seq[Double]]] = {
    var m = memory
    var cond: Boolean = true
    val is_outerloop: Boolean = (converge_iter_count == -1)
    if(is_outerloop) {
      converge_iter_count = 0
    }
    var i = 0
    while(cond && ((itermax == -1)||(i < itermax))) {
      val (nm, v) = body(m)
      if(v.length != 1) throw new IRValidationException()
      cond = (v(0) > 0.0)
      m = nm
      if(!is_outerloop) {
        converge_iter_count += 1
      }
      else {
        //println(v(0))
      }
      i += 1
    }
    if(is_outerloop) {
      println("converged in " + converge_iter_count.toString + " iterations")
      converge_iter_count = -1
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
