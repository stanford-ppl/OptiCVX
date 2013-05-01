package ppl.dsl.opticvx.model

import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.common._
import scala.collection.immutable.Seq

trait SolverRuntime {
  def compile(v: AVector): SolverCompiled
}
trait SolverCompiled {
  def eval(params: Seq[Int], inputs: Seq[MultiSeq[DMatrix]], memory: Seq[Seq[Double]], tolerance: Double): SolverResult
}
case class SolverResult(data: Seq[Double], iterationct: Int, time: Double)

case class DMatrix(domain: Int, codomain: Int, data: Seq[Double]) {
  val m = codomain
  val n = domain
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


