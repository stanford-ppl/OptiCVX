package ppl.dsl.opticvx.runtime.cgen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import ppl.dsl.opticvx.solvers._

import java.io._
import java.lang.Runtime


object Sym {
  def subst(code: String, repls: Seq[Tuple2[String, Sym]]): String = {
    val rv = repls.foldLeft(code)((s, r) => s.replace("$" + r._1, r._2.name))
    if(rv.contains("$")) throw new IRValidationException()
    rv
  }
}

trait Sym {
  def name: String
}

trait IntSym extends Sym
case class IntSymD(val name: String) extends IntSym
case class IntSymL(val i: Int) extends IntSym {
  val name: String = i.toString
}
object IntLikeIntSym extends IntLike[IntSym] {
  def add(x: IntSym, y: IntSym): IntSym = (x,y) match {
    case (IntSymL(xi), IntSymL(yi)) => IntSymL(xi + yi)
    case (IntSymL(0), _) => y
    case (_, IntSymL(0)) => x
    case _ => IntSymD("(" + x.name + " + " + y.name + ")")
  }
  def neg(x: IntSym): IntSym = x match {
    case IntSymL(xi) => IntSymL(-xi)
    case _ => IntSymD("(-" + x.name + ")")
  }
  def multiply(x: IntSym, y: IntSym): IntSym = (x,y) match {
    case (IntSymL(xi), IntSymL(yi)) => IntSymL(xi * yi)
    case (IntSymL(0), _) => IntSymL(0)
    case (_, IntSymL(0)) => IntSymL(0)
    case (IntSymL(1), _) => y
    case (_, IntSymL(1)) => x
    case _ => IntSymD("(" + x.name + " * " + y.name + ")")
  }
  def divide(x: IntSym, r: Int): IntSym = (x, r) match {
    case (IntSymL(xi), _) => IntSymL(xi / r)
    case (_, 1) => x
    case _ => IntSymD("(" + x.name + " / " + r.toString + ")")
  }
  implicit def int2T(x: Int): IntSym = IntSymL(x)
}

trait DoubleSym extends Sym
case class DoubleSymD(val name: String) extends DoubleSym
case class DoubleSymL(val d: Double) extends DoubleSym {
  val name: String = d.toString
}
object DoubleLikeDoubleSym {
  def add(x: DoubleSym, y: DoubleSym): DoubleSym = (x,y) match {
    case (DoubleSymL(xi), DoubleSymL(yi)) => DoubleSymL(xi + yi)
    case (DoubleSymL(0.0), _) => y
    case (_, DoubleSymL(0.0)) => x
    case _ => DoubleSymD("(" + x.name + " + " + y.name + ")")
  }
  def neg(x: DoubleSym): DoubleSym = x match {
    case DoubleSymL(xi) => DoubleSymL(-xi)
    case _ => DoubleSymD("(-" + x.name + ")")
  }
  def multiply(x: DoubleSym, y: DoubleSym): DoubleSym = (x,y) match {
    case (DoubleSymL(xi), DoubleSymL(yi)) => DoubleSymL(xi * yi)
    case (DoubleSymL(0.0), _) => DoubleSymL(0.0)
    case (_, DoubleSymL(0.0)) => DoubleSymL(0.0)
    case (DoubleSymL(1.0), _) => y
    case (_, DoubleSymL(1.0)) => x
    case _ => DoubleSymD("(" + x.name + " * " + y.name + ")")
  }
  def divide(x: DoubleSym, y: DoubleSym): DoubleSym = (x,y) match {
    case (DoubleSymL(xi), DoubleSymL(yi)) => DoubleSymL(xi / yi)
    case (DoubleSymL(0.0), _) => DoubleSymL(0.0)
    case (_, DoubleSymL(0.0)) => throw new Exception("symbol divide by zero")
    case (_, DoubleSymL(1.0)) => x
    case _ => DoubleSymD("(" + x.name + " * " + y.name + ")")
  }
  implicit def double2T(x: Double): DoubleSym = DoubleSymL(x)

  class DoubleLikeDoubleHack(val t: DoubleSym) {
    def unary_-(): DoubleSym = neg(t)
    def +(u: DoubleSym) = add(t, u)
    def -(u: DoubleSym) = add(t, neg(u))
    def *(u: DoubleSym) = multiply(t, u)
    def /(u: DoubleSym) = divide(t, u)
  }
  implicit def doublelikedoublehackimpl(t: DoubleSym) = new DoubleLikeDoubleHack(t)
}

