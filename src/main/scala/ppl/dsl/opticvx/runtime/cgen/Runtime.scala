package ppl.dsl.opticvx.runtime.cgen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import ppl.dsl.opticvx.solvers._

trait Sym {
  def name: String
}
class IntSym(val i: Int) extends Sym {
  def name: String = "i" + i.toString
}
class VectorSym(val i: Int, val size: IntSym) extends Sym {
  def name: String = "x" + i.toString
}
class MemorySym(val i: Int) extends Sym {
  def name: String = "m" + i.toString
}
class MatrixSym(val i: Int) extends Sym {
  def name: String = "a" + i.toString
}
class InputSym(val i: Int) extends Sym {
  def name: String = "b" + i.toString
}

class SolverRuntimeCGen extends SolverRuntime[IntSym, MatrixSym, InputSym, VectorSym, MemorySym] {
  private var intsym_ni: Int = 0
  private def nextint: IntSym = {
    intsym_ni += 1
    new IntSym(intsym_ni)
  }
  private var vectorsym_ni: Int = 0
  private def nextvector(size: IntSym): VectorSym = {
    vectorsym_ni += 1
    new VectorSym(vectorsym_ni, size)
  }
  private var memorysym_ni: Int = 0
  private def nextmemory: MemorySym = {
    memorysym_ni += 1
    new MemorySym(memorysym_ni)
  }
  private var matrixsym_ni: Int = 0
  private def nextmatrix: MatrixSym = {
    matrixsym_ni += 1
    new MatrixSym(matrixsym_ni)
  }
  private var inputsym_ni: Int = 0
  private def nextinput: InputSym = {
    inputsym_ni += 1
    new InputSym(inputsym_ni)
  }

  private def emit(code: String, repls: Tuple2[String, Sym]*) {

  }

  object intlikei extends IntLike[IntSym] {
    def add(x: IntSym, y: IntSym): IntSym = {
      val rv = nextint
      emit("int %rv = %x + %y;", "rv" -> rv, "x" -> x, "y" -> y)
      rv
    }
    def neg(x: IntSym): IntSym = {
      val rv = nextint
      emit("int %rv = -%x;", "rv" -> rv, "x" -> x)
      rv
    }
    def multiply(x: IntSym, y: IntSym): IntSym = {
      val rv = nextint
      emit("int %rv = %x * %y;", "rv" -> rv, "x" -> x, "y" -> y)
      rv
    }
    def divide(x: IntSym, r: Int): IntSym = {
      val rv = nextint
      emit("int %rv = %x / " + r.toString + ";", "rv" -> rv, "x" -> x)
      rv
    }
    implicit def int2T(x: Int): IntSym = {
      val rv = nextint
      emit("int %rv = " + x.toString + ";", "rv" -> rv)
      rv
    }
  }


  //VECTOR OPERATIONS
  //base objects
  def size(arg: VectorSym): IntSym = arg.size
  def zero(size: IntSym): VectorSym = {
    val rv = nextvector(size)
    val i = nextint
    emit("""
      double %rv[%size];
      for(int %i = 0; %i < %size; %i++) %rv[%i] = 0.0;
      """, 
      "rv" -> rv, "i" -> i, "size" -> size)
    rv
  } 
  def one: VectorSym = {
    val rv = nextvector(intlikei.int2T(1))
    emit("double %rv[1] = {1.0};", "rv" -> rv)
    rv
  }
  //linear operators
  def sum(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    val rv = nextvector(arg1.size)
    val i = nextint
    emit("""
      double %rv[%size];
      for(int %i = 0; %i < %size; %i++) %rv[%i] = %x[%i] + %y[%i];
      """,
      "rv" -> rv, "i" -> i, "x" -> arg1, "y" -> arg2, "size" -> arg1.size)
    rv
  }
  def sumfor(len: IntSym, size: IntSym, arg: (IntSym => VectorSym)): VectorSym = {
    val rv = nextvector(size)
    val i = nextint
    val j = nextint
    emit("""
      double %rv[%size];
      for(int %j = 0; %j < %len; %j++) %rv[%j] = 0.0;
      for(int %i = 0; %i < %len, %i++) {
      """,
      "rv" -> rv, "size" -> size, "len" -> len, "i" -> i, "j" -> j)
    val va = arg(i)
    emit("""
      for(int %j = 0; %j < %len; %j++) %rv[%j] += %va[%j];
      }
      """,
      "rv" -> rv, "va" -> va, "size" -> size, "len" -> len, "i" -> i, "j" -> j)
    return rv
  }
  def neg(arg: VectorSym): VectorSym = {
    val rv = nextvector(arg.size)
    val i = nextint
    emit("""
      double %rv[%size];
      for(int %i = 0; %i < %size; %i++) %rv[%i] = -%x[%i];
      """,
      "rv" -> rv, "i" -> i, "x" -> arg, "size" -> arg.size)
    rv
  }
  def scaleconstant(arg: VectorSym, scale: Double): VectorSym = {
    val rv = nextvector(arg.size)
    val i = nextint
    emit("double %rv[%size];\nfor(int %i = 0; %i < %size; %i++) %rv[%i] = %x[%i] * " + scale.toString + ";",
      "rv" -> rv, "i" -> i, "x" -> arg, "size" -> arg.size)
    rv
  }
  def cat(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    val irv = intlikei.add(arg1.size, arg2.size)
    val rv = nextvector(irv)
    val i = nextint
    val j = nextint
    emit("""
      double %rv[%size];
      for(int %i = 0; %i < %xsize; %i++) %rv[%i] = %x[%i];
      for(int %j = 0; %j < %ysize; %j++) %rv[%j + %xsize] = %y[%j];
      """,
      "rv" -> rv, "size" -> irv, "x" -> arg1, "y" -> arg2, "xsize" -> arg1.size, "ysize" -> arg2.size, "i" -> i, "j" -> j)
    rv
  }
  def catfor(len: IntSym, size: IntSym, arg: (IntSym => VectorSym)): VectorSym = {
    val rv = nextvector(size)
    val i = nextint
    val j = nextint
    val k = nextint
    emit("""
      double %rv[%size];
      int %k = 0;
      for(int %i = 0; %i < %len; %i++) {
      """,
      "rv" -> rv, "len" -> len, "i" -> i, "j" -> j, "k" -> k, "size" -> size)
    val va = arg(i)
    emit("""
      for(int %j = 0; %j < %vasize; %j++, %k++) %rv[%k] = %va[%j];
      }
      """,
      "rv" -> rv, "len" -> len, "va" -> va, "vasize" -> va.size, "i" -> i, "j" -> j, "k" -> k, "size" -> size)
    rv
  }
  /*
  def slice(arg: V, at: I, size: I): V
  //nonlinear operators
  def dot(arg1: V, arg2: V): V
  def mpy(arg: V, scale: V): V
  def div(arg: V, scale: V): V
  def norm2(arg: V): V
  def sqrt(arg: V): V
  def max(arg1: V, arg2: V): V
  def min(arg1: V, arg2: V): V
  def norm_inf(arg: V): V


  def matrixmpy(m: M, x: V): V
  def matrixmpytranspose(m: M, x: V): V

  def matrixget(mats: N, at: Seq[I]): M

  def vectorget(vecs: W, at: Seq[I]): V
  def vectorset(src: V, vecs: W, at: Seq[I]): W

  def memoryallocfor(dim: I, ar: Int, body: I => W): W
  def memoryalloc(size: I): W

  def converge(memory: Seq[W], itermax: Int, body: (Seq[W]) => (Seq[W], V)): Seq[W]
  def runfor(len: I, memory: Seq[W], body: (I, Seq[W]) => Seq[W]): Seq[W]
  */
}