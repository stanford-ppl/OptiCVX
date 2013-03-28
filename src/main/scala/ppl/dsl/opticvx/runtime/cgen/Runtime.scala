package ppl.dsl.opticvx.runtime.cgen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import ppl.dsl.opticvx.solvers._


class IntSym(val i: Int) {
  def name: String = "i" + i.toString
}
class VectorSym(val i: Int, val size: IntSym) {
  def name: String = "x" + i.toString
}
class MemorySym(val i: Int) {
  def name: String = "m" + i.toString
}
class MatrixSym(val i: Int) {
  def name: String = "a" + i.toString
}
class InputSym(val i: Int) {
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

  private def emit(code: String) {

  }

  object intlikei extends IntLike[IntSym] {
    def add(x: IntSym, y: IntSym): IntSym = {
      val rv = nextint
      emit("int " + rv.name + " = " + x.name + " + " + y.name + ";")
      rv
    }
    def neg(x: IntSym): IntSym = {
      val rv = nextint
      emit("int " + rv.name + " = -" + x.name + ";")
      rv
    }
    def multiply(x: IntSym, y: IntSym): IntSym = {
      val rv = nextint
      emit("int " + rv.name + " = " + x.name + " * " + y.name + ";")
      rv
    }
    def divide(x: IntSym, r: Int): IntSym = {
      val rv = nextint
      emit("int " + rv.name + " = " + x.name + " / " + r.toString + ";")
      rv
    }
    implicit def int2T(x: Int): IntSym = {
      val rv = nextint
      emit("int " + rv.name + " = " + x.toString + ";")
      rv
    }
  }


  //VECTOR OPERATIONS
  //base objects
  def size(arg: VectorSym): IntSym = arg.size
  def zero(size: IntSym): VectorSym = {
    val rv = nextvector(size)
    emit("double " + rv.name + "[" + size.name + "];")
    emit("for(itmp = 0; itmp < " + size.name + "; itmp++) " + rv.name + "[itmp] = 0.0;")
    rv
  } 
  def one: VectorSym = {
    val rv = nextvector(size)
    emit("double " + rv.name + "[1] = {1.0};")
    rv
  }
  //linear operators
  def sum(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    val rv = nextvector(arg1.size)
    emit("for(itmp = 0; itmp < " + arg1.size.name + "; itmp++) " + rv.name + "[itmp] = " + arg1.name + "[itmp] + " + arg2.name + "[itmp];")
    rv
  }
  def sumfor(len: I, size: I, arg: (I => V)): V
  def neg(arg: V): V
  def scaleconstant(arg: V, scale: Double): V
  def cat(arg1: V, arg2: V): V
  def catfor(len: I, arg: (I => V)): V
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
}