package ppl.dsl.opticvx.runtime.cgen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import ppl.dsl.opticvx.solvers._

trait Sym {
  def name: String
}
class FlatSym(val name: String) extends Sym
trait IntSym extends Sym
class IntSymL(val i: Int) extends IntSym {
  def name: String = "i" + i.toString
}
class IntSymD(val c: Int) extends IntSym {
  def name: String = c.toString
}
class IntSymM(val name: String) extends IntSym
trait VectorSym {
  val size: IntSym
  def at(idx: IntSym): Sym
}
class VectorSymL(val i: Int, val size: IntSym) extends Sym with VectorSym {
  def name: String = "x" + i.toString
  def at(idx: IntSym): Sym = new FlatSym("x" + i.toString + "[" + idx.name + "]")
}
class VectorSymFlat(val vss: String, val size: IntSym) extends VectorSym {
  def at(idx: IntSym): Sym = new FlatSym(vss.replace("$", idx.name))
}
class MemorySym(val i: Int) extends Sym {
  private var valid: Boolean = true
  def name: String = {
    if(!valid) throw new IRValidationException()
    "m" + i.toString
  }
  def copyinvalidate: MemorySym = {
    valid = false
    new MemorySym(i)
  }
}
class MatrixSym(val i: Int) extends Sym {
  def name: String = "a" + i.toString
}
class InputSym(val i: Int) extends Sym {
  def name: String = "inputs[" + i.toString + "]"
}
case class InputDescCGen(dims: Seq[Seq[IntSym]=>IntSym], domain: Seq[IntSym] => IntSym, codomain: Seq[IntSym] => IntSym)


class SolverRuntimeCGen(val arity: Int) extends SolverRuntime[IntSym, MatrixSym, InputSym, VectorSym, MemorySym] {
  private var intsym_ni: Int = 0
  private def nextint: IntSym = {
    intsym_ni += 1
    new IntSymL(intsym_ni)
  }
  private var vectorsym_ni: Int = 0
  private def nextvector(size: IntSym): VectorSymL = {
    vectorsym_ni += 1
    new VectorSymL(vectorsym_ni, size)
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

  private var inputdescs: Seq[InputDescCGen] = null
  def setinputs(is: Seq[InputDescCGen]) {
    if(inputdescs != null) throw new IRValidationException()
    inputdescs = is
  }

  def code: String = {
    if(!output_written) throw new IRValidationException()
    var rv = """
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "../src/solver.h"

typedef union memory_t {
  union memory_t* idx[0];
  double vec[0];
} memory_t;

//returns the number of inner loop iterations required to converge
static solution_t solve(int* params, input_t** inputs, double* output) {"""
    rv += "\nint inner_loop_ct = 0;\n\n"
    rv += codeacc
    rv += "\n\nsolution_t rv;\nrv.num_iterations = inner_loop_ct;\nreturn rv;\n\n}\n\n"
    rv += "static int variable_size(int* params) {\n"
    rv += "return " + output_size.name + ";\n}\n\n"
    for(i <- 0 until inputdescs.length) {
      for(j <- 0 until inputdescs(i).dims.length) {
        rv += "int structure_input" + i.toString + "_order" + j.toString + "(int* params, int* iidx) {\n"
        val pps = for (k <- 0 until arity) yield new IntSymM("params[" + k.toString + "]")
        val iis = for (k <- 0 until j) yield new IntSymM("iidx[" + k.toString + "]")
        val szs = inputdescs(i).dims(j)(pps ++ iis)
        rv += "return " + szs.name + ";\n}\n\n"
      }
      rv += "matrix_shape_t shape_input" + i.toString + "(int* params, int* iidx) {\n"
      val pps = for (k <- 0 until arity) yield new IntSymM("params[" + k.toString + "]")
      val iis = for (k <- 0 until inputdescs(i).dims.length) yield new IntSymM("iidx[" + k.toString + "]")
      val dm = inputdescs(i).domain(pps ++ iis)
      val cdm = inputdescs(i).codomain(pps ++ iis)
      rv += "matrix_shape_t rv;\n"
      rv += "rv.domain = " + dm.name + ";\n"
      rv += "rv.codomain = " + cdm.name + ";\n"
      rv += "return rv;\n}\n\n"
      rv += "int (*inputdesc_structure" + i.toString + "[])(int* params, int* idxs) = {\n"
      for(j <- 0 until inputdescs(i).dims.length) {
        if(j != 0) rv += ",\n"
        rv += "structure_input" + i.toString + "_order" + j.toString
      }
      rv += "};\n\n"
      rv += "input_desc_t inputdesc" + i.toString + " = {\n"
      rv += ".order = " + inputdescs(i).dims.length.toString + ",\n"
      rv += ".structure = inputdesc_structure" + i.toString + ",\n"
      rv += ".shape = shape_input" + i.toString + "};\n\n"
    }
    rv += "input_desc_t* solver_inputdescs[] = {\n"
    for(i <- 0 until inputdescs.length) {
      if(i != 0) rv += ",\n"
      rv += "&inputdesc" + i.toString
    }
    rv += "};\n\n"
    rv += "solver_t solver = {\n"
    rv += ".num_params = " + arity.toString + ",\n"
    rv += ".num_inputs = " + inputdescs.length + ",\n"
    rv += ".input_descs = solver_inputdescs,\n"
    rv += ".variable_size = variable_size,\n"
    rv += ".solve = solve};\n\n"
    rv
  }

  private var codeacc: String = ""
  private def emit(code: String, repls: Tuple2[String, Sym]*) {
    var accv = code.trim
    for(r <- repls) {
      accv = accv.replace("$" + r._1, r._2.name)
    }
    if(accv.contains("$")) {
     println(accv)
     throw new IRValidationException()
    }
    codeacc += accv + "\n"
  }


  var output_written = false
  var output_size: IntSym = null
  def write_output(o: VectorSym) {
    if(output_written) throw new IRValidationException()
    output_written = true
    output_size = o.size
    val i = nextint
    val po = new FlatSym("output")
    emit("for(int $i = 0; $i < $size; $i++) $po[$i] = $o;",
      "o" -> o.at(i), "size" -> o.size, "i" -> i, "po" -> po)
  }

  def print(x: VectorSym, name: String, fmt: String) {
    val i = nextint
    emit("printf(\"" + name + " = [\");\nfor(int $i = 0; $i < $size; $i++) printf(\"" + fmt + ", \", $x);\nprintf(\"]\\n\");",
      "i" -> i, "x" -> x.at(i), "size" -> x.size)
  }


  lazy val params: Seq[IntSym] = for(i <- 0 until arity) yield {
    // val rv = nextint
    // emit("int $rv = param" + i.toString + ";", "rv" -> rv)
    // rv
    new IntSymM("params[" + i.toString + "]")
  }

  lazy val inputs: Seq[InputSym] = for(i <- 0 until inputdescs.length) yield {
    new InputSym(i)
  }

  object intlikei extends IntLike[IntSym] {
    def add(x: IntSym, y: IntSym): IntSym = {
      if(x.isInstanceOf[IntSymD] && y.isInstanceOf[IntSymD]) {
        new IntSymD(x.asInstanceOf[IntSymD].c + y.asInstanceOf[IntSymD].c)
      }
      else if(x.isInstanceOf[IntSymD] && x.asInstanceOf[IntSymD].c == 0) {
        y
      }
      else if(y.isInstanceOf[IntSymD] && y.asInstanceOf[IntSymD].c == 0) {
        x
      }
      else {
        // val rv = nextint
        // emit("int $rv = $x + $y;", "rv" -> rv, "x" -> x, "y" -> y)
        // rv
        new IntSymM("(" + x.name + " + " + y.name + ")")
      }
    }
    def neg(x: IntSym): IntSym = {
      if(x.isInstanceOf[IntSymD]) {
        new IntSymD(-x.asInstanceOf[IntSymD].c)
      }
      else {
        // val rv = nextint
        // emit("int $rv = -$x;", "rv" -> rv, "x" -> x)
        // rv
        new IntSymM("(-" + x.name + ")")
      }
    }
    def multiply(x: IntSym, y: IntSym): IntSym = {
      if(x.isInstanceOf[IntSymD] && y.isInstanceOf[IntSymD]) {
        new IntSymD(x.asInstanceOf[IntSymD].c * y.asInstanceOf[IntSymD].c)
      }
      else if(x.isInstanceOf[IntSymD] && x.asInstanceOf[IntSymD].c == 0) {
        new IntSymD(0)
      }
      else if(y.isInstanceOf[IntSymD] && y.asInstanceOf[IntSymD].c == 0) {
        new IntSymD(0)
      }
      else if(x.isInstanceOf[IntSymD] && x.asInstanceOf[IntSymD].c == 1) {
        y
      }
      else if(y.isInstanceOf[IntSymD] && y.asInstanceOf[IntSymD].c == 1) {
        x
      }
      else {
        new IntSymM("(" + x.name + " * " + y.name + ")")
        // val rv = nextint
        // emit("int $rv = $x * $y;", "rv" -> rv, "x" -> x, "y" -> y)
        // rv
      }
    }
    def divide(x: IntSym, r: Int): IntSym = {
      if(x.isInstanceOf[IntSymD]) {
        new IntSymD(x.asInstanceOf[IntSymD].c / r)
      }
      else if(r == 1) {
        x
      }
      else {
        new IntSymM("(" + x.name + " / " + r.toString + ")")
        // val rv = nextint
        // emit("int $rv = $x / " + r.toString + ";", "rv" -> rv, "x" -> x)
        // rv
      }
    }
    implicit def int2T(x: Int): IntSym = {
      new IntSymD(x)
      // val rv = nextint
      // emit("int $rv = " + x.toString + ";", "rv" -> rv)
      // rv
    }
  }


  //VECTOR OPERATIONS
  //base objects
  def size(arg: VectorSym): IntSym = arg.size
  def zero(size: IntSym): VectorSym = {
    // val rv = nextvector(size)
    // val i = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $size; $i++) $rv[$i] = 0.0;
    //   """, 
    //   "rv" -> rv, "i" -> i, "size" -> size)
    // rv
    new VectorSymFlat("0.0", size)
  } 
  def one: VectorSym = {
    // val rv = nextvector(intlikei.int2T(1))
    // emit("double $rv[1] = {1.0};", "rv" -> rv)
    // rv
    new VectorSymFlat("1.0", intlikei.int2T(1))
  }
  //linear operators
  def sum(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    // val rv = nextvector(arg1.size)
    // val i = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $size; $i++) $rv[$i] = $x + $y;
    //   """,
    //   "rv" -> rv, "i" -> i, "x" -> arg1.at(i), "y" -> arg2.at(i), "size" -> arg1.size)
    // rv
    new VectorSymFlat("(" + arg1.at(new IntSymM("$")).name + " + " + arg2.at(new IntSymM("$")).name + ")", arg1.size)
  }
  def sumfor(len: IntSym, size: IntSym, arg: (IntSym => VectorSym)): VectorSym = {
    val rv = nextvector(size)
    val i = nextint
    val j = nextint
    val k = nextint
    emit("""
      double $rv[$size];
      for(int $j = 0; $j < $size; $j++) $rv[$j] = 0.0;
      for(int $i = 0; $i < $len; $i++) {
      """,
      "rv" -> rv, "size" -> size, "len" -> len, "i" -> i, "j" -> j)
    val va = arg(i)
    emit("""
      for(int $k = 0; $k < $size; $k++) $rv[$k] += $va;
      }
      """,
      "rv" -> rv, "va" -> va.at(k), "size" -> size, "len" -> len, "k" -> k)
    return rv
  }
  def neg(arg: VectorSym): VectorSym = {
    // val rv = nextvector(arg.size)
    // val i = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $size; $i++) $rv[$i] = -$x;
    //   """,
    //   "rv" -> rv, "i" -> i, "x" -> arg.at(i), "size" -> arg.size)
    // rv
    new VectorSymFlat("(-" + arg.at(new IntSymM("$")).name + ")", arg.size)
  }
  def scaleconstant(arg: VectorSym, scale: Double): VectorSym = {
    // val rv = nextvector(arg.size)
    // val i = nextint
    // emit("double $rv[$size];\nfor(int $i = 0; $i < $size; $i++) $rv[$i] = $x * " + scale.toString + ";",
    //   "rv" -> rv, "i" -> i, "x" -> arg.at(i), "size" -> arg.size)
    // rv
    new VectorSymFlat("(" + arg.at(new IntSymM("$")).name + " * " + scale.toString + ")", arg.size)
  }
  def cat(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    // val irv = intlikei.add(arg1.size, arg2.size)
    // val rv = nextvector(irv)
    // val i = nextint
    // val j = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $xsize; $i++) $rv[$i] = $x;
    //   for(int $j = 0; $j < $ysize; $j++) $rv[$j + $xsize] = $y;
    //   """,
    //   "rv" -> rv, "size" -> irv, "xsize" -> arg1.size, "ysize" -> arg2.size, "x" -> arg1.at(i), "y" -> arg2.at(j), "i" -> i, "j" -> j)
    // rv
    new VectorSymFlat(
      "(($ < " + arg1.size.name + ") ? " + arg1.at(new IntSymM("$")).name + " : " + arg2.at(new IntSymM("($ - " + arg1.size.name + ")")).name + ")", 
      intlikei.add(arg1.size, arg2.size))
  }
  def catfor(len: IntSym, size: IntSym, arg: (IntSym => VectorSym)): VectorSym = {
    val rv = nextvector(size)
    val i = nextint
    val j = nextint
    val k = nextint
    emit("""
      double $rv[$size];
      int $k = 0;
      for(int $i = 0; $i < $len; $i++) {
      """,
      "rv" -> rv, "len" -> len, "i" -> i, "j" -> j, "k" -> k, "size" -> size)
    val va = arg(i)
    emit("""
      for(int $j = 0; $j < $vasize; $j++, $k++) $rv[$k] = $va;
      }
      """,
      "rv" -> rv, "len" -> len, "vasize" -> va.size, "va" -> va.at(j), "i" -> i, "j" -> j, "k" -> k, "size" -> size)
    rv
  }
  def slice(arg: VectorSym, at: IntSym, size: IntSym): VectorSym = {
    // val rv = nextvector(size)
    // val i = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $size; $i++) $rv[$i] = $x;
    //   """,
    //   "rv" -> rv, "x" -> arg.at(intlikei.add(i, at)), "at" -> at, "size" -> size, "i" -> i)
    // rv
    new VectorSymFlat("(" + arg.at(new IntSymM("($ + " + at.name + ")")).name + ")", size)
  }
  //nonlinear operators
  def dot(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    val rvsize = intlikei.int2T(1)
    val rv = nextvector(rvsize)
    val i = nextint
    emit("""
      double $rv[1] = {0.0};
      for(int $i = 0; $i < $size; $i++) $rv[0] += $x * $y;
      """,
      "rv" -> rv, "i" -> i, "size" -> arg1.size, "x" -> arg1.at(i), "y" -> arg2.at(i))
    rv
  }
  def mpy(arg: VectorSym, scale: VectorSym): VectorSym = {
    // val rv = nextvector(arg.size)
    // val i = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $size; $i++) $rv[$i] = $x * $a;
    //   """,
    //   "rv" -> rv, "size" -> arg.size, "i" -> i, "x" -> arg.at(i), "a" -> scale.at(intlikei.int2T(0)))
    // rv
    new VectorSymFlat("(" + arg.at(new IntSymM("$")).name + " * " + scale.at(intlikei.int2T(0)).name + ")", arg.size)
  }
  def div(arg: VectorSym, scale: VectorSym): VectorSym = {
    // val rv = nextvector(arg.size)
    // val i = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $size; $i++) $rv[$i] = $x / $a;
    //   """,
    //   "rv" -> rv, "size" -> arg.size, "i" -> i, "x" -> arg.at(i), "a" -> scale.at(intlikei.int2T(0)))
    // rv
    new VectorSymFlat("(" + arg.at(new IntSymM("$")).name + " / " + scale.at(intlikei.int2T(0)).name + ")", arg.size)
  }
  def norm2(arg: VectorSym): VectorSym = dot(arg, arg)
  def sqrt(arg: VectorSym): VectorSym = {
    val rvsize = intlikei.int2T(1)
    val rv = nextvector(rvsize)
    emit("double $rv[1] = { sqrt($x) };", "rv" -> rv, "x" -> arg.at(intlikei.int2T(0)))
    rv
  }
  def max(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    // val rv = nextvector(arg1.size)
    // val i = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $size; $i++) $rv[$i] = ($x > $y) ? $x : $y;
    //   """,
    //   "rv" -> rv, "i" -> i, "size" -> arg1.size, "x" -> arg1.at(i), "y" -> arg2.at(i))
    // rv
    new VectorSymFlat("fmax(" + arg1.at(new IntSymM("$")).name + ", " + arg2.at(new IntSymM("$")).name + ")", arg1.size)
  }
  def min(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    // val rv = nextvector(arg1.size)
    // val i = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $size; $i++) $rv[$i] = ($x < $y) ? $x : $y;
    //   """,
    //   "rv" -> rv, "i" -> i, "size" -> arg1.size, "x" -> arg1.at(i), "y" -> arg2.at(i))
    // rv
    new VectorSymFlat("fmin(" + arg1.at(new IntSymM("$")).name + ", " + arg2.at(new IntSymM("$")).name + ")", arg1.size)
  }
  def norm_inf(arg: VectorSym): VectorSym = {
    val rvsize = intlikei.int2T(1)
    val rv = nextvector(rvsize)
    val i = nextint
    emit("""
      double $rv[1] = {0.0};
      for(int $i = 0; $i < $size; $i++) if (fabs($x) > $rv[0]) $rv[0] = fabs($x);
      """,
      "rv" -> rv, "x" -> arg.at(i), "size" -> arg.size, "i" -> i)
    rv
  }

  def matrixmpy(m: MatrixSym, osize: IntSym, x: VectorSym): VectorSym = {
    val rv = nextvector(osize)
    val i = nextint
    val j = nextint
    emit("""
      double $rv[$osize];
      for(int $i = 0; $i < $osize; $i++) {
        $rv[$i] = 0.0;
        for(int $j = 0; $j < $isize; $j++) {
          $rv[$i] += $m[$i*$isize+$j] * $x;
        }
      }
      """,
      "m" -> m, "rv" -> rv, "isize" -> x.size, "osize" -> osize, "i" -> i, "j" -> j, "x" -> x.at(j))
    rv
  }
  def matrixmpytranspose(m: MatrixSym, osize: IntSym, x: VectorSym): VectorSym = {
    val rv = nextvector(osize)
    val i = nextint
    val j = nextint
    emit("""
      double $rv[$osize];
      for(int $i = 0; $i < $osize; $i++) {
        $rv[$i] = 0.0;
        for(int $j = 0; $j < $isize; $j++) {
          $rv[$i] += $m[$j*$osize+$i] * $x;
        }
      }
      """,
      "m" -> m, "rv" -> rv, "isize" -> x.size, "osize" -> osize, "i" -> i, "j" -> j, "x" -> x.at(j))
    rv
  }

  def matrixget(mats: InputSym, at: Seq[IntSym]): MatrixSym = {
    val rv = nextmatrix
    var emitstr: String = "double* " + rv.name + " = " + mats.name
    for(a <- at) {
      emitstr += "->idx[" + a.name + "]"
    }
    emitstr += "->mat;"
    emit(emitstr)
    rv
  }

  def vectorget(vecs: MemorySym, size: IntSym, at: Seq[IntSym]): VectorSym = {
    val rv = nextvector(size)
    var emitstr: String = "double* " + rv.name + " = " + vecs.name
    for(i <- at) {
      emitstr += "->idx[" + i.name + "]"
    }
    emitstr += "->vec;"
    emit(emitstr)
    rv
  }

  def vectorset(src: VectorSym, vecs: MemorySym, at: Seq[IntSym]): MemorySym = {
    val j = nextint
    val i = nextint
    val v = nextvector(src.size)
    var emitstr: String = "double " + v.name + "[" + src.size.name + "];\n"
    emitstr += "for(int " + j.name + " = 0; " + j.name + " < " + src.size.name + "; " + j.name + "++) "
    emitstr += v.name + "[" + j.name + "] = " + src.at(j).name + ";\n"
    emitstr += "for(int " + i.name + " = 0; " + i.name + " < " + src.size.name + "; " + i.name + "++) "
    emitstr += vecs.name
    for(i <- at) {
      emitstr += "->idx[" + i.name + "]"
    }
    emitstr += "->vec[" + i.name + "] = " + v.at(i).name + ";\n"
    emit(emitstr)
    vecs.copyinvalidate
  }

  def memoryallocfor(dim: IntSym, ar: Int, body: IntSym => MemorySym): MemorySym = {
    val rv = nextmemory
    val i = nextint
    emit("""
      memory_t* $rv = alloca($dim * sizeof(memory_t*)));
      for(int $i = 0; $i < $dim; $i++) {
      """,
      "rv" -> rv, "i" -> i, "dim" -> dim)
    val x = body(i)
    emit("""
      $rv->idx[$i] = $x;
      }
      """,
      "rv" -> rv, "i" -> i, "dim" -> dim, "x" -> x)
    rv
  }
  def memoryalloc(size: IntSym): MemorySym = {
    val rv = nextmemory
    emit("memory_t* $rv = alloca($size * sizeof(double));", "rv" -> rv, "size" -> size)
    rv
  }

  var isconverge: Boolean = false
  def converge(memory: Seq[MemorySym], itermax: Int, body: (Seq[MemorySym]) => (Seq[MemorySym], VectorSym)): Seq[MemorySym] = {
    val ict = nextint
    if(itermax > 0) {
      emit("int $ict = 0;", "ict" -> ict)
    }
    emit("while(1) {")
    isconverge = false
    val (newmem, cond) = body(memory)
    if(newmem.length != memory.length) throw new IRValidationException()
    for(i <- 0 until newmem.length) {
      if(newmem(i).i != memory(i).i) throw new IRValidationException()
    }
    if(isconverge == false) {
      //this is the innermost convergence loop
      emit("inner_loop_ct++;")
    }
    if(itermax > 0) {
      emit("$ict++;\nif($ict >= " + itermax.toString + ") break;", "ict" -> ict)
    }
    emit("if($cond <= 0.0) break;\n}", "cond" -> cond.at(intlikei.int2T(0)))
    isconverge = true
    newmem
  }

  def runfor(len: IntSym, memory: Seq[MemorySym], body: (IntSym, Seq[MemorySym]) => Seq[MemorySym]): Seq[MemorySym] = {
    val i = nextint
    emit("for(int $i = 0; $i < $len; $i++) {", "i" -> i, "len" -> len)
    val newmem = body(i, memory)
    if(newmem.length != memory.length) throw new IRValidationException()
    for(i <- 0 until newmem.length) {
      if(newmem(i).i != memory(i).i) throw new IRValidationException()
    }
    emit("}")
    newmem
  }
  
}