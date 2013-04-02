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
class VectorSym(val i: Int, val size: IntSym) extends Sym {
  def name: String = "x" + i.toString
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
  def name: String = "b" + i.toString
}

class SolverRuntimeCGen(val arity: Int, val ninputs: Int) extends SolverRuntime[IntSym, MatrixSym, InputSym, VectorSym, MemorySym] {
  private var intsym_ni: Int = 0
  private def nextint: IntSym = {
    intsym_ni += 1
    new IntSymL(intsym_ni)
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
    rv += "solver_t solver = {\n"
    rv += ".num_params = " + arity.toString + ",\n"
    rv += ".num_inputs = 0,\n"
    rv += ".input_descs = NULL,\n"
    rv += ".variable_size = variable_size,\n"
    rv += ".solve = solve};\n\n"
    rv
  }

/*
  def hcode: String = {
    var rv = """

typedef union input_t {
  union input_t* idx[0];
  double mat[0];
} input_t;

int solve("""
    for(i <- 0 until arity) {
      if(i != 0) rv += ", "
      rv += "int param" + i.toString
    }
    for(i <- 0 until ninputs) {
      rv += ", input_t* input" + i.toString
    }
    for(i <- 0 until num_outputs) {
      rv += ", double** output" + i.toString
    }
    rv += ");\n\n"
    rv
  }

  def maincode: String = {
    var rv = """
#include <stdlib.h>
#include <stdio.h>
#include "out.h"

int main(int argc, char** argv) {
"""
    rv += "if (argc != " + arity.toString + " + 1) {\nprintf(\"error: expected " + arity.toString + " arguments.\\n\");\nreturn -1;\n}\n\n"
    for(i <- 0 until arity) {
      rv += "int param" + i.toString + " = atoi(argv[" + (i+1).toString + "]);\n"
    }
    rv += "\n"
    rv += "int iteration_ct = solve("
    for(i <- 0 until arity) {
      if(i != 0) rv += ", "
      rv += "param" + i.toString
    }
    for(i <- 0 until ninputs) {
      rv += ", NULL"
    }
    for(i <- 0 until num_outputs) {
      rv += ", NULL"
    }
    rv += ");\n\n"
    rv += "printf(\"converged in %d iterations\\n\", iteration_ct);\n\n"
    rv += "return 0;\n\n}\n\n"
    rv
  }
*/

  private var codeacc: String = ""
  private def emit(code: String, repls: Tuple2[String, Sym]*) {
    var accv = code.trim
    for(r <- repls) {
      accv = accv.replace("%" + r._1, r._2.name)
    }
    //if(accv.contains("%")) {
    //  println(accv)
    //  throw new IRValidationException()
    //}
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
    emit("for(int %i = 0; %i < %size; %i++) %po[%i] = %o[%i];",
      "o" -> o, "size" -> o.size, "i" -> i, "po" -> po)
  }

  def print(x: VectorSym, name: String, fmt: String) {
    val i = nextint
    emit("printf(\"" + name + " = [\");\nfor(int %i = 0; %i < %size; %i++) printf(\"" + fmt + ", \", %x[%i]);\nprintf(\"]\\n\");",
      "i" -> i, "x" -> x, "size" -> x.size)
  }


  val params: Seq[IntSym] = for(i <- 0 until arity) yield {
    // val rv = nextint
    // emit("int %rv = param" + i.toString + ";", "rv" -> rv)
    // rv
    new IntSymM("params[" + i.toString + "]")
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
        // emit("int %rv = %x + %y;", "rv" -> rv, "x" -> x, "y" -> y)
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
        // emit("int %rv = -%x;", "rv" -> rv, "x" -> x)
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
        // emit("int %rv = %x * %y;", "rv" -> rv, "x" -> x, "y" -> y)
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
        // emit("int %rv = %x / " + r.toString + ";", "rv" -> rv, "x" -> x)
        // rv
      }
    }
    implicit def int2T(x: Int): IntSym = {
      new IntSymD(x)
      // val rv = nextint
      // emit("int %rv = " + x.toString + ";", "rv" -> rv)
      // rv
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
    val k = nextint
    emit("""
      double %rv[%size];
      for(int %j = 0; %j < %size; %j++) %rv[%j] = 0.0;
      for(int %i = 0; %i < %len; %i++) {
      """,
      "rv" -> rv, "size" -> size, "len" -> len, "i" -> i, "j" -> j)
    val va = arg(i)
    emit("""
      for(int %k = 0; %k < %size; %k++) %rv[%k] += %va[%k];
      }
      """,
      "rv" -> rv, "va" -> va, "size" -> size, "len" -> len, "k" -> k)
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
      "rv" -> rv, "size" -> irv, "xsize" -> arg1.size, "ysize" -> arg2.size, "x" -> arg1, "y" -> arg2, "i" -> i, "j" -> j)
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
      "rv" -> rv, "len" -> len, "vasize" -> va.size, "va" -> va, "i" -> i, "j" -> j, "k" -> k, "size" -> size)
    rv
  }
  def slice(arg: VectorSym, at: IntSym, size: IntSym): VectorSym = {
    val rv = nextvector(size)
    val i = nextint
    emit("""
      double %rv[%size];
      for(int %i = 0; %i < %size; %i++) %rv[%i] = %x[%i + %at];
      """,
      "rv" -> rv, "x" -> arg, "at" -> at, "size" -> size, "i" -> i)
    rv
  }
  //nonlinear operators
  def dot(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    val rvsize = intlikei.int2T(1)
    val rv = nextvector(rvsize)
    val i = nextint
    emit("""
      double %rv[1] = {0.0};
      for(int %i = 0; %i < %size; %i++) %rv[0] += %x[%i] * %y[%i];
      """,
      "rv" -> rv, "i" -> i, "size" -> arg1.size, "x" -> arg1, "y" -> arg2)
    rv
  }
  def mpy(arg: VectorSym, scale: VectorSym): VectorSym = {
    val rv = nextvector(arg.size)
    val i = nextint
    emit("""
      double %rv[%size];
      for(int %i = 0; %i < %size; %i++) %rv[%i] = %x[%i] * %a[0];
      """,
      "rv" -> rv, "size" -> arg.size, "i" -> i, "x" -> arg, "a" -> scale)
    rv
  }
  def div(arg: VectorSym, scale: VectorSym): VectorSym = {
    val rv = nextvector(arg.size)
    val i = nextint
    emit("""
      double %rv[%size];
      for(int %i = 0; %i < %size; %i++) %rv[%i] = %x[%i] / %a[0];
      """,
      "rv" -> rv, "size" -> arg.size, "i" -> i, "x" -> arg, "a" -> scale)
    rv
  }
  def norm2(arg: VectorSym): VectorSym = dot(arg, arg)
  def sqrt(arg: VectorSym): VectorSym = {
    val rvsize = intlikei.int2T(1)
    val rv = nextvector(rvsize)
    emit("double %rv[1] = { sqrt(%x[0]) };", "rv" -> rv, "x" -> arg)
    rv
  }
  def max(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    val rv = nextvector(arg1.size)
    val i = nextint
    emit("""
      double %rv[%size];
      for(int %i = 0; %i < %size; %i++) %rv[%i] = (%x[%i] > %y[%i]) ? %x[%i] : %y[%i];
      """,
      "rv" -> rv, "i" -> i, "size" -> arg1.size, "x" -> arg1, "y" -> arg2)
    rv
  }
  def min(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    val rv = nextvector(arg1.size)
    val i = nextint
    emit("""
      double %rv[%size];
      for(int %i = 0; %i < %size; %i++) %rv[%i] = (%x[%i] < %y[%i]) ? %x[%i] : %y[%i];
      """,
      "rv" -> rv, "i" -> i, "size" -> arg1.size, "x" -> arg1, "y" -> arg2)
    rv
  }
  def norm_inf(arg: VectorSym): VectorSym = {
    val rvsize = intlikei.int2T(1)
    val rv = nextvector(rvsize)
    val i = nextint
    emit("""
      double %rv[1] = {0.0};
      for(int %i = 0; %i < %size; %i++) if (fabs(%x[%i]) > %rv[0]) %rv[0] = fabs(%x[%i]);
      """,
      "rv" -> rv, "x" -> arg, "size" -> arg.size, "i" -> i)
    rv
  }

  def matrixmpy(m: MatrixSym, x: VectorSym): VectorSym = {
    throw new Exception("Matrix inputs not yet supported")
  }
  def matrixmpytranspose(m: MatrixSym, x: VectorSym): VectorSym = {
    throw new Exception("Matrix inputs not yet supported")
  }

  def matrixget(mats: InputSym, at: Seq[IntSym]): MatrixSym = {
    throw new Exception("Matrix inputs not yet supported")
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
    var emitstr: String = "memcpy(" + vecs.name
    for(i <- at) {
      emitstr += "->idx[" + i.name + "]"
    }
    emitstr += "->vec, " + src.name + ", " + src.size.name + " * sizeof(double));"
    emit(emitstr)
    vecs.copyinvalidate
  }

  def memoryallocfor(dim: IntSym, ar: Int, body: IntSym => MemorySym): MemorySym = {
    val rv = nextmemory
    val i = nextint
    emit("""
      memory_t* %rv = alloca(%dim * sizeof(memory_t*)));
      for(int %i = 0; %i < %dim; %i++) {
      """,
      "rv" -> rv, "i" -> i, "dim" -> dim)
    val x = body(i)
    emit("""
      %rv->idx[%i] = %x;
      }
      """,
      "rv" -> rv, "i" -> i, "dim" -> dim, "x" -> x)
    rv
  }
  def memoryalloc(size: IntSym): MemorySym = {
    val rv = nextmemory
    emit("memory_t* %rv = alloca(%size * sizeof(double));", "rv" -> rv, "size" -> size)
    rv
  }

  var isconverge: Boolean = false
  def converge(memory: Seq[MemorySym], itermax: Int, body: (Seq[MemorySym]) => (Seq[MemorySym], VectorSym)): Seq[MemorySym] = {
    val ict = nextint
    if(itermax > 0) {
      emit("int %ict = 0;", "ict" -> ict)
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
      emit("%ict++;\nif(%ict >= " + itermax.toString + ") break;", "ict" -> ict)
    }
    emit("if(%cond[0] <= 0.0) break;\n}", "cond" -> cond)
    isconverge = true
    newmem
  }

  def runfor(len: IntSym, memory: Seq[MemorySym], body: (IntSym, Seq[MemorySym]) => Seq[MemorySym]): Seq[MemorySym] = {
    val i = nextint
    emit("for(int %i = 0; %i < %len; %i++) {", "i" -> i, "len" -> len)
    val newmem = body(i, memory)
    if(newmem.length != memory.length) throw new IRValidationException()
    for(i <- 0 until newmem.length) {
      if(newmem(i).i != memory(i).i) throw new IRValidationException()
    }
    emit("}")
    newmem
  }
  
}