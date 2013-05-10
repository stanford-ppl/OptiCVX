package ppl.dsl.opticvx.runtime.cgen

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import ppl.dsl.opticvx.solvers._

import java.io._
import java.lang.Runtime



/*
case class DstOp(val dstscale: String, val srcscale: String) {
  def sub(dst: Sym, src: Sym) = {
    if(dstscale == "0.0") {
      if(srcscale == "1.0") {
        dst.name + " = " + src.name
      }
      else if(srcscale == "-1.0") {
        dst.name + " = -" + src.name
      }
      else {
        dst.name + " = " + src.name + " * " + srcscale
      }
    }
    else if(dstscale == "1.0") {
      if(srcscale == "1.0") {
        dst.name + " += " + src.name
      }
      else if(srcscale == "-1.0") {
        dst.name + " -= " + src.name
      }
      else {
        dst.name + " += " + src.name + " * " + srcscale
      }
    }
    else {
      dst.name + " = " + dst.name + " * " + dstscale + " + " + src.name + " * " + srcscale
    }
  }
}
*/

object SolverRuntimeCGen extends SolverRuntime {
  def compile(vix: AVector): SolverCompiled = {
    val v = vix.simplify
    val analysis = new SolverAnalysisCGen(null, null)
    analysis.analyze(v).addNeedWrite
    val params: Seq[IntSym] = for(i <- 0 until v.arity) yield IntSymD("param" + i.toString)
    val compiler = new SolverCompilerCGen(params, Seq(), analysis, null, null)
    val result = compiler.eval(v)
    compiler.emit(result.writeTo(new DstWritableArray("output")))
    var code = """
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
static solution_t solve(const int* const params, const input_t* const* const inputs, double* const output, double tolerance) {"""
    code += "\nint inner_loop_ct = 0;\n\n"
    for(i <- 0 until v.arity) {
      code += "const int param" + i.toString + " = params[" + i.toString + "];\n"
    }
    for(i <- 0 until v.input.args.length) {
      code += "const input_t* const input" + i.toString + " = inputs[" + i.toString + "];\n"
    }
    code += "\n"
    code += compiler.codeacc
    //code += "\nfor(int i = 0; i < " + v.size.eval(params)(IntLikeIntSym).name + "; i++) output[i] = " + result.at(IntSymD("i")).name + ";\n"
    code += "\nsolution_t rv;\nrv.num_iterations = inner_loop_ct;\nreturn rv;\n\n}\n\n"
    
    code += "static int variable_size(int* params) {\n"
    for(i <- 0 until v.arity) {
      code += "int param" + i.toString + " = params[" + i.toString + "];\n"
    }
    code += "return " + v.size.eval(params)(IntLikeIntSym).name + ";\n}\n\n"
    for(i <- 0 until v.input.args.length) {
      for(j <- 0 until v.input.args(i).dims.length) {
        code += "static int structure_input" + i.toString + "_order" + j.toString + "(int* params, int* iidx) {\n"
        for(i <- 0 until v.arity) {
          code += "int param" + i.toString + " = params[" + i.toString + "];\n"
        }
        val pps = for (k <- 0 until v.arity) yield (IntSymD("params[" + k.toString + "]"): IntSym)
        val iis = for (k <- 0 until j) yield (IntSymD("iidx[" + k.toString + "]"): IntSym)
        val szs = v.input.args(i).dims(j).eval(pps ++ iis)(IntLikeIntSym)
        code += "return " + szs.name + ";\n}\n\n"
      }
      code += "static matrix_shape_t shape_input" + i.toString + "(int* params, int* iidx) {\n"
      for(i <- 0 until v.arity) {
        code += "int param" + i.toString + " = params[" + i.toString + "];\n"
      }
      val pps = for (k <- 0 until v.arity) yield (IntSymD("params[" + k.toString + "]"): IntSym)
      val iis = for (k <- 0 until v.input.args(i).dims.length) yield (IntSymD("iidx[" + k.toString + "]"): IntSym)
      val dm = v.input.args(i).body.domain.eval(pps ++ iis)(IntLikeIntSym)
      val cdm = v.input.args(i).body.codomain.eval(pps ++ iis)(IntLikeIntSym)
      code += "matrix_shape_t rv;\n"
      code += "rv.domain = " + dm.name + ";\n"
      code += "rv.codomain = " + cdm.name + ";\n"
      code += "return rv;\n}\n\n"
      code += "int (*inputdesc_structure" + i.toString + "[])(int* params, int* idxs) = {\n"
      for(j <- 0 until v.input.args(i).dims.length) {
        if(j != 0) code += ",\n"
        code += "structure_input" + i.toString + "_order" + j.toString
      }
      code += "};\n\n"
      code += "input_desc_t inputdesc" + i.toString + " = {\n"
      code += ".order = " + v.input.args(i).dims.length.toString + ",\n"
      code += ".structure = inputdesc_structure" + i.toString + ",\n"
      code += ".shape = shape_input" + i.toString + "};\n\n"
    }
    code += "input_desc_t* solver_inputdescs[] = {\n"
    for(i <- 0 until v.input.args.length) {
      if(i != 0) code += ",\n"
      code += "&inputdesc" + i.toString
    }
    code += "};\n\n"
    code += "solver_t solver = {\n"
    code += ".num_params = " + v.arity.toString + ",\n"
    code += ".num_inputs = " + v.input.args.length + ",\n"
    code += ".input_descs = solver_inputdescs,\n"
    code += ".variable_size = variable_size,\n"
    code += ".solve = solve};\n\n"

    val lrx = code.split("\n")
    code = ""
    var indentlvl = 0
    for(l <- lrx) {
      if((l.contains("}"))&&(!l.contains("{"))) indentlvl -= 1
      code += ("  " * indentlvl) + l.trim + "\n"
      if((l.contains("{"))&&(!l.contains("}"))) indentlvl += 1
    }
    
    val fout = new File("cgen/gen/out.c")
    val fwriter = new FileWriter(fout)
    fwriter.write(code)
    fwriter.close()
    val cmd = "gcc --std=gnu99 -O3 -ffast-math -fassociative-math -ftree-vectorizer-verbose=2 -msse3 -o bin/cgen.out gen/out.c src/main.c -lm >log/gcc.o 2>log/gcc.e"
    println("[exec] " + cmd)
    val rt = java.lang.Runtime.getRuntime()
    val cproc = rt.exec(
      Array[String]("bash", "-c", cmd),
      null,
      new File("cgen"))
    val gccrv = cproc.waitFor()
    if(gccrv != 0) throw new Exception("Error in compiling generated source.")

    new SolverCompiledCGen(v.input, v.size)
  }
}

class SolverCompiledCGen(val inputdesc: InputDesc, val vvsize: IRPoly) extends SolverCompiled {
  def eval(params: Seq[Int], inputs: Seq[MultiSeq[DMatrix]], memory: Seq[Seq[Double]], tolerance: Double): SolverResult = {
    val pp = Seq(params:_*)
    if(pp.length != inputdesc.arity) throw new IRValidationException()
    if(inputs.length != inputdesc.args.length) throw new IRValidationException()
    val fin = new File("cgen/data/in.dat")
    val fwriter = new FileWriter(fin)
    for(a <- inputs) {
      for(l <- a.linearize) {
        fwriter.write(l.formatc + "\n")
      }
    }
    fwriter.close()
    var cmd: String = "bin/cgen.out " + tolerance.toString
    for(i <- 0 until pp.length) {
      cmd += " " + pp(i).toString
    }
    cmd += " <data/in.dat >data/out.dat 2>log/cgen.e"
    val rt = java.lang.Runtime.getRuntime()
    println("[exec] " + cmd)
    val cproc = rt.exec(
      Array[String]("bash", "-c", cmd),
      null,
      new File("cgen"))
    val solverrv = cproc.waitFor()
    if(solverrv != 0) throw new Exception("Error in running generated program.")
    val vvsz = vvsize.eval(pp)(IntLikeInt)
    val fout = new File("cgen/data/out.dat")
    val freader = new FileReader(fout)
    val bfreader = new BufferedReader(freader)
    val vv = for(i <- 0 until vvsz) yield bfreader.readLine().toDouble
    val Array(iterlabel: String, siterct: String) = bfreader.readLine().split(" ")
    if(iterlabel != "iterations:") throw new Exception("Expected iteration count label.")
    val iterct: Int = siterct.toInt
    val Array(timelabel: String, stimer: String) = bfreader.readLine().split(" ")
    if(timelabel != "time:") throw new Exception("Expected time clock label.")
    val timer: Double = stimer.toDouble
    bfreader.close()
    freader.close()
    SolverResult(vv, iterct, timer)
  }
}

trait SolverAnalysisEntry {
  // another op needs this, but in write mode
  def addNeedWrite: Unit
  // another op needs this and it needs it indexed
  def addNeedIndex: Unit
  // an op inside a loop needs this in write mode
  def addNeedWriteLoop: Unit
  // an op inside a loop needs this indexed
  def addNeedIndexLoop: Unit
}
class SolverAnalysisEntryInfo extends SolverAnalysisEntry {
  private var writesNeeded: Int = 0
  private var indexesNeeded: Int = 0
  def addNeedWrite {
    writesNeeded += 1
  }
  def addNeedIndex {
    indexesNeeded += 1
  }
  def addNeedWriteLoop {
    writesNeeded += 100 //a large number
  }
  def addNeedIndexLoop {
    indexesNeeded += 100 //a large number
  }
  def hint: SolverAnalysisHint = {
    if((writesNeeded <= 3)&&(indexesNeeded == 0)) {
      SolverAnalysisHintWritable()
    }
    else if (writesNeeded + indexesNeeded <= 3) {
      SolverAnalysisHintIndexable()
    }
    else {
      SolverAnalysisHintArray()
    }
  }
}
class SolverAnalysisEntryLoop(parent: SolverAnalysisEntry) extends SolverAnalysisEntry {
  def addNeedWrite {
    parent.addNeedWriteLoop
  }
  def addNeedIndex {
    parent.addNeedIndexLoop
  }
  def addNeedWriteLoop {
    parent.addNeedWriteLoop
  }
  def addNeedIndexLoop {
    parent.addNeedIndexLoop
  }
}
trait SolverAnalysisHint
case class SolverAnalysisHintWritable() extends SolverAnalysisHint
case class SolverAnalysisHintIndexable() extends SolverAnalysisHint
case class SolverAnalysisHintArray() extends SolverAnalysisHint

class SolverAnalysisCGen(inherit_promote: SolverAnalysisCGen, inherit_push: SolverAnalysisCGen) {
  val infocache = new scala.collection.mutable.HashMap[AVector, SolverAnalysisEntryInfo]
  val promoteSubs = new scala.collection.mutable.HashMap[AVector, SolverAnalysisCGen]
  val pushSubs = new scala.collection.mutable.HashMap[AVector, SolverAnalysisCGen]

  def analyze(v: AVector): SolverAnalysisEntry = {
    if(infocache.contains(v)) return infocache(v)

    if(inherit_promote != null) {
      if(v.invariantAt(v.arity - 1)) {
        val dv = v.demote
        return new SolverAnalysisEntryLoop(inherit_promote.analyze(dv))
      }
    }
    else if(inherit_push != null) {
      if(v.memoryInvariantAt(v.input.memory.length - 1)) {
        val dv = v.popMemory
        return new SolverAnalysisEntryLoop(inherit_push.analyze(dv))
      }
    }

    v match {
      case AVectorZero(input, size) => 
      case AVectorConst(input, data) => 
      case AVectorSum(args) => {
        for(a <- args) {
          analyze(a).addNeedWrite
        }
      }
      case AVectorNeg(arg) => {
        analyze(arg).addNeedWrite
      }
      case AVectorScaleConstant(arg, scale) => {
        analyze(arg).addNeedWrite
      }
      case AVectorCat(args) => {
        for(a <- args) {
          analyze(a).addNeedWrite
        }
      }
      case AVectorCatFor(len, arg) => {
        val ev = new SolverAnalysisCGen(this, null)
        ev.analyze(arg).addNeedWrite
        promoteSubs += (v -> ev)
      }
      case AVectorSlice(arg, at, size) => {
        analyze(arg).addNeedIndex
      }
      case AVectorSumFor(len, arg) => {
        val ev = new SolverAnalysisCGen(this, null)
        ev.analyze(arg).addNeedWrite
        promoteSubs += (v -> ev)
      }
      case AVectorMpyInput(arg, iidx, sidx) => {
        if(arg.size == IRPoly.const(1, arg.arity)) {
          analyze(arg).addNeedIndex
        }
        else {
          analyze(arg).addNeedIndexLoop
        }
      }
      case AVectorMpyInputT(arg, iidx, sidx) => {
        if(arg.size == IRPoly.const(1, arg.arity)) {
          analyze(arg).addNeedIndex
        }
        else {
          analyze(arg).addNeedIndexLoop
        }
      }
      case AVectorRead(input, iidx) => 
      case AVectorDot(arg1, arg2) => {
        analyze(arg1).addNeedIndex
        analyze(arg2).addNeedIndex
      }
      case AVectorMpy(arg, scale) => {
        analyze(arg).addNeedWrite
        analyze(scale).addNeedIndexLoop
      }
      case AVectorDiv(arg, scale) => {
        analyze(arg).addNeedWrite
        analyze(scale).addNeedIndexLoop
      }
      case AVectorSqrt(arg) => {
        analyze(arg).addNeedIndex
      }
      case AVectorNorm2(arg) => {
        analyze(arg).addNeedIndex
      }
      case AVectorNormInf(arg) => {
        analyze(arg).addNeedIndex
      }
      case AVectorMax(arg1, arg2) => {
        analyze(arg1).addNeedIndex
        analyze(arg2).addNeedIndex
      }
      case AVectorMin(arg1, arg2) => {
        analyze(arg1).addNeedIndex
        analyze(arg2).addNeedIndex
      }
      case AVectorTolerance(input) => 
      case AVectorConverge(arg, cond, body, itermax) => {
        analyze(arg).addNeedWrite
        val ev = new SolverAnalysisCGen(null, this)
        ev.analyze(body).addNeedWrite
        ev.analyze(cond).addNeedIndex
        pushSubs += (v -> ev)
      }
      case _ =>
        throw new IRValidationException()
    }

    infocache += (v -> new SolverAnalysisEntryInfo())
    return infocache(v)
  }
}

class SolverCompilerCGen(params: Seq[IntSym], memory: Seq[VectorSym], analysis: SolverAnalysisCGen, inherit_promote: SolverCompilerCGen, inherit_push: SolverCompilerCGen) {
  val memocache = new scala.collection.mutable.HashMap[AVector, VectorSym]()

  def memolookup(v: AVector): VectorSym = {
    val rv = memocache.getOrElse(v, null)
    if(rv == null) {
      if(inherit_promote != null) {
        if(v.invariantAt(v.arity - 1)) {
          val dv = v.demote
          inherit_promote.eval(dv)
        }
        else {
          null
        }
      }
      else if(inherit_push != null) {
        if(v.memoryInvariantAt(v.input.memory.length - 1)) {
          val dv = v.popMemory
          inherit_push.eval(dv)
        }
        else {
          null
        }
      }
      else {
        null
      }
    }
    else {
      rv
    }
  }

  private var vectorsym_ni: Int = 0
  private def nextvector(size: IntSym): VectorSymNamed = {
    if(inherit_promote != null) {
      inherit_promote.nextvector(size)
    }
    else if(inherit_push != null) {
      inherit_push.nextvector(size)
    }
    else {
      vectorsym_ni += 1
      new VectorSymNamed("x" + vectorsym_ni.toString, size)
    }
  }

  private var scalarsym_ni: Int = 0
  private def nextscalar: VectorSymNamedScalar = {
    if(inherit_promote != null) {
      inherit_promote.nextscalar
    }
    else if(inherit_push != null) {
      inherit_push.nextscalar
    }
    else {
      scalarsym_ni += 1
      new VectorSymNamedScalar("z" + scalarsym_ni.toString)
    }
  }

  private var intsym_ni: Int = 0
  private def nextint: IntSymD = {
    if(inherit_promote != null) {
      inherit_promote.nextint
    }
    else if(inherit_push != null) {
      inherit_push.nextint
    }
    else {
      intsym_ni += 1
      IntSymD("i" + intsym_ni.toString)
    }
  }

  private var doublesym_ni: Int = 0
  private def nextdouble: DoubleSymD = {
    if(inherit_promote != null) {
      inherit_promote.nextdouble
    }
    else if(inherit_push != null) {
      inherit_push.nextdouble
    }
    else {
      doublesym_ni += 1
      DoubleSymD("d" + doublesym_ni.toString)
    }
  }

  var codeacc: String = ""
  def emit(code: String, repls: Tuple2[String, Sym]*) {
    codeacc += Sym.subst(code.trim, Seq(repls:_*)) + "\n"
  }

    
  def getInput(iidx: Int, sidx: Seq[IRPoly]): String = {
    var mstr: String = "input" + iidx.toString
    for(s <- sidx) {
      mstr += "->idx[" + s.eval(params)(IntLikeIntSym).name + "]"
    }
    mstr += "->mat"
    mstr
  }

  var contains_converge: Boolean = false

  def eval(v: AVector): VectorSym = {
    val memo = memolookup(v)
    if(memo != null) {
      return memo
    }

    implicit val ilisimpl = IntLikeIntSym
    import IntLikeIntSym._
    import DoubleLikeDoubleSym._

    val rv: VectorSym = v match {
      case AVectorZero(input, sz) => new VectorSymIndexable {
        def indexAt(idx: IntSym): DoubleSym = DoubleSymL(0.0)
        val size: IntSym = sz.eval(params)
      }
      case AVectorConst(input, data) => {
        if(data.length == 1) {
          new VectorSymScalar(DoubleSymL(data(0)))
        }
        else {
          val v = nextvector(IntSymL(data.length))
          emit("double $v[$size] = { " + data.drop(1).foldLeft(data(0).toString)((a, u) => a + ", " + u.toString) + " };",
            "v" -> v, "size" -> IntSymL(data.length))
          v
        }
      }
      case AVectorSum(args) => {
        val avs: Seq[VectorSym] = args map (a => eval(a))
        if(avs forall (a => a.isInstanceOf[VectorSymIndexable])) {
          val avsi: Seq[VectorSymIndexable] = avs map (a => a.asInstanceOf[VectorSymIndexable])
          new VectorSymIndexable {
            def indexAt(idx: IntSym): DoubleSym = avsi.foldLeft(DoubleSymL(0.0): DoubleSym)((a,b) => a + b.indexAt(idx))
            def size: IntSym = avsi(0).size
          }
        }
        else {
          new VectorSym {
            def writeTo(dst: DstWritable): String = {
              avs.drop(1).foldLeft(avs(0).writeTo(dst))((a, u) => a + u.writeTo(dst.accumulator))
            }
          }
        }
      }
      case AVectorNeg(arg) => {
        val avv = eval(arg)
        avv match {
          case av: VectorSymIndexable => {
            new VectorSymIndexable {
              def indexAt(idx: IntSym): DoubleSym = -av.indexAt(idx)
              def size: IntSym = av.size
            }
          }
          case _ => {
            new VectorSym {
              def writeTo(dst: DstWritable): String = {
                avv.writeTo(new DstWritableScaled(dst, -1))
              }
            }
          }
        }
      }
      case AVectorScaleConstant(arg, scale) =>{
        val avv = eval(arg)
        avv match {
          case av: VectorSymIndexable => {
            new VectorSymIndexable {
              def indexAt(idx: IntSym): DoubleSym = av.indexAt(idx) * scale
              def size: IntSym = av.size
            }
          }
          case _ => {
            new VectorSym {
              def writeTo(dst: DstWritable): String = {
                avv.writeTo(new DstWritableScaled(dst, scale))
              }
            }
          }
        }
      }
      case AVectorCat(args) => {
        val avs: Seq[VectorSym] = args map (a => eval(a))
        new VectorSym {
          def writeTo(dst: DstWritable): String = {
            var rv: String = ""
            var offset: IRPoly = IRPoly.const(0, v.arity)
            for(i <- 0 until args.length) {
              rv += avs(i).writeTo(new DstWritableOffset(dst, offset.eval(params)))
              offset += args(i).size
            }
            rv
          }
        }
      }
      case AVectorCatFor(len, arg) => {
        val i = nextint
        val ev = new SolverCompilerCGen(params :+ i, memory, analysis.promoteSubs(v), this, null)
        val ilv = ev.eval(arg)
        val elen = len.eval(params)
        val offset = arg.size.sum(v.arity).eval(params :+ i)
        new VectorSym {
          def writeTo(dst: DstWritable): String = {
            Sym.subst("for(int $i = 0; $i < $len; $i++) {\n", Seq("i" -> i, "len" -> elen)) +
              ev.codeacc +
              ilv.writeTo(new DstWritableOffset(dst, offset)) +
              "}\n"
          }
        }
      }
      case AVectorSlice(arg, at, size) => {
        val vs = eval(arg).asInstanceOf[VectorSymIndexable]
        val osize = size.eval(params)
        new VectorSymIndexable {
          def indexAt(idx: IntSym): DoubleSym = vs.indexAt(idx + at.eval(params))
          def size: IntSym = osize
        }
      }
      case AVectorSumFor(len, arg) => {
        val i = nextint
        val ev = new SolverCompilerCGen(params :+ i, memory, analysis.promoteSubs(v), this, null)
        val osize = v.size.eval(params)(IntLikeIntSym)
        val ilv = ev.eval(arg)
        val elen = len.eval(params)
        new VectorSym {
          def writeTo(dst: DstWritable): String = {
            Sym.subst("for(int i = 0; i < $len; i++) ", Seq("len" -> elen)) + dst.writeAt(IntSymD("i"), DoubleSymL(0.0)) + ";\n" +
              Sym.subst("for(int $i = 0; $i < $len; $i++) {\n", Seq("i" -> i, "len" -> elen)) +
              ev.codeacc +
              ilv.writeTo(dst.accumulator) + 
              "}\n"
          }
        }
      }
      case AVectorMpyInput(arg, iidx, sidx) => {
        val vs = eval(arg).asInstanceOf[VectorSymIndexable]
        val mstr = new FlatSym(getInput(iidx, sidx))
        val osize = arg.input.args(iidx).body.codomain.substituteSeq(sidx).eval(params)
        if(arg.size == IRPoly.const(1, arg.arity)) {
          new VectorSymIndexable {
            def indexAt(idx: IntSym): DoubleSym = vs.indexAt(IntSymL(0)) * DoubleSymD(mstr.name + "[" + idx.name + "]")
            def size: IntSym = osize
          }
        }
        else {
          val nv = nextvector(osize)
          val isize = arg.size.eval(params)
          emit("""
            double $nv[$osize];
            for(int i = 0; i < $osize; i++) {
              double acc = 0.0;
              for(int j = 0; j < $isize; j++) {
                acc += $m[i*$isize+j] * $xj;
              }
              $nv[i] = acc;
            }
            """,
            "m" -> mstr, "nv" -> nv, "osize" -> osize, "isize" -> isize, "xj" -> vs.indexAt(IntSymD("j")))
          nv
        }
      }
      case AVectorMpyInputT(arg, iidx, sidx) => {
        val vs = eval(arg).asInstanceOf[VectorSymIndexable]
        val mstr = new FlatSym(getInput(iidx, sidx))
        val osize = arg.input.args(iidx).body.domain.substituteSeq(sidx).eval(params)
        if(arg.size == IRPoly.const(1, arg.arity)) {
          new VectorSymIndexable {
            def indexAt(idx: IntSym): DoubleSym = vs.indexAt(IntSymL(0)) * DoubleSymD(mstr.name + "[" + idx.name + "]")
            def size: IntSym = osize
          }
        }
        else {
          val nv = nextvector(osize)
          val isize = arg.size.eval(params)
          emit("""
            double $nv[$osize];
            for(int i = 0; i < $osize; i++) $nv[i] = 0.0;
            for(int j = 0; j < $isize; j++) {
              for(int i = 0; i < $osize; i++) {
                $nv[i] += $m[j*$osize+i] * $xj;
              }
            }
            """,
            "m" -> mstr, "nv" -> nv, "osize" -> osize, "isize" -> isize, "xj" -> vs.indexAt(IntSymD("j")))
          nv
        }
      }
      case AVectorRead(input, iidx) => {
        memory(iidx)
      }
      case AVectorDot(arg1, arg2) => {
        val av1 = eval(arg1).asInstanceOf[VectorSymIndexable]
        val av2 = eval(arg2).asInstanceOf[VectorSymIndexable]
        val i = IntSymD("i")
        val nv = nextscalar
        val asize = arg1.size.eval(params)(IntLikeIntSym)
        emit("double $nv = 0.0;", "nv" -> nv)
        emit("for(int i = 0; i < $asize; i++) $nv += $av1i * $av2i;",
          "asize" -> asize, "nv" -> nv, "av1i" -> av1.indexAt(i), "av2i" -> av2.indexAt(i))
        nv
      }
      case AVectorMpy(arg, scale) => {
        val va = eval(arg)
        val vs = eval(scale).asInstanceOf[VectorSymIndexable].indexAt(IntSymL(0))
        va match {
          case vaa: VectorSymIndexable => {
            new VectorSymIndexable {
              def indexAt(idx: IntSym): DoubleSym = vaa.indexAt(idx) * vs
              def size: IntSym = vaa.size
            }
          }
          case _ => {
            new VectorSym {
              def writeTo(dst: DstWritable): String = va.writeTo(new DstWritableScaled(dst, vs))
            }
          }
        }
      }
      case AVectorDiv(arg, scale) => {
        val va = eval(arg)
        val vs = eval(scale).asInstanceOf[VectorSymIndexable].indexAt(IntSymL(0))
        va match {
          case vaa: VectorSymIndexable => {
            new VectorSymIndexable {
              def indexAt(idx: IntSym): DoubleSym = vaa.indexAt(idx) / vs
              def size: IntSym = vaa.size
            }
          }
          case _ => {
            new VectorSym {
              def writeTo(dst: DstWritable): String = va.writeTo(new DstWritableScaledInv(dst, vs))
            }
          }
        }
      }
      case AVectorSqrt(arg) => {
        val va = eval(arg).asInstanceOf[VectorSymIndexable]
        val nv = nextscalar
        emit("double $nv = sqrt($x);", "nv" -> nv, "x" -> va.indexAt(IntSymL(0)))
        nv
      }
      case AVectorNorm2(arg) => {
        val va = eval(arg).asInstanceOf[VectorSymIndexable]
        val nv = nextscalar
        val asize = arg.size.eval(params)
        emit("double $nv = 1e-100;", "nv" -> nv)
        emit("for(int i = 0; i < $asize; i++) $nv += $avi * $avi;",
          "asize" -> asize, "nv" -> nv, "avi" -> va.indexAt(IntSymD("i")))
        nv
      }
      case AVectorNormInf(arg) => {
        val va = eval(arg).asInstanceOf[VectorSymIndexable]
        val nv = nextscalar
        val asize = arg.size.eval(params)
        emit("double $nv = 1e-100;", "nv" -> nv)
        emit("for(int i = 0; i < $asize; i++) $nv = fmax($nv, fabs($avi));",
          "asize" -> asize, "nv" -> nv, "avi" -> va.indexAt(IntSymD("i")))
        nv
      }
      case AVectorMax(arg1, arg2) => {
        val va1 = eval(arg1).asInstanceOf[VectorSymIndexable]
        val va2 = eval(arg2).asInstanceOf[VectorSymIndexable]
        new VectorSymIndexable {
          def indexAt(idx: IntSym): DoubleSym = DoubleSymD("fmax(" + va1.indexAt(idx).name + ", " + va2.indexAt(idx).name + ")")
          def size: IntSym = va1.size
        }
      }
      case AVectorMin(arg1, arg2) => {
        val va1 = eval(arg1).asInstanceOf[VectorSymIndexable]
        val va2 = eval(arg2).asInstanceOf[VectorSymIndexable]
        new VectorSymIndexable {
          def indexAt(idx: IntSym): DoubleSym = DoubleSymD("fmin(" + va1.indexAt(idx).name + ", " + va2.indexAt(idx).name + ")")
          def size: IntSym = va1.size
        }
      }
      case AVectorTolerance(input) => {
        new VectorSymIndexable {
          def indexAt(idx: IntSym): DoubleSym = DoubleSymD("tolerance")
          def size: IntSym = IntSymL(1)
        }
      }
      case AVectorConverge(arg, cond, body, itermax) => {
        val evarg = eval(arg)
        val ssize = arg.size.eval(params)
        val state = nextvector(ssize)
        val iterct = nextint
        val ev = new SolverCompilerCGen(params, memory :+ state, analysis.pushSubs(v), null, this)
        val evcond = ev.eval(cond).asInstanceOf[VectorSymIndexable].indexAt(IntSymL(0))
        val evbody = ev.eval(body)
        emit("double $statea[$ssize];\ndouble $stateb[$ssize];\ndouble* $state = $statea;\ndouble* $stateswap = $stateb;", 
          "state" -> state, "ssize" -> ssize)
        emit(evarg.writeTo(new DstWritableArray(state.name)))
        if(itermax >= 0) {
          emit("int $iterct = 0;", "iterct" -> iterct)
        }
        emit("while(1) {")
        emit(ev.codeacc)
        emit(evbody.writeTo(new DstWritableArray(state.name + "swap")))
        if(!ev.contains_converge) {
          emit("inner_loop_ct++;")
        }
        if(itermax >= 0) {
          emit("$iterct++;\nif($iterct >= " + itermax.toString + ") break;", "iterct" -> iterct)
        }
        emit("if($cond <= 0) break;", "cond" -> evcond)
        if((inherit_promote == null)&&(inherit_push == null)) {
          emit("fprintf(stderr, \"%f\\n\", $cond);", "cond" -> evcond)
        }
        else {
          emit("fprintf(stderr, \"    %f\\n\", $cond);", "cond" -> evcond)
        }
        emit("{double* tmp = $state; $state = $stateswap; $stateswap = tmp;}", "state" -> state)
        emit("}")
        this.contains_converge = true
        state
      }
      case _ =>
        throw new IRValidationException()
    }

    val needsSet = analysis.infocache(v).hint match {
      case SolverAnalysisHintWritable() => false
      case SolverAnalysisHintIndexable() => !rv.isInstanceOf[VectorSymIndexable]
      case SolverAnalysisHintArray() => !rv.isInstanceOf[VectorSymLM]
    }

    val trv = if(needsSet) {
      if((v.size == IRPoly.const(1, v.arity))&&(rv.isInstanceOf[VectorSymIndexable])) {
        val tdd = nextscalar
        emit("double $t = $x;", "t" -> tdd, "x" -> rv.asInstanceOf[VectorSymIndexable].indexAt(IntSymL(0)))
        tdd
      }
      else {
        val vsize = v.size.eval(params)
        val tdata = nextvector(vsize)
        emit("double $t[$size];", "t" -> tdata, "size" -> vsize)
        emit(rv.writeTo(new DstWritableArray(tdata.name)))
        tdata
      }
    }
    else {
      rv
    }

    memocache += (v -> trv)

    trv
  }

  /*
  def evalwrite(v: AVector, dst: VectorSym, dstop: DstOp) {
    val memo = memolookup(v)
    if(memo != null) {
      val osize = v.size.eval(params)(IntLikeIntSym)
      emit("for(int i = 0; i < " + osize.name + "; i++) " + dstop.sub(dst.at(IntSymD("i")), memo.at(IntSymD("i"))) + ";")
      return
    }
    
    val done: Boolean = v match {
      case AVectorZero(input, size) => {
        if(dstop.dstscale == "1.0") {
          true
        }
        else {
          false
        }
      }
      case AVectorConst(input, data) => {
        for(i <- 0 until data.length) {
          emit(dstop.sub(dst.at(new IntSymL(i)), new FlatSym(data(i).toString)) + ";")
        }
        true
      }
      case AVectorSum(args) => {
        evalwrite(args(0), dst, dstop)
        for(a <- args.drop(1)) {
          evalwrite(a, dst, DstOp("1.0", dstop.srcscale)
        }
        true
      }
      case AVectorNeg(arg) => {
        evalwrite(arg, dst, DstOp(dstscale, "(-1.0 * " + dstop.srcscale + ")"))
        true
      }
      case AVectorScaleConstant(arg, scale) => {
        evalwrite(arg, dst, DstOp(dstscale, "(" + scale.toString + " * " + dstop.srcscale + ")"))
        true
      }
      case AVectorCat(args) => {
        var prx: IRPoly = IRPoly.const(0, v.arity)
        for(a <- args) {
          val erxa = prx.eval(params)(IntLikeIntSym)
          evalwrite(a, new VectorSym(dst.vss.substituteSeq("$", "($ + " + erxa.toString + ")")), dstop)
          prx = prx + a.size
        }
        true
      }
      case AVectorCatFor(len, arg) => {
        val osize = v.size.eval(params)(IntLikeIntSym)
        val nv = nextvector
        val i = nextint
        val j = nextint
        val ev = new SolverCompilerCGen(params :+ i, memory, this, null)
        ev.evalwrite(arg, new VectorSym(dst.vss.substituteSeq("$", "($ + " j.name.toString + ")")), dstop)
        emit("for(int $i = 0, $j = 0; $i < $len; $i++) {",
          "i" -> i, "j" -> j, "len" -> len.eval(params)(IntLikeIntSym))
        emit(ev.codeacc)
        emit("$j += $msize;", "j" -> j, "msize" -> arg.size.eval(params :+ i)(IntLikeIntSym))
        emit("}")
        true
      }
      case AVectorSlice(arg, at, size) => {
        false
      }
      case AVectorSumFor(len, arg) => {
        val osize = v.size.eval(params)(IntLikeIntSym)
        val nv = nextvector
        val i = nextint
        val ev = new SolverCompilerCGen(params :+ i, memory, this, null)
        ev.evalwrite(arg, dst, DstOp("1.0", dstop.srcscale))
        if(dstop.srcscale == "0.0") {
          emit("for(int i = 0; i < " + osize.name + "; i++) " + dst.at(IntSymD("i")).name + " = 0.0;")
        }
        else if(dstop.srcscale != "1.0") {
          emit("for(int i = 0; i < " + osize.name + "; i++) " + dst.at(IntSymD("i")).name + " *= " + dstop.srcscale + ";")
        }
        emit("for(int $i = 0; $i < $len; $i++) {",
          "i" -> i, "len" -> len.eval(params)(IntLikeIntSym))
        emit(ev.codeacc)
        emit("}")
        true
      }
      case AVectorMpyInput(arg, iidx, sidx) => {
        val vs = eval(arg)
        val mstr = new FlatSym(getInput(iidx, sidx))
        val nv = nextvector
        val isize = arg.size.eval(params)(IntLikeIntSym)
        val osize = arg.input.args(iidx).body.codomain.substituteSeq(sidx).eval(params)(IntLikeIntSym)
        emit("""
          double $nv[$osize];
          for(int i = 0; i < $osize; i++) {
            $nv[i] = 0.0;
            for(int j = 0; j < $isize; j++) {
              $nv[i] += $m[i*$isize+j] * $xj;
            }
          }
          """,
          "m" -> mstr, "nv" -> nv, "osize" -> osize, "isize" -> isize, "xj" -> vs.at(IntSymD("j")))
        nv
      }
      case AVectorMpyInputT(arg, iidx, sidx) => {
        val vs = eval(arg)
        val mstr = new FlatSym(getInput(iidx, sidx))
        if(arg.size == IRPoly.const(1, arg.arity)) {
          new VectorSym("(" + mstr.name + "[$] * " + vs.at(new IntSymL(0)).name + ")")
        }
        else {
          val nv = nextvector
          val isize = arg.size.eval(params)(IntLikeIntSym)
          val osize = arg.input.args(iidx).body.domain.substituteSeq(sidx).eval(params)(IntLikeIntSym)
          emit("""
            double $nv[$osize];
            for(int i = 0; i < $osize; i++) $nv[i] = 0.0;
            for(int j = 0; j < $isize; j++) {
              for(int i = 0; i < $osize; i++) {
                $nv[i] += $m[j*$osize+i] * $xj;
              }
            }
            """,
            "m" -> mstr, "nv" -> nv, "osize" -> osize, "isize" -> isize, "xj" -> vs.at(IntSymD("j")))
          nv
        }
        // emit("""
        //   double $nv[$osize];
        //   for(int i = 0; i < $osize; i++) {
        //     $nv[i] = 0.0;
        //     for(int j = 0; j < $isize; j++) {
        //       $nv[i] += $m[j*$osize+i] * $xj;
        //     }
        //   }
        //   """,
        //   "m" -> mstr, "nv" -> nv, "osize" -> osize, "isize" -> isize, "xj" -> vs.at(new IntSym("j")))
      }
      case AVectorRead(input, iidx) => {
        memory(iidx)
      }
      case AVectorDot(arg1, arg2) => {
        val av1 = eval(arg1)
        val av2 = eval(arg2)
        val nv = nextvector
        val asize = arg1.size.eval(params)(IntLikeIntSym)
        emit("double d$nv = 0.0;", "nv" -> nv)
        emit("for(int i = 0; i < $asize; i++) d$nv += $av1i * $av2i;",
          "asize" -> asize, "nv" -> nv, "av1i" -> av1.at(new IntSym("i")), "av2i" -> av2.at(new IntSym("i")))
        new VectorSym("d" + nv.name)
      }
      case AVectorMpy(arg, scale) => {
        val va = eval(arg)
        val vs = eval(scale)
        new VectorSym("(" + va.vss + " * " + vs.at(new IntSymL(0)).name + ")")
      }
      case AVectorDiv(arg, scale) => {
        val va = eval(arg)
        val vs = eval(scale)
        new VectorSym("(" + va.vss + " / " + vs.at(new IntSymL(0)).name + ")")
      }
      case AVectorSqrt(arg) => {
        val va = eval(arg)
        new VectorSym("sqrt(" + va.vss + ")")
      }
      case AVectorNorm2(arg) => {
        val av = eval(arg)
        val nv = nextvector
        val asize = arg.size.eval(params)(IntLikeIntSym)
        emit("double d$nv = 0.0;", "nv" -> nv)
        emit("for(int i = 0; i < $asize; i++) d$nv += $avi * $avi;",
          "asize" -> asize, "nv" -> nv, "avi" -> av.at(new IntSym("i")))
        new VectorSym("d" + nv.name)
      }
      case AVectorNormInf(arg) => {
        val av = eval(arg)
        val nv = nextvector
        val asize = arg.size.eval(params)(IntLikeIntSym)
        emit("double d$nv = 0.0;", "nv" -> nv)
        emit("for(int i = 0; i < $asize; i++) d$nv = fmax(d$nv, fabs($avi));",
          "asize" -> asize, "nv" -> nv, "avi" -> av.at(new IntSym("i")))
        new VectorSym("d" + nv.name)
      }
      case AVectorMax(arg1, arg2) => {
        val av1 = eval(arg1)
        val av2 = eval(arg2)
        new VectorSym("fmax(" + av1.vss + ", " + av2.vss + ")")
      }
      case AVectorMin(arg1, arg2) => {
        val av1 = eval(arg1)
        val av2 = eval(arg2)
        new VectorSym("fmin(" + av1.vss + ", " + av2.vss + ")")
      }
      case AVectorTolerance(input) => {
        new VectorSym("tolerance")
      }
      case AVectorConverge(arg, cond, body, itermax) => {
        val evarg = eval(arg)
        val state = nextvector
        val iterct = nextint
        val ev = new SolverCompilerCGen(params, memory :+ state, null, this)
        val evcond = ev.eval(cond)
        val evbody = ev.eval(body)
        val ssize = arg.size.eval(params)(IntLikeIntSym)
        emit("double $state[$ssize];", "state" -> state, "ssize" -> ssize)
        emit("for(int i = 0; i < $ssize; i++) $state[i] = $evai;", "state" -> state, "ssize" -> ssize, "evai" -> evarg.at(new IntSym("i")))
        if(itermax >= 0) {
          emit("int $iterct = 0;", "iterct" -> iterct)
        }
        emit("while(1) {")
        emit(ev.codeacc)
        emit("for(int i = 0; i < $ssize; i++) $state[i] = $evbi;", "state" -> state, "ssize" -> ssize, "evbi" -> evbody.at(new IntSym("i")))
        if(!ev.contains_converge) {
          emit("inner_loop_ct++;")
        }
        if(itermax >= 0) {
          emit("$iterct++;\nif($iterct >= " + itermax.toString + ") break;", "iterct" -> iterct)
        }
        emit("if($cond <= 0) break;", "cond" -> evcond.at(new IntSymL(0)))
        emit("}")
        this.contains_converge = true
        state
      }
      case _ =>
        throw new IRValidationException()
    }
  }
  */
}

/*
trait Sym {
  def name: String
}
class FlatSym(val name: String) extends Sym
trait IntSym extends Sym {
  def is0: Boolean
  def is1: Boolean
}
class IntSymL(val i: Int) extends IntSym {
  def name: String = "i" + i.toString
  def is0: Boolean = false
  def is1: Boolean = false
}
class IntSymD(val c: Int) extends IntSym {
  def name: String = c.toString
  def is0: Boolean = (c == 0)
  def is1: Boolean = (c == 1)
}
class IntSymM(val name: String) extends IntSym {
  def is0: Boolean = false
  def is1: Boolean = false
}
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
class MatrixSym(val name: String) extends Sym
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
  // private var matrixsym_ni: Int = 0
  // private def nextmatrix: MatrixSym = {
  //   matrixsym_ni += 1
  //   new MatrixSym(matrixsym_ni)
  // }

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
static solution_t solve(int* params, input_t** inputs, double* output, double tolerance) {"""
    rv += "\nint inner_loop_ct = 0;\n\n"
    for(i <- 0 until arity) {
      rv += "int param" + i.toString + " = params[" + i.toString + "];\n"
    }
    rv += codeacc
    rv += "\n\nsolution_t rv;\nrv.num_iterations = inner_loop_ct;\nreturn rv;\n\n}\n\n"
    rv += "static int variable_size(int* params) {\n"
    for(i <- 0 until arity) {
      rv += "int param" + i.toString + " = params[" + i.toString + "];\n"
    }
    rv += "return " + output_size.name + ";\n}\n\n"
    for(i <- 0 until inputdescs.length) {
      for(j <- 0 until inputdescs(i).dims.length) {
        rv += "static int structure_input" + i.toString + "_order" + j.toString + "(int* params, int* iidx) {\n"
        for(i <- 0 until arity) {
          rv += "int param" + i.toString + " = params[" + i.toString + "];\n"
        }
        val pps = for (k <- 0 until arity) yield new IntSymM("params[" + k.toString + "]")
        val iis = for (k <- 0 until j) yield new IntSymM("iidx[" + k.toString + "]")
        val szs = inputdescs(i).dims(j)(pps ++ iis)
        rv += "return " + szs.name + ";\n}\n\n"
      }
      rv += "static matrix_shape_t shape_input" + i.toString + "(int* params, int* iidx) {\n"
      for(i <- 0 until arity) {
        rv += "int param" + i.toString + " = params[" + i.toString + "];\n"
      }
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
    val lrx = rv.split("\n")
    rv = ""
    var indentlvl = 0
    for(l <- lrx) {
      if((l.contains("}"))&&(!l.contains("{"))) indentlvl -= 1
      rv += ("  " * indentlvl) + l.trim + "\n"
      if((l.contains("{"))&&(!l.contains("}"))) indentlvl += 1
    }
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
    emit("for(int $i = 0; $i < $size; $i++) {\n $po[$i] = $o; \n}",
      "o" -> o.at(i), "size" -> o.size, "i" -> i, "po" -> po)
  }

  def print(x: VectorSym, name: String, fmt: String) {
    val i = nextint
    emit("printf(\"" + name + " = [\");\nfor(int $i = 0; $i < $size; $i++) { \n printf(\"" + fmt + ", \", $x);\nprintf(\"]\\n\"); \n}",
      "i" -> i, "x" -> x.at(i), "size" -> x.size)
  }


  lazy val params: Seq[IntSym] = for(i <- 0 until arity) yield {
    // val rv = nextint
    // emit("int $rv = param" + i.toString + ";", "rv" -> rv)
    // rv
    new IntSymM("param" + i.toString)
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
      else if(x.is0||y.is0) {
        new IntSymD(0)
      }
      else if(x.is1) {
        y
      }
      else if(y.is1) {
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
  def const(data: Seq[Double]): VectorSym = {
    // val rv = nextvector(intlikei.int2T(1))
    // emit("double $rv[1] = {1.0};", "rv" -> rv)
    // rv
    // new VectorSymFlat("1.0", intlikei.int2T(1))
    if(data.length == 1) {
      new VectorSymFlat(data(0).toString, intlikei.int2T(1))
    }
    else {
      val rv = nextvector(intlikei.int2T(data.length))
      emit("double $rv[" + data.length.toString + "] = { " + data.drop(1).foldLeft(data(0).toString)((a,u) => a + ", " + u.toString) + " };",
        "rv" -> rv);
      rv
    }
  }
  //linear operators
  def sum(args: Seq[VectorSym]): VectorSym = {
    // var sexpr = args.drop(1).foldLeft("(" + args(0).at(new IntSymM("$")).name)((a, u) => a + "+" + u.at(new IntSymM("$")).name) + ")"
    // val irv = new VectorSymFlat(sexpr, args(0).size)
    // val rv = nextvector(args(0).size)
    // val i = nextint
    // emit("""
    //   double $rv[$size];
    //   for(int $i = 0; $i < $size; $i++) $rv[$i] = $x;
    //   """,
    //   "rv" -> rv, "i" -> i, "x" -> irv.at(i), "size" -> args(0).size)
    // rv
    var sexpr = args.drop(1).foldLeft("(" + args(0).at(new IntSymM("$")).name)((a, u) => a + "+" + u.at(new IntSymM("$")).name) + ")"
    new VectorSymFlat(sexpr, args(0).size)
  }
  def sumfor(len: IntSym, size: IntSym, arg: (IntSym => VectorSym)): VectorSym = {
    val rv = nextvector(size)
    if(size.is1) {
      val i = nextint
      val j = nextint
      val k = nextint
      emit("""
        double d$rv = 0.0;
        for(int $i = 0; $i < $len; $i++) {
        """,
        "rv" -> rv, "size" -> size, "len" -> len, "i" -> i)
      val va = arg(i)
      emit("""
        d$rv += $va;
        }
        """,
        "rv" -> rv, "va" -> va.at(intlikei.int2T(0)), "size" -> size, "len" -> len)
      return new VectorSymFlat("d" + rv.name, size)
    }
    else {
      val i = nextint
      val j = nextint
      val k = nextint
      emit("""
        double $rv[$size];
        for(int $j = 0; $j < $size; $j++) {
          $rv[$j] = 0.0;
        }
        for(int $i = 0; $i < $len; $i++) {
        """,
        "rv" -> rv, "size" -> size, "len" -> len, "i" -> i, "j" -> j)
      val va = arg(i)
      emit("""
        for(int $k = 0; $k < $size; $k++) {
          $rv[$k] += $va;
        }
        }
        """,
        "rv" -> rv, "va" -> va.at(k), "size" -> size, "len" -> len, "k" -> k)
      return rv
    }
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
  def cat(args: Seq[VectorSym]): VectorSym = {
    val irv = args.drop(1).foldLeft(args(0).size)((a,u) => intlikei.add(a, u.size))
    val rv = nextvector(irv)
    val i = nextint
    emit("double $rv[$size];\nint $i = 0;", "rv" -> rv, "size" -> irv, "i" -> i)
    for (a <- args) {
      val j = nextint
      emit("for(int $j = 0; $j < $xsize; $i++, $j++) $rv[$i] = $x;",
        "rv" -> rv, "xsize" -> a.size, "x" -> a.at(j), "i" -> i, "j" -> j)
    }
    rv
    // new VectorSymFlat(
    //   "(($ < " + arg1.size.name + ") ? " + arg1.at(new IntSymM("$")).name + " : " + arg2.at(new IntSymM("($ - " + arg1.size.name + ")")).name + ")", 
    //   intlikei.add(arg1.size, arg2.size))
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
    if(va.size.is1) {
      emit("""
          $rv[$k] = $va;
          $k++;
        }
        """,
        "rv" -> rv, "len" -> len, "vasize" -> va.size, "va" -> va.at(intlikei.int2T(0)), "i" -> i, "j" -> j, "k" -> k, "size" -> size)
    }
    else {
      emit("""
        for(int $j = 0; $j < $vasize; $j++, $k++) {
          $rv[$k] = $va;
        }
        }
        """,
        "rv" -> rv, "len" -> len, "vasize" -> va.size, "va" -> va.at(j), "i" -> i, "j" -> j, "k" -> k, "size" -> size)
    }
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
    new VectorSymFlat(arg.at(new IntSymM("($ + " + at.name + ")")).name, size)
  }
  //nonlinear operators
  def dot(arg1: VectorSym, arg2: VectorSym): VectorSym = {
    val rvsize = intlikei.int2T(1)
    val rv = nextvector(rvsize)
    if(arg1.size.is1) {
      emit("""
        double d$rv = $x * $y;
        """,
        "rv" -> rv, "x" -> arg1.at(intlikei.int2T(0)), "y" -> arg2.at(intlikei.int2T(0)))
      return new VectorSymFlat("d" + rv.name, intlikei.int2T(1))
    }
    else {
      val i = nextint
      emit("""
        double d$rv = 0.0;
        for(int $i = 0; $i < $size; $i++) {
          d$rv += $x * $y;
        }
        """,
        "rv" -> rv, "i" -> i, "size" -> arg1.size, "x" -> arg1.at(i), "y" -> arg2.at(i))
      return new VectorSymFlat("d" + rv.name, intlikei.int2T(1))
    }
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
    if(arg.size.is1) {
      return new VectorSymFlat("fabs(" + arg.at(intlikei.int2T(0)).name + ")", intlikei.int2T(1))
    }
    else {
      val rvsize = intlikei.int2T(1)
      val rv = nextvector(rvsize)
      val i = nextint
      emit("""
        double d$rv = 0.0;
        for(int $i = 0; $i < $size; $i++) {
          if (fabs($x) > d$rv) {
            d$rv = fabs($x);
          }
        }
        """,
        "rv" -> rv, "x" -> arg.at(i), "size" -> arg.size, "i" -> i)
      return new VectorSymFlat("d" + rv.name, intlikei.int2T(1))
    }
  }
  def tolerance(): VectorSym = {
    new VectorSymFlat("tolerance", intlikei.int2T(1))
  }

  def matrixmpy(m: MatrixSym, osize: IntSym, x: VectorSym): VectorSym = {
    if((osize.is1)&&(x.size.is1)) {
      new VectorSymFlat("(" + m.name + "[0] * " + x.at(intlikei.int2T(0)).name + ")", osize)
    }
    else if(x.size.is1) {
      new VectorSymFlat("(" + m.name + "[$] * " + x.at(intlikei.int2T(0)).name + ")", osize)
    }
    else {
      val rv = nextvector(osize)
      val i = nextint
      val j = nextint
      if(true) {
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
      }
      else {
        val k = nextint
        emit("""
          double $rv[$osize];
          for(int $i = 0; $i < $osize; $i++) {
            $rv[$i] = 0.0;
          }
          for(int $j = 0; $j < $isize; $j++) {
            for(int $k = 0; $k < $osize; $k++) {
              $rv[$k] += $m[$k*$isize+$j] * $x;
            }
          }
          """,
          "m" -> m, "rv" -> rv, "isize" -> x.size, "osize" -> osize, "i" -> i, "j" -> j, "k" -> k, "x" -> x.at(j))
      }
      rv
    }
  }
  def matrixmpytranspose(m: MatrixSym, osize: IntSym, x: VectorSym): VectorSym = {
    if((osize.is1)&&(x.size.is1)) {
      new VectorSymFlat("(" + m.name + "[0] * " + x.at(intlikei.int2T(0)).name + ")", osize)
    }
    else if(x.size.is1) {
      new VectorSymFlat("(" + m.name + "[$] * " + x.at(intlikei.int2T(0)).name + ")", osize)
    }
    else {
      val rv = nextvector(osize)
      val i = nextint
      val j = nextint
      if(false) {
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
      }
      else {
        val k = nextint
        emit("""
          double $rv[$osize];
          for(int $i = 0; $i < $osize; $i++) {
            $rv[$i] = 0.0;
          }
          for(int $j = 0; $j < $isize; $j++) {
            for(int $k = 0; $k < $osize; $k++) {
              $rv[$k] += $m[$j*$osize+$k] * $x;
            }
          }
          """,
          "m" -> m, "rv" -> rv, "isize" -> x.size, "osize" -> osize, "i" -> i, "j" -> j, "k" -> k, "x" -> x.at(j))
      }
      rv
    }
  }

  def matrixget(mats: InputSym, at: Seq[IntSym]): MatrixSym = {
    // val rv = nextmatrix
    // var emitstr: String = "double* " + rv.name + " = " + mats.name
    // for(a <- at) {
    //   emitstr += "->idx[" + a.name + "]"
    // }
    // emitstr += "->mat;"
    // emit(emitstr)
    // rv
    var mstr: String = mats.name
    for(a <- at) {
      mstr += "->idx[" + a.name + "]"
    }
    mstr += "->mat"
    new MatrixSym(mstr)
  }

  def vectorget(vecs: MemorySym, size: IntSym, at: Seq[IntSym]): VectorSym = {
    // val rv = nextvector(size)
    // var emitstr: String = "double* " + rv.name + " = " + vecs.name
    // for(i <- at) {
    //   emitstr += "->idx[" + i.name + "]"
    // }
    // emitstr += "->vec;"
    // emit(emitstr)
    // rv
    var vstr: String = vecs.name
    for(i <- at) {
      vstr += "->idx[" + i.name + "]"
    }
    vstr += "->vec[$]"
    new VectorSymFlat(vstr, size)
  }

  def vectorset(src: VectorSym, vecs: MemorySym, at: Seq[IntSym]): MemorySym = {
    var emitstr: String = ""
    val v = nextvector(src.size)
    val j = nextint
    emitstr += "double " + v.name + "[" + src.size.name + "];\n"
    emitstr += "for(int " + j.name + " = 0; " + j.name + " < " + src.size.name + "; " + j.name + "++) { \n"
    emitstr += v.name + "[" + j.name + "] = " + src.at(j).name + ";\n}\n"
    val i = nextint
    emitstr += "for(int " + i.name + " = 0; " + i.name + " < " + src.size.name + "; " + i.name + "++) {\n"
    emitstr += vecs.name
    for(i <- at) {
      emitstr += "->idx[" + i.name + "]"
    }
    emitstr += "->vec[" + i.name + "] = " + v.at(i).name + ";\n}\n"
    emit(emitstr)
    vecs.copyinvalidate
  }

  // def vectorput(src: VectorSym): MemorySym = {
  //   var emitstr: String = ""
  //   val vv = nextvector(src.size)
  //   val rv = nextmemory
  //   val j = nextint
  //   emit("""
  //     double $vv[$size];
  //     for(int $j = 0; $j < $size; $j++) $vv[$j] = $x;
  //     memory_t* $rv = (memory_t*)$vv;
  //     """,
  //     "vv" -> vv, "rv" -> rv, "size" -> src.size, "j" -> j, "x" -> src.at(j))
  //   rv
  // }

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
  var converge_loop_depth: Int = 0
  def converge(memory: Seq[MemorySym], itermax: Int, body: (Seq[MemorySym]) => (Seq[MemorySym], VectorSym)): Seq[MemorySym] = {
    val ict = nextint
    if(itermax > 0) {
      emit("int $ict = 0;", "ict" -> ict)
    }
    emit("while(1) {")
    isconverge = false
    converge_loop_depth += 1
    val (newmem, cond) = body(memory)
    converge_loop_depth -= 1
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
    emit("if($cond <= 0.0) break;", "cond" -> cond.at(intlikei.int2T(0)))
    //emit("fprintf(stderr, \"" + ("  " * converge_loop_depth) + "cond = %g\\n\", $cond);", "cond" -> cond.at(intlikei.int2T(0)))
    emit("}")
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
*/
