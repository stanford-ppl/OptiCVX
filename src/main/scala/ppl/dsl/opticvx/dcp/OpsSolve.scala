package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvers
import ppl.dsl.opticvx.solvergen._
import ppl.dsl.opticvx.runtime.definite._
import ppl.dsl.opticvx.runtime.cgen._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set

import java.io._
import java.lang.Runtime

trait DCPOpsSolve extends DCPOpsFunction {

  def inputscalar: InputArgDesc = 
    InputArgDesc(Seq(), IRPoly.const(1, globalArity), IRPoly.const(1, globalArity))

  def inputvector(n: IRPoly): InputArgDesc = {
    if(n.arity != globalArity) throw new IRValidationException()
    InputArgDesc(Seq(), IRPoly.const(1, globalArity), n)
  }

  def inputmatrix(m: IRPoly, n: IRPoly): InputArgDesc = {
    if(m.arity != globalArity) throw new IRValidationException()
    if(n.arity != globalArity) throw new IRValidationException()
    InputArgDesc(Seq(), m, n)
  }

  val PrimalDualOperatorSplitting = ppl.dsl.opticvx.solvers.PrimalDualOperatorSplitting
  val PrimalDualSubgradient = ppl.dsl.opticvx.solvers.PrimalDualSubgradient
  val AlternatingProjections = ppl.dsl.opticvx.solvers.AlternatingProjections

  class CvxSProblem(val problem: Problem) {
    def gen(generator: SolverGen): CvxSSolver = {
      new CvxSSolver(generator.gen(problem))
    }
    def gen(): CvxSSolver = gen(PrimalDualOperatorSplitting)
  }

  class CvxSSolver(val solver: Solver) {
    def solve_definite(pp: Int*)(ins: MultiSeq[MatrixDefinite]*): CvxSSolutionDefinite = {
      if(pp.length != solver.arity) throw new IRValidationException()
      if(ins.length != solver.input.args.length) throw new IRValidationException()
      val spp: Seq[Int] = Seq(pp:_*)
      val srt = SolverRuntimeDefinite
      val mm = for(m <- solver.input.memory) yield srt.memoryallocfrom(m, spp)
      val vv = solver.run[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]](srt, spp, Seq(ins:_*), mm)
      new CvxSSolutionDefinite(spp, vv(0))
    }
    def solve_cgen(): CvxSSolverCGen = {
      if(solver.input.args.length != 0) throw new IRValidationException()
      val srt = new SolverRuntimeCGen(solver.arity, solver.input.args.length)
      val mm = for(m <- solver.input.memory) yield srt.memoryallocfrom(m, srt.params)
      val vv = solver.run(srt, srt.params, Seq(), mm)
      new CvxSSolverCGen(srt, vv(0))
    }
  }

  class CvxSSolverCGen(val srt: SolverRuntimeCGen, val vv: MemorySym) {
    def code: String = srt.code

    def resolve_print(x: CvxExprSymbol, name: String, fmt: String) {
      val xr = x.resolution.eval(srt, srt.params, Seq(), Seq(vv))
      srt.print(xr, name, fmt)
    }

    def resolve_output(x: CvxExprSymbol) {
      val xr = x.resolution.eval(srt, srt.params, Seq(), Seq(vv))
      srt.add_output(xr)
    }

    def resolve_output_and_print(x: CvxExprSymbol, name: String, fmt: String) {
      val xr = x.resolution.eval(srt, srt.params, Seq(), Seq(vv))
      srt.add_output(xr)
      srt.print(xr, name, fmt)
    }

    def compile(): CvxCSolverProgram = {
      val fout = new File("cgen/out.c")
      val fwriter = new FileWriter(fout)
      fwriter.write(srt.code)
      fwriter.close()
      val fhout = new File("cgen/out.h")
      val fhwriter = new FileWriter(fhout)
      fhwriter.write(srt.hcode)
      fhwriter.close()
      val fmout = new File("cgen/main.c")
      val fmwriter = new FileWriter(fmout)
      fmwriter.write(srt.maincode)
      fmwriter.close()
      val rt = java.lang.Runtime.getRuntime()
      val cproc = rt.exec(
        Array[String]("gcc", "--std=gnu99", "-O1", "-o", "cgen.out" ,"out.c", "main.c"),
        Array[String](),
        new File("cgen"))
      cproc.waitFor()
      new CvxCSolverProgram(srt.arity)
    }
  }

  class CvxCSolverProgram(val arity: Int) {
    def run(params: Int*) {
      if(params.length != arity) throw new IRValidationException()
      val argarray: Array[String] = new Array[String](1 + params.length)
      argarray(0) = "./cgen.out"
      for(i <- 0 until params.length) {
        argarray(i + 1) = params(i).toString
      }
      val rt = java.lang.Runtime.getRuntime()
      val cproc = rt.exec(
        argarray,
        Array[String](),
        new File("cgen"))
      val in = cproc.getInputStream()
      var c: Int = in.read()
      while (c != -1) {
        System.out.write(c.toChar);
        c = in.read();
      }
      cproc.waitFor()
    }
  }

  def inputscalardefinite(a: Double): MultiSeq[MatrixDefinite] = {
    val amd = MatrixDefinite(1, 1, Seq(a))
    MultiSeqA0(amd)
  }

  def inputvectordefinite(as: Seq[Double]): MultiSeq[MatrixDefinite] = {
    val amd = MatrixDefinite(as.length, 1, as)
    MultiSeqA0(amd)
  }

  class CvxSSolutionDefinite(val pp: Seq[Int], val vv: MultiSeq[Seq[Double]]) {
    def resolve(x: CvxExprSymbol): Seq[Double] = {
      val srt = SolverRuntimeDefinite
      x.resolution.eval[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]](srt, pp, Seq(), Seq(vv))
    }


      // val syms = (s_over map (x => x.symbol)) ++ (s_let map (x => x.symbol))
      // for(s <- syms) {
      //   val x = s.boundexpr
      //   val msfx: Function = s_over.foldLeft(x.fx.expand(tmpfxn.varSize))((a,b) => a.minimize_over_lastarg)
      //   val sinput = InputDesc(msfx.arity, msfx.input.args, Seq(tt.input.memory(0)))
      //   val sv = AVectorSum(
      //     msfx.valueOffset.addMemory(sinput.memory),
      //     msfx.valueVarAlmap.addMemory(sinput.memory).mmpy(AVectorRead(sinput, 0, Seq())))
      //   s.rset(sv.eval[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]](srt, pp, Seq(), Seq(vv(0))))
      // }
  }

  def problem(
    ts_params: =>CvxParams,
    ts_inputs: =>CvxInputs,
    ts_over: =>CvxOver,
    ts_let: =>CvxLet,
    ts_where: =>CvxWhere,
    ts_value: =>CvxValue
    ): CvxSProblem =
  {
    // bind the parameters
    val s_params: Seq[CvxParamSymbol] = ts_params.params
    for(i <- 0 until s_params.length) {
      s_params(i).bind(IRPoly.param(i, s_params.length))
    }
    globalArity = s_params.length
    // bind the inputs
    val s_inputs: Seq[CvxInputBinding] = ts_inputs.inputs
    val s_inputsize = InputDesc(globalArity, s_inputs map (s => s.argdesc), Seq())
    globalInputSize = s_inputsize
    for(i <- 0 until s_inputs.length) {
      s_inputs(i).symbol.bind(CvxInput(i))
    }
    // there are no arguments or DCP information
    // get the variables
    val s_over = ts_over.vars
    val s_argsize: Seq[IRPoly] = (s_over map (x => x.size))
    globalArgSize = s_argsize
    // bind the variables
    for(i <- 0 until s_over.length) {
      s_over(i).symbol.bindexpr(CvxExpr(Function.param(i, s_inputsize, s_argsize)))
    }
    // bind the let-expressions
    val s_let = ts_let.exprs
    for(b <- s_let) {
      b.symbol.bindexpr(b.expr)
    }
    // get the constraints and value
    val s_where = ts_where.constraints
    val s_value = ts_value
    globalArity = -1
    globalInputSize = null
    globalArgSize = null
    // make the return value
    val min_value = s_value match {
      case x: CvxMinimize => x.expr.fx
      case x: CvxMaximize => -x.expr.fx
    }
    val tmpfxn = s_where.foldLeft(min_value)((a,b) => a + b.fx).simplify

    val minfxn = s_over.foldLeft(tmpfxn)((a,b) => a.minimize_over_lastarg).simplify
    if(minfxn.codomain != IRPoly.const(1, s_params.length)) throw new IRValidationException()
    val problem = Problem(
      s_inputsize,
      minfxn.valueVarAlmap.T.mmpy(AVectorOne(s_inputsize)),
      minfxn.affineVarAlmap,
      minfxn.affineOffset,
      minfxn.conicVarAlmap,
      minfxn.conicOffset,
      minfxn.conicCone)

    /* rebind the functions */
    val syms = (s_over map (x => x.symbol)) ++ (s_let map (x => x.symbol))
    for(s <- syms) {
      val x = s.boundexpr
      val msfx: Function = s_over.foldLeft(x.fx.expand(tmpfxn.varSize))((a,b) => a.minimize_over_lastarg)
      //s.releaseexpr()
      //s.bindexpr(new CvxExpr(msfx))
      val sinput = InputDesc(msfx.arity, msfx.input.args, Seq(MemoryArgDesc(Seq(), msfx.varSize)))
      val sv = AVectorSum(
        msfx.valueOffset.addMemory(sinput.memory),
        msfx.valueVarAlmap.addMemory(sinput.memory).mmpy(AVectorRead(sinput, 0, Seq())))
      s.rset(sv)
      //s.rset(sv.eval[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]](srt, pp, Seq(), Seq(vv(0))))
    }

    new CvxSProblem(problem)
  }

}

