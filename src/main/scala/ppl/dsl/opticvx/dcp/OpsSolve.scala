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

  val PrimalDualOperatorSplitting = ppl.dsl.opticvx.solvers.PrimalDualOperatorSplitting
  val PrimalDualProjections = ppl.dsl.opticvx.solvers.PrimalDualProjections
  val PrimalDualHomogeneous = ppl.dsl.opticvx.solvers.PrimalDualHomogeneous
  val PrimalDualHomogeneousEx = ppl.dsl.opticvx.solvers.PrimalDualHomogeneousEx
  val PrimalDualSubgradient = ppl.dsl.opticvx.solvers.PrimalDualSubgradient
  val AlternatingProjections = ppl.dsl.opticvx.solvers.AlternatingProjections

  class CvxSProblem(val problem: Problem) {
    def gen(generator: SolverGen): CvxSSolver = {
      new CvxSSolver(generator.gen(problem))
    }
    def gen(): CvxSSolver = gen(PrimalDualOperatorSplitting)
  }

  class CvxSSolver(val solver: Solver) {
    def solve(pp: Int*)(ins: MultiSeq[MatrixDefinite]*)(tolerance: Double): CvxSSolutionDefinite = {
      throw new IRValidationException()
      // if(pp.length != solver.arity) throw new IRValidationException()
      // if(ins.length != solver.input.args.length) throw new IRValidationException()
      // val spp: Seq[Int] = Seq(pp:_*)
      // val srt = new SolverRuntimeDefinite(tolerance)
      // val mm = for(m <- solver.input.memory) yield srt.memoryallocfrom(m, spp)
      // val vv = solver.run[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]](srt, spp, Seq(ins:_*), mm)
      // new CvxSSolutionDefinite(spp, vv(0), srt.converge_iter_count, 0.0, tolerance)
    }
    def cgen(): CvxSSolverCGen = {
      throw new IRValidationException()
      // val srt = new SolverRuntimeCGen(solver.arity)
      // srt.setinputs(solver.input.args map ((a: Multi[AlmapShape]) =>
      //   InputDescCGen(
      //     a.dims.map((s: IRPoly) => ((b: Seq[IntSym]) => (s.eval(b)(srt.intlikei)))),
      //     ((b: Seq[IntSym]) => (a.body.domain.eval(b)(srt.intlikei))),
      //     ((b: Seq[IntSym]) => (a.body.codomain.eval(b)(srt.intlikei)))
      // )))
      // val mm = for(m <- solver.input.memory) yield srt.memoryallocfrom(m, srt.params)
      // val vv = solver.run(srt, srt.params, srt.inputs, mm)
      // srt.write_output(srt.vectorget(vv(0), solver.input.memory(0).body.eval(srt.params)(srt.intlikei), Seq()))
      // new CvxSSolverCGen(srt, solver.input.args.length, vv(0), solver.input.memory(0).body)
    }
  }

  class CvxSSolverCGen(val srt: SolverRuntimeCGen, val numinputs: Int, val vv: MemorySym, val vvsize: IRPoly) {
    def code: String = srt.code

    def compile(): CvxCSolverProgram = {
      val fout = new File("cgen/gen/out.c")
      val fwriter = new FileWriter(fout)
      fwriter.write(srt.code)
      fwriter.close()
      val cmd = "gcc --std=gnu99 -O3 -ffast-math -funroll-loops -o bin/cgen.out gen/out.c src/main.c -lm >log/gcc.o 2>log/gcc.e"
      println("[exec] " + cmd)
      val rt = java.lang.Runtime.getRuntime()
      val cproc = rt.exec(
        Array[String]("bash", "-c", cmd),
        null,
        new File("cgen"))
      val gccrv = cproc.waitFor()
      if(gccrv != 0) throw new Exception("Error in compiling generated source.")
      new CvxCSolverProgram(srt.arity, numinputs, vvsize)
    }
  }

  class CvxCSolverProgram(val arity: Int, val numinputs: Int, val vvsize: IRPoly) {
    def solve(params: Int*)(ins: MultiSeq[MatrixDefinite]*)(tolerance: Double): CvxSSolutionDefinite = {
      val pp = Seq(params:_*)
      if(pp.length != arity) throw new IRValidationException()
      if(ins.length != numinputs) throw new IRValidationException()
      val fin = new File("cgen/data/in.dat")
      val fwriter = new FileWriter(fin)
      for(a <- ins) {
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
      new CvxSSolutionDefinite(pp, MultiSeqA0(vv), iterct, timer, tolerance)
    }
  }

  implicit def definitescalar(a: Double): MultiSeq[MatrixDefinite] = {
    val amd = MatrixDefinite(1, 1, Seq(a))
    MultiSeqA0(amd)
  }

  implicit def definitevector(as: Seq[Double]): MultiSeq[MatrixDefinite] = {
    val amd = MatrixDefinite(as.length, 1, as)
    MultiSeqA0(amd)
  }

  implicit def definitematrix(as: Seq[Seq[Double]]): MultiSeq[MatrixDefinite] = {
    val m = as.length
    if(m < 1) throw new IRValidationException()
    val n = as(0).length
    if(n < 1) throw new IRValidationException()
    for(a <- as) {
      if(a.length != n) throw new IRValidationException()
    }
    val sas = as.foldLeft(Seq(): Seq[Double])((a,b) => a ++ b)
    val amd = MatrixDefinite(m, n, sas)
    MultiSeqA0(amd)
  }

  def definitematrixdiag(as: Seq[Double]): MultiSeq[MatrixDefinite] = {
    MultiSeqN(1, as map (a => MultiSeqA0(MatrixDefinite(1,1,Seq(a)))))
  }

  class CvxSSolutionDefinite(val pp: Seq[Int], val vv: MultiSeq[Seq[Double]], val itercount: Int, val timer: Double, val tolerance: Double) {
    def resolve(x: CvxExprSymbol): Seq[Double] = {
      val srt = new SolverRuntimeDefinite(tolerance)
      x.resolution.eval[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]](srt, pp, Seq(), Seq(vv))
    }
    def num_iterations: Int = itercount
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
    val s_inputsize = InputDesc(globalArity, s_inputs map (s => s.argdesc.shape), Seq())
    for(i <- 0 until s_inputs.length) {
      val shapei = s_inputs(i).argdesc.shape
      val dimsi = shapei.dims.size
      val Mi: Multi[Almap] = Multi(shapei.dims, 
        AlmapInput(s_inputsize.promoteBy(dimsi), i,
          for(j <- 0 until dimsi) yield IRPoly.param(s_params.length + j, s_params.length + dimsi)))
      s_inputs(i).symbol.bind(CvxInput(s_inputs(i).argdesc.proc(Mi)))
    }
    globalInputSize = s_inputsize
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
      b match {
        case bx: CvxLetExprBinding => bx.symbol.bindexpr(bx.expr)
        case bx: CvxLetInputBinding => bx.symbol.bind(bx.input)
        case _ => throw new IRValidationException()
      }
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
    var syms = s_over map (x => x.symbol)
    for(b <- s_let) {
      b match {
        case bx: CvxLetExprBinding => syms :+ bx.symbol
      }
    }
    for(s <- syms) {
      val x = s.boundexpr
      val msfx: Function = s_over.foldLeft(x.fx.expand(tmpfxn.varSize))((a,b) => a.minimize_over_lastarg)
      val sinput = InputDesc(msfx.arity, msfx.input.args, Seq(msfx.varSize))
      val sv = AVectorSum(
        msfx.valueOffset.addMemory(sinput.memory),
        msfx.valueVarAlmap.addMemory(sinput.memory).mmpy(AVectorRead(sinput, 0)))
      s.rset(sv)
    }

    new CvxSProblem(problem)
  }

}

