package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import ppl.dsl.opticvx.solvers._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set


trait DCPOpsSolve extends DCPOpsFunction {

  class CvxSolveParams(val params: Seq[CvxSolveParamBinding])
  class CvxSolveParamBinding(val binding: Int, val symbol: CvxParamSymbol)

  class CvxSolveInputs(val inputs: Seq[CvxSolveInputBinding])
  class CvxSolveInputBinding(val argdesc: InputArgDesc, val symbol: CvxInputSymbol)

  def params(ps: CvxSolveParamBinding*): CvxSolveParams = new CvxSolveParams(Seq(ps:_*))
  implicit def cvxsolveparambindingimpl(tpl: Tuple2[Int, CvxParamSymbol]): CvxSolveParamBinding =
    new CvxSolveParamBinding(tpl._1, tpl._2)

  def given(bs: CvxSolveInputBinding*): CvxSolveInputs = new CvxSolveInputs(Seq(bs:_*))

  def solve(
    ts_params: =>CvxSolveParams,
    ts_inputs: =>CvxSolveInputs,
    ts_over: =>CvxOver,
    ts_let: =>CvxLet,
    ts_where: =>CvxWhere,
    ts_value: =>CvxValue
    ) =
  {
    // bind the parameters
    val s_params: Seq[CvxSolveParamBinding] = ts_params.params
    for(i <- 0 until s_params.length) {
      s_params(i).symbol.bind(IRPoly.param(i, s_params.length))
    }
    globalArity = s_params.length
    // bind the inputs
    val s_inputs: Seq[CvxSolveInputBinding] = ts_inputs.inputs
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

    val tt = PrimalDualOperatorSplitting.gen(problem)
    val pp = s_params map (s => s.binding)
    val srt = SolverRuntimeDefinite
    val mm = for(m <- tt.input.memory) yield srt.memoryallocfrom(m, pp)
    val vv = tt.run[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]](srt, pp, Seq(), mm)
    val syms = (s_over map (x => x.symbol)) ++ (s_let map (x => x.symbol))
    for(s <- syms) {
      val x = s.boundexpr
      val msfx: Function = s_over.foldLeft(x.fx.expand(tmpfxn.varSize))((a,b) => a.minimize_over_lastarg)
      val sinput = InputDesc(msfx.arity, msfx.input.args, Seq(tt.input.memory(0)))
      val sv = AVectorSum(
        msfx.valueOffset.addMemory(sinput.memory),
        msfx.valueVarAlmap.addMemory(sinput.memory).mmpy(AVectorRead(sinput, 0, Seq())))
      s.rset(sv.eval[Int, MatrixDefinite, MultiSeq[MatrixDefinite], Seq[Double], MultiSeq[Seq[Double]]](srt, pp, Seq(), Seq(vv(0))))
    }
  }

}

