package ppl.dsl.opticvx.dcp

import ppl.dsl.opticvx.common._
import ppl.dsl.opticvx.model._
import scala.collection.immutable.Seq
import scala.collection.immutable.Set


trait DCPOpsFunction extends DCPOpsExpr {

  class CvxParams(val params: Seq[CvxParamSymbol])

  class CvxInputs(val inputs: Seq[CvxInputBinding])
  trait CvxInputDesc {
    val shape: Multi[AlmapShape]
    def proc(m: Multi[Almap]): Multi[Almap] 
  }
  class CvxInputBinding(val argdesc: CvxInputDesc, val symbol: CvxInputSymbol)

  class CvxArgs(val args: Seq[CvxArgBinding])
  class CvxArgBinding(val size: IRPoly, val symbol: CvxExprSymbol)

  class CvxSign(val sign: SignumPoly)

  class CvxTonicity(val tonicity: Seq[SignumPoly])

  class CvxVexity(val vexity: SignumPoly)

  class CvxOver(val vars: Seq[CvxOverBinding])
  class CvxOverBinding(val size: IRPoly, val symbol: CvxExprSymbol)

  class CvxLet(val exprs: Seq[CvxLetBinding])
  trait CvxLetBinding
  class CvxLetExprBinding(val expr: CvxExpr, val symbol: CvxExprSymbol) extends CvxLetBinding
  class CvxLetInputBinding(val input: CvxInput, val symbol: CvxInputSymbol) extends CvxLetBinding

  class CvxWhere(val constraints: Seq[CvxConstraint])

  trait CvxValue
  class CvxMinimize(val expr: CvxExpr) extends CvxValue
  class CvxMaximize(val expr: CvxExpr) extends CvxValue

  def params(ps: CvxParamSymbol*): CvxParams = new CvxParams(Seq(ps:_*))

  def given(bs: CvxInputBinding*): CvxInputs = new CvxInputs(Seq(bs:_*))
  implicit def inputbindingimpl(tpl: Tuple2[CvxInputDesc, CvxInputSymbol]): CvxInputBinding = 
    new CvxInputBinding(tpl._1, tpl._2)

  def args(as: CvxArgBinding*): CvxArgs = new CvxArgs(Seq(as:_*))
  implicit def argbindingimpl(tpl: Tuple2[IRPoly, CvxExprSymbol]): CvxArgBinding =
    new CvxArgBinding(tpl._1, tpl._2)

  def sign(s: SignumPoly): CvxSign = new CvxSign(s)

  def tonicity(ts: SignumPoly*): CvxTonicity = new CvxTonicity(Seq(ts:_*))

  def vexity(v: SignumPoly): CvxVexity = new CvxVexity(v)

  def over(vs: CvxOverBinding*): CvxOver = new CvxOver(Seq(vs:_*))
  implicit def overbindingimpl(tpl: Tuple2[IRPoly, CvxExprSymbol]): CvxOverBinding = 
    new CvxOverBinding(tpl._1, tpl._2)

  def let(xs: CvxLetBinding*): CvxLet = new CvxLet(Seq(xs:_*))
  implicit def letexprbindingimpl(tpl: Tuple2[CvxExpr, CvxExprSymbol]): CvxLetBinding =
    new CvxLetExprBinding(tpl._1, tpl._2)
  implicit def letinputbindingimpl(tpl: Tuple2[CvxInput, CvxInputSymbol]): CvxLetBinding =
    new CvxLetInputBinding(tpl._1, tpl._2)

  def where(xs: CvxConstraint*): CvxWhere = new CvxWhere(Seq(xs:_*))

  def minimize(x: CvxExpr): CvxValue = {
    if(x.size != IRPoly.const(1, globalArity)) throw new IRValidationException()
    val v: Signum = x.fx.vexity.reduce
    if(!(v <= Signum.Positive)) {
      println(v)
      throw new IRValidationException()
    }
    new CvxMinimize(x)
  }
  def maximize(x: CvxExpr): CvxValue = {
    if(x.size != IRPoly.const(1, globalArity)) throw new IRValidationException()
    val v: Signum = x.fx.vexity.reduce
    if(!(v <= Signum.Negative)) {
      println(v)
      throw new IRValidationException()
    }
    new CvxMaximize(x)
  }

  def positive: SignumPoly = SignumPoly.const(Signum.Positive, globalSignumArity)
  def nonnegative: SignumPoly = SignumPoly.const(Signum.Positive, globalSignumArity)
  def negative: SignumPoly = SignumPoly.const(Signum.Negative, globalSignumArity)
  def nonpositive: SignumPoly = SignumPoly.const(Signum.Negative, globalSignumArity)
  def zero: SignumPoly = SignumPoly.const(Signum.Zero, globalSignumArity)
  def none: SignumPoly = SignumPoly.const(Signum.All, globalSignumArity)

  implicit def double2cvxfunexprimpl(c: Double): CvxExpr = 
    CvxExpr(Function.const(AVector.const(c, globalInputSize), globalInputSize, globalArgSize))
  implicit def int2cvxfunexprimpl(i: Int): CvxExpr = 
    double2cvxfunexprimpl(i.toDouble)


  class CvxInputDescFlat(val shape: Multi[AlmapShape]) extends CvxInputDesc {
    def proc(m: Multi[Almap]): Multi[Almap] = m
  }

  class CvxInputDescFor(val len: IRPoly, val cxfx: CvxInputDesc) extends CvxInputDesc {
    val shape: Multi[AlmapShape] = Multi(cxfx.shape.dims :+ len, cxfx.shape.body)
    def proc(m: Multi[Almap]): Multi[Almap] = cxfx.proc(m)
  }

  def inputscalar: CvxInputDesc = 
    new CvxInputDescFlat(Multi(Seq(), AlmapShape(IRPoly.const(1, globalArity), IRPoly.const(1, globalArity))))

  def inputvector(n: IRPoly): CvxInputDesc = {
    if(n.arity != globalArity) throw new IRValidationException()
    new CvxInputDescFlat(Multi(Seq(), AlmapShape(IRPoly.const(1, globalArity), n)))
  }

  def inputmatrix(m: IRPoly, n: IRPoly): CvxInputDesc = {
    if(m.arity != globalArity) throw new IRValidationException()
    if(n.arity != globalArity) throw new IRValidationException()
    new CvxInputDescFlat(Multi(Seq(), AlmapShape(m, n)))
  }

  def inputfor(len: IRPoly)(fx: (IRPoly) => CvxInputDesc): CvxInputDesc = {
    if(len.arity != globalArity) throw new IRValidationException()
    globalArityPromote()
    val cxfx = fx(len.next)
    globalArityDemote()
    new CvxInputDescFor(len, cxfx)
  }

  class CvxInputDescDiag(val n: IRPoly) extends CvxInputDesc {
    val shape: Multi[AlmapShape] = Multi(Seq(n), AlmapShape(IRPoly.const(1, n.arity + 1), IRPoly.const(1, n.arity + 1)))
    def proc(m: Multi[Almap]): Multi[Almap] = {
      if(m.dims != Seq(n)) throw new IRValidationException()
      Multi(Seq(), AlmapDiagCatFor(n, m.body))
    }
  }

  def inputmatrixdiag(n: IRPoly): CvxInputDesc = {
    if(n.arity != globalArity) throw new IRValidationException()
    new CvxInputDescDiag(n)
  }


  def cvxfun(
    ts_params: =>CvxParams,
    ts_inputs: =>CvxInputs,
    ts_args: =>CvxArgs,
    ts_sign: =>CvxSign,
    ts_tonicity: =>CvxTonicity,
    ts_vexity: =>CvxVexity,
    ts_over: =>CvxOver,
    ts_let: =>CvxLet,
    ts_where: =>CvxWhere,
    ts_value: =>CvxValue
    ): Function =
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
    // bind the arguments
    val s_args: Seq[CvxArgBinding] = ts_args.args
    for(i <- 0 until s_args.length) {
      s_args(i).symbol.bindsign(SignumPoly.param(i, s_args.length))
    }
    // gather the dcp information for the function
    globalSignumArity = s_args.length
    val s_sign: SignumPoly = ts_sign.sign
    val s_tonicity: Seq[SignumPoly] = ts_tonicity.tonicity
    val s_vexity: SignumPoly = ts_vexity.vexity
    globalSignumArity = -1
    // get the variables
    val s_over = ts_over.vars
    val s_argsize: Seq[IRPoly] = (s_args map (x => x.size)) ++ (s_over map (x => x.size))
    globalArgSize = s_argsize
    // rebind the arguments
    for(i <- 0 until s_args.length) {
      s_args(i).symbol.releasesign()
      s_args(i).symbol.bindexpr(CvxExpr(Function.param(i, s_inputsize, s_argsize)))
    }
    // bind the variables
    for(i <- 0 until s_over.length) {
      s_over(i).symbol.bindexpr(CvxExpr(Function.param(i + s_args.length, s_inputsize, s_argsize)))
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
    globalArgSize = null
    globalInputSize = null
    // make the return value
    val tmpfxn = (s_value match {
      case x: CvxMinimize => s_where.foldLeft(x.expr.fx)((a,b) => a + b.fx)
      case x: CvxMaximize => s_where.foldLeft(x.expr.fx)((a,b) => a - b.fx)
      case _ => throw new IRValidationException()
    })
    val minfxn = s_over.foldLeft(tmpfxn)((a,b) => a.minimize_over_lastarg).simplify

    minfxn.chdcp(s_sign, s_tonicity, s_vexity)
  }
}