package ppl.dsl.opticvx.la

import ppl.dsl.opticvx.common.IRValidationException


object PrettyPrint {
  case class pr(val str: String, pri: Int) {
    def ap(opri: Int): String = {
      if(opri >= pri) {
        str
      }
      else {
        "(" + str + ")"
      }
    }
  }

  def pprint(x: LExpr): String = pfmt(x).str
  def pprint(x: LType): String = pfmt(x).str
  def pprint(x: LIExpr): String = pfmt(x).str

  def pfmt(x: LExpr): pr = x match {
    case EVar(id) => pr("x" + id.toString, 0)
    case EConst(d) => pr(d.toString, 0)
    case ELambda(t, id, body) => pr("λ x" + id.toString + ": " + pfmt(t).str + ". " + pfmt(body).ap(4), 4)
    case EILambda(id, body) => pr("Λ n" + id.toString + ". " + pfmt(body).ap(4), 4)
    case EApp(fx, arg) => pr(pfmt(fx).ap(2) + " " + pfmt(arg).ap(1), 2)
    case EIApp(fx, arg) => pr(pfmt(fx).ap(1) + "[" + pprint(arg) + "]", 1)
    case ep: EPrimitive => pr(ep.name, 0)
  }

  def pfmt(x: LType): pr = x match {
    case TVector(len) => pr("V[" + pprint(len) + "]", 0)
    case TArr(l, r) => pr(pfmt(l).ap(1) + " ——> " + pfmt(r).ap(2), 2)
    case TILambda(id, body) => pr("Λ n" + id.toString + ". " + pfmt(body).ap(4), 4)
    case TIApp(fx, arg) => pr(pfmt(fx).ap(1) + "[" + pprint(arg) + "]", 1)
  }

  def pfmt(x: LIExpr): pr = x match {
    case IConst(i) => pr(i.toString, 0)
    case IVar(id) => pr("n" + id.toString, 0)
    case IAdd(a, b) =>  pr(pfmt(a).ap(2) + " + " + pfmt(b).ap(3), 3)
    case IMult(a, b) => pr(pfmt(a).ap(1) + " * " + pfmt(b).ap(2), 2)
    case ISxp(a, e) => pr(pfmt(a).str + "@" + e.toString, 1)
  }
}
