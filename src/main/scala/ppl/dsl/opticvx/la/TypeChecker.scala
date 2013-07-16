package ppl.dsl.opticvx.la


object TypeChecker {
  def typeOf(x: LExpr): LType = typeOf(x, Map())

  def typeOf(x: LExpr, env: Map[Int, LType]): LType = x match {
    case EVar(id) => env.getOrElse(id, throw new Exception("unbound symbol x" + id.toString))
    case EConst(d) => TVector(IConst(1))
    case ELambda(t, id, body) => TArr(t, typeOf(body, env + (id -> t)))
    case EILambda(id, body) => TILambda(id, typeOf(body, env))
    case EApp(fx, arg) => {
      val tfx = typeOf(fx, env)
      val ta = typeOf(arg, env)
      tfx match {
        case TArr(tl, tr) if(tl == ta) => tr
        case _ => throw new Exception("invalid function application")
      }
    }
    case EIApp(fx, arg) => TIApp(typeOf(fx, env), arg)
    case ep: EPrimitive => ep.ltype
  }
}
