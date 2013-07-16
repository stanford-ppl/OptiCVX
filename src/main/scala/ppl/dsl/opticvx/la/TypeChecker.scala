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

  def betaSub(x: LExpr): LExpr = x match {
    case EIApp(EILambda(i, b), n) => isub(b, Map(i -> n))
    case ELambda(t, id, body) => {
      val u = betaSub(body)
      if(u == x) x else ELambda(t, id, u)
    }
    case EILambda(id, body) => {
      val u = betaSub(body)
      if(u == x) x else EILambda(id, u)
    }
    case EApp(f, a) => {
      val uf = betaSub(f)
      val ua = betaSub(a)
      if((uf == f)&&(ua == a)) x else EApp(uf, ua)
    }
    case EIApp(f, a) => {
      val uf = betaSub(f)
      if(uf == f) x else EIApp(uf, a)
    }
    case _ => x
  }

  def sub(x: LExpr, s: Map[Int, LExpr]): LExpr = x match {
    case EVar(id) => s.getOrElse(id, x)
    case ELambda(t, id, body) => {
      val u = sub(body, s - id)
      if(u == x) x else ELambda(t, id, u)
    }
    case EILambda(id, body) => {
      val u = sub(body, s)
      if(u == x) x else EILambda(id, u)
    }
    case EApp(f, a) => {
      val uf = sub(f, s)
      val ua = sub(a, s)
      if((uf == f)&&(ua == a)) x else EApp(uf, ua)
    }
    case EIApp(f, a) => {
      val uf = sub(f, s)
      if(uf == f) x else EIApp(uf, a)
    }
    case _ => x
  }

  def isub(x: LExpr, s: Map[Int, LIExpr]): LExpr = x match {
    case ELambda(t, id, body) => {
      val u = isub(body, s)
      if(u == x) x else ELambda(t, id, u)
    }
    case EILambda(id, body) => {
      val u = isub(body, s - id)
      if(u == x) x else EILambda(id, u)
    }
    case EApp(f, a) => {
      val uf = isub(f, s)
      val ua = isub(a, s)
      if((uf == f)&&(ua == a)) x else EApp(uf, ua)
    }
    case EIApp(f, a) => {
      val uf = isub(f, s)
      val ua = isub(a, s)
      if((uf == f)&&(ua == a)) x else EIApp(uf, ua)
    }
    case _ => x
  }

  def isub(x: LIExpr, s: Map[Int, LIExpr]): LIExpr = x match {
    case IVar(id) => s.getOrElse(id, x)
    case IAdd(a, b) =>  {
      val ua = isub(a, s)
      val ub = isub(b, s)
      if((ua == a)&&(ub == b)) x else IAdd(ua, ub)
    }
    case IMult(a, b) => {
      val ua = isub(a, s)
      val ub = isub(b, s)
      if((ua == a)&&(ub == b)) x else IMult(ua, ub)
    }
    case ISxp(a, e) => {
      val ua = isub(a, s)
      if(ua == a) x else ISxp(ua, e)
    }
    case _ => x
  }
}
