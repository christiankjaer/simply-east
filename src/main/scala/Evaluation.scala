object Evaluation {

  type Env = List[Value]

  def evalInf(t: InfTerm, env: Env): Value = t match
    case InfTerm.Ann(e, _) => evalCheck(e, env)
    case InfTerm.Star => Value.VStar
    case InfTerm.Pi(e1, e2) => Value.VPi(evalCheck(e1, env), v => evalCheck(e2, v :: env))
    case InfTerm.Var(x) => env(x)
    case InfTerm.Par(n) => n.vpar
    case InfTerm.App(e1, e2) =>
      val v = evalCheck(e2, env)
      evalInf(e1, env) match
        case Value.VLam(l) => l(v)
        case Value.VNeutral(n) => Value.VNeutral(Neutral.NApp(n, v))
        case v => v // Impossible case

  def evalCheck(t: CheckTerm, env: Env): Value = t match
    case CheckTerm.Inf(it) => evalInf(it, env)
    case CheckTerm.Lam(body) => Value.VLam(x => evalCheck(body, x :: env))
}
