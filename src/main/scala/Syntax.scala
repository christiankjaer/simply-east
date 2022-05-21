enum Name {
  case Const(x: String)
  case Bound(n: Int)
  case Unquoted(n: Int)

  // Lifts a name into a value.
  def vpar: Value = Value.VNeutral(Neutral.NPar(this))

  def varPar(i: Int): InfTerm = this match
    case Name.Unquoted(k) => InfTerm.Var(i - k - 1)
    case _ => InfTerm.Par(this)

}

enum InfTerm {
  case Ann(e: CheckTerm, t: CheckTerm)
  case Star
  case Pi(e1: CheckTerm, e2: CheckTerm)
  case Var(x: Int)
  case Par(n: Name)
  case App(e1: InfTerm, e2: CheckTerm)
}

enum CheckTerm {
  case Inf(it: InfTerm)
  case Lam(body: CheckTerm)
  case Zero
  case Succ(t: CheckTerm)
}

enum Value {
  case VLam(f: Value => Value)
  case VStar
  case VPi(t: Value, f: Value => Value)
  case VNeutral(n: Neutral)

  case VNat
  case VZero
  case VSucc(v: Value)
}

enum Neutral {
  case NPar(x: Name)
  case NApp(n: Neutral, v: Value)
}

type Type = Value
