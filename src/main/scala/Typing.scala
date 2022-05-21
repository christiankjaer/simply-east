import Evaluation.evalCheck

object Typing {

  def typeInf0(ctx: Context, t: InfTerm): Either[String, Type] = typeInf(0, ctx, t)

  def typeInf(n: Int, ctx: Context, t: InfTerm): Either[String, Type] =
    t match
      case InfTerm.Ann(e, t) =>
        for {
          _ <- typeCheck(n, ctx, t, Value.VStar)
          v = evalCheck(t, List.empty)
          _ <- typeCheck(n, ctx, e, v)
        } yield v

      case InfTerm.Par(x) =>
        ctx.lookup(x) match
          case Some(t) => Right(t)
          case _ => Left(s"unknown identifier: $x")

      case InfTerm.Var(_) => Left("impossible case")
      case InfTerm.Star => Right(Value.VStar)
      case InfTerm.Pi(t1, t2) =>
        for {
          _ <- typeCheck(n, ctx, t1, Value.VStar)
          v = evalCheck(t1, List.empty)
          _ <- typeCheck(
            n + 1,
            ctx.extend(Name.Bound(n), v),
            substCheck(0, InfTerm.Par(Name.Bound(n)), t2),
            Value.VStar
          )
        } yield Value.VStar

      case InfTerm.App(e1, e2) =>
        for {
          t1 <- typeInf(n, ctx, e1)
          t2 <- t1 match
            case Value.VPi(t, f) =>
              typeCheck(n, ctx, e2, t).map(_ => f(evalCheck(e2, List.empty)))
            case _ => Left("illegal application")
        } yield t2

  def typeCheck(i: Int, ctx: Context, ct: CheckTerm, t: Type): Either[String, Unit] =
    ct match
      case CheckTerm.Inf(it) =>
        typeInf(i, ctx, it).flatMap(t2 =>
          val qt1 = Quotation.quote0(t)
          val qt2 = Quotation.quote0(t2)
          if qt1 == qt2
          then Right(())
          else Left(s"type mismatch: $qt1 != $qt2")
        )
      case CheckTerm.Lam(body) =>
        t match
          case Value.VPi(v, f) =>
            typeCheck(
              i + 1,
              ctx.extend(Name.Bound(i), v),
              substCheck(0, InfTerm.Par(Name.Bound(i)), body),
              f(Name.Bound(i).vpar)
            )
          case _ => Left("type mismatch. Pi type expected")

  def substCheck(i: Int, r: InfTerm, in: CheckTerm): CheckTerm =
    in match
      case CheckTerm.Inf(it) => CheckTerm.Inf(substInf(i, r, it))
      case CheckTerm.Lam(body) => CheckTerm.Lam(substCheck(i + 1, r, body))

  def substInf(i: Int, r: InfTerm, in: InfTerm): InfTerm =
    in match
      case InfTerm.Ann(e, t) => InfTerm.Ann(substCheck(i, r, e), t)
      case InfTerm.Var(j) => if i == j then r else InfTerm.Var(j)
      case InfTerm.Par(x) => InfTerm.Par(x)
      case InfTerm.App(e1, e2) => InfTerm.App(substInf(i, r, e1), substCheck(i, r, e2))
      case InfTerm.Star => InfTerm.Star
      case InfTerm.Pi(t1, t2) => InfTerm.Pi(substCheck(i, r, t1), substCheck(i + 1, r, t2))
}
