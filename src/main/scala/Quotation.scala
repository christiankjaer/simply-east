object Quotation {

  def quote0(v: Value): CheckTerm = quote(0, v)

  def quote(i: Int, v: Value): CheckTerm =
    v match
      case Value.VLam(f) => CheckTerm.Lam(quote(i + 1, f(Name.Unquoted(i).vpar)))
      case Value.VNeutral(n) => CheckTerm.Inf(neutralQuote(i, n))
      case Value.VStar => CheckTerm.Inf(InfTerm.Star)
      case Value.VPi(t, f) =>
        CheckTerm.Inf(InfTerm.Pi(quote(i, t), quote(i + 1, f(Name.Unquoted(i).vpar))))

  def neutralQuote(i: Int, n: Neutral): InfTerm =
    n match
      case Neutral.NPar(x) => x.varPar(i)
      case Neutral.NApp(n, v) => InfTerm.App(neutralQuote(i, n), quote(i, v))

}
