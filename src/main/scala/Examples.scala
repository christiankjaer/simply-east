object Examples {

  import CheckTerm.*
  import InfTerm.*
  import Name.*
  import Value.*
  import Neutral.*

  val id: InfTerm = Ann(
    Lam(Lam(Inf(Var(0)))),
    Inf(Pi(Inf(Star), Inf(Pi(Inf(Var(0)), Inf(Var(1))))))
  )

  val ctx: Context = Context.empty
    .extend(Const("Bool"), VStar)
    .extend(Const("False"), VNeutral(NPar(Const("Bool"))))

  val app1: InfTerm = App(id, Inf(Par(Const("Bool"))))
  val app2: InfTerm = App(app1, Inf(Par(Const("False"))))

}
