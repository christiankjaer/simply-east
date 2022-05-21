opaque type Context = List[(Name, Type)]

object Context {

  def empty: Context = List.empty

  extension (c: Context) {
    def lookup(n: Name): Option[Type] =
      c.find(_._1 == n).map(_._2)

    def extend(n: Name, t: Type): Context =
      (n -> t) :: c

  }
}
