package cs320

object Implementation extends Template {

  // apply a binary numeric function on all the combinations of numbers from
  // the two input lists, and return the list of all the results
  def binOp(op: (Int, Int) => Int, ls: List[Int], rs: List[Int]): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(l, r)

      rs.map(f) ++ binOp(op, rest, rs)
  }

  def interp(expr: Expr): List[Int] = {
    def lookup(name: String, env: Map[String, List[Int]]): List[Int] = 
      env.getOrElse(name, error(s"free identifier: $name"))

    def bigger(n1: Int, n2: Int) = if (n1 < n2) n2 else n1
    def smaller(n1: Int, n2: Int) = if (n1 < n2) n1 else n2

    def interprete(expr: Expr, env: Map[String, List[Int]]): List[Int] = expr match {
      case Num(v) => v
      case Add(ls, rs) => binOp(_ + _, interprete(ls, env), interprete(rs, env))
      case Sub(ls, rs) => binOp(_ - _, interprete(ls, env), interprete(rs, env))
      case Val(x, es, body) => interprete(body, env ++ Map(x->interprete(es, env)))
      case Id(x) => lookup(x, env)
      case Min(e1, e2, e3) => {
        val es = binOp(smaller(_, _), interprete(e1, env), interprete(e2, env))
        binOp(smaller(_, _), es, interprete(e3, env))
      }
      case Max(e1, e2, e3) => {
        val es = binOp(bigger(_, _), interprete(e1, env), interprete(e2, env))
        binOp(bigger(_, _), es, interprete(e3, env))
      }
    }

    interprete(expr, Map[String, List[Int]]())
  }
}
