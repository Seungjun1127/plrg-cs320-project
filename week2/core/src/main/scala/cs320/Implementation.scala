package cs320

object Implementation extends Template {

  def freeIds(expr: Expr): Set[String] = {
    def free_recursive(body: Expr, env: Set[String]): Set[String] = body match {
      case Num(_) => Set()
      case Add(l, r) => free_recursive(l, env) ++ free_recursive(r, env)
      case Sub(l, r) => free_recursive(l, env) ++ free_recursive(r, env)
      case Id(x) => if (env.contains(x)) Set() else Set(x)
      case Val(x, e1, e2) => free_recursive(e1, env) ++ free_recursive(e2, env ++ Set(x))
    }
    free_recursive(expr, Set())
  }

  def bindingIds(expr: Expr): Set[String] = expr match {
    case Num(_) => Set()
    case Add(l, r) => bindingIds(l) ++ bindingIds(r)
    case Sub(l, r) => bindingIds(l) ++ bindingIds(r)
    case Id(x) => Set()
    case Val(x, e1, e2) => bindingIds(e1) ++ Set(x) ++ bindingIds(e2)
  }

  def boundIds(expr: Expr): Set[String] = {
    def bound_recursive(body: Expr, env: Set[String]): Set[String] = body match {
      case Num(_) => Set()
      case Add(l, r) => bound_recursive(l, env) ++ bound_recursive(r, env)
      case Sub(l, r) => bound_recursive(l, env) ++ bound_recursive(r, env)
      case Id(x) => if (env.contains(x)) Set(x) else Set()
      case Val(x, e1, e2) => bound_recursive(e1, env) ++ bound_recursive(e2, env ++ Set(x))
    }
    bound_recursive(expr, Set())
  }
}
