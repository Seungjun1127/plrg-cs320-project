package cs320

object Implementation extends Template {
  def freeIds(expr: Expr): Set[String] = expr match {
    case Num(value) => Set()
    case Add(left, right) => freeIds(left)|freeIds(right)
    case Sub(left, right) => freeIds(left)|freeIds(right)
    case Id(id) => Set(id)
    case Val(name, expr, body) => freeIds(expr)|freeIds(body)-name
  }

  def bindingIds(expr: Expr): Set[String] = expr match {
    case Num(value) => Set()
    case Add(left, right) => bindingIds(left)|bindingIds(right)
    case Sub(left, right) => bindingIds(left)|bindingIds(right)
    case Id(id) => Set()
    case Val(name, expr, body) => bindingIds(expr)|bindingIds(body)+name
  }

  def boundIds(expr: Expr): Set[String] = expr match {
    case Num(value) => Set()
    case Add(left, right) => boundIds(left)|boundIds(right)
    case Sub(left, right) => boundIds(left)|boundIds(right)
    case Id(id) => Set()
    case Val(name, expr, body) => boundIds(expr)|boundIds(body)|(freeIds(body)&Set(name))
  }
}
