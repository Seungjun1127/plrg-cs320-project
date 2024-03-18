package cs320

import Value._

object Implementation extends Template {
  def num(op: (Int,Int) => Int, left: Expr, right: Expr, env: Map[String, Value]) =
  (interp(left, env), interp(right, env)) match {
    case (NumV(ln), NumV(rn)) => NumV(op(ln, rn))
    case (lv, rv) => error(s"wrong operator $op with $lv and $rv")
  }

  def interp(expr: Expr, env: Map[String, Value]): Value = expr match {
    case Num(n) => NumV(n)
    case Add(l, r) => num(_ + _, l, r, env)
    case Sub(l, r) => num(_ - _, l, r, env) 
    case Val(name, v, b) => interp(b, env + (name->interp(v, env)))
    case Id(name) => env.getOrElse(name, error(s"free identifier: $name"))

    case App(func: Expr, args: List[Expr]) => interp(func, env) match {
      case CloV(params, body, fenv) => 
        if (params.length == args.length) 
          interp(body, fenv ++ params.zip(args).map{case (name, argexpr) => name -> interp(argexpr, env)})
        else error(s"wrong arity. params: $params args: $args")
      case v => error(s"not a closure: $v")
    }
    case Fun(params, body) => CloV(params, body, env)

    case Rec(rec) => RecV(rec.map{case (name, recexpr) => name -> interp(recexpr, env)})

    case Acc(expr, name) => interp(expr, env) match {
      case RecV(rec) => rec.getOrElse(name, error(s"no such field: $name"))
      case v => error(s"not a record: $v")
    }
  }

  def interp(expr: Expr): Value = interp(expr, Map());
}