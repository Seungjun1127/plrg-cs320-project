package cs320

object Implementation extends Template {

  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a * b * c

  def concat(x: String, y: String): String = x + y

  def addN(n: Int): Int => Int = _ + n

  def twice(f: Int => Int): Int => Int = x => f(f(x))

  def compose(f: Int => Int, g: Int => Int): Int => Int = x => f(g(x))

  def double(l: List[Int]): List[Int] = l.map(2*_)

  def sum(l: List[Int]): Int = l.foldRight(0)(_+_)

  def getKey(m: Map[String, Int], s: String): Int = m.getOrElse(s, error(s"no such key $s"))

  def countLeaves(t: Tree): Int = t match {
    case Leaf(_) => 1
    case Branch(l, v, r) => countLeaves(l) + countLeaves(r)
  }

  def flatten(t: Tree): List[Int] = t match {
    case Leaf(v) => List(v)
    case Branch(l, v, r) => flatten(l) ++ (v :: flatten(r))
  }
}
