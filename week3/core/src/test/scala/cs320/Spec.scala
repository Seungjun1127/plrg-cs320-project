package cs320

import Macros._

class Spec extends SpecBase {

  val run = Implementation.run _

  test(run("5"), List(5))
  test(run("(5 + 5)"), List(10))
  test(run("{ val x = (5 + 5); (x + x) }"), List(20))
  test(run("{ val x = 5; (x + x) }"), List(10))
  test(run("{ val x = (5 + 5); { val y = (x - 3); (y + y) } }"), List(14))
  test(run("{ val x = 5; { val y = (x - 3); (y + y) } }"), List(4))
  test(run("{ val x = 5; (x + { val x = 3; 10 }) }"), List(15))
  test(run("{ val x = 5; (x + { val x = 3; x }) }"), List(8))
  test(run("{ val x = 5; (x + { val y = 3; x }) }"), List(10))
  test(run("{ val x = 5; { val y = x; y } }"), List(5))
  test(run("{ val x = 5; { val x = x; x } }"), List(5))
  test(run("{ val x = 2; ((x + x) - x) }"), List(2))
  test(run("(3 + 7)"), List(10))
  test(run("(10 - (3, 5))"), List(7, 5))
  test(run("min(3, 4, 5)"), List(3))
  test(run("max((1 + 2), 4, 5)"), List(5))
  test(run("(min(9, 3, 7) + max(6, 2, 20))"), List(23))
  test(run("((1, 2) + (3, 4))"), List(4, 5, 5, 6))
  test(run("(((1, 2) + (3, 4)) - (1, 2))"), List(3, 2, 4, 3, 4, 3, 5, 4))
  test(run("((10, 2, 1) - (3, 2))"), List(7, 8, -1, 0, -2, -1))
  test(run("{ val x = (1, 2); (x + (4, 3)) }"), List(5, 4, 6, 5))
  test(run("{ val x = 9; (x + { val x = 3; x }) }"), List(12))
  test(run("{ val x = 100; (x + { val y = 3; x }) }"), List(200))
  test(run("{ val x = (7, 5); (x + x) }"), List(14, 12, 12, 10))
  test(run("(min(3, 5, 7) + min(10, 100, 1000))"), List(13))
  test(run("{ val x = 10; max(x, 2, 3) }"), List(10))
  test(run("{ val x = 20; { val y = 5; { val z = (10, 20); (z + max((x + y), 0, 12)) } } }"), List(35, 45))
  test(run("{ val x = 20; { val y = 5; { val z = (10, 20); (z + min((x + y), 0, 12)) } } }"), List(10, 20))
  test(run("{ val x = min(3, 9, 5); { val y = (x - 3); y } }"), List(0))
  test(run("{ val x = max(2, 3, 5); min(x, 7, 6) }"), List(5))
  test(run("{ val x = max(9, 7, 10); max(8, x, (1 + x)) }"), List(11))
  test(run("(min(6, 4, 5) - max(2, 3, 4))"), List(0))
  test(run("{ val x = (7 + 2); min(x, 7, 0) }"), List(0))
  test(run("{ val x = (7, 2); min(x, 7, 0) }"), List(0, 0))
  test(run("{ val x = (13); min(x, 1, 12) }"), List(1))
  test(run("{ val x = min(2, 1, 3); (x + x) }"), List(2))
  test(run("{ val a = 10; { val b = 19; { val c = 2; min(a, b, c) } } }"), List(2))
  test(run("{ val x = 3; max(3, 4, (x + x)) }"), List(6))
  test(run("{ val a = 10; { val b = 19; { val c = 2; max(a, b, c) } } }"), List(19))
  test(run("{ val x = min(2, 5, 4); (x + x) }"), List(4))
  test(run("{ val x = max(2, 5, 4); (x + x) }"), List(10))
  test(run("{ val x = (11 - 3); max(x, (x + x), (x - x)) }"), List(16))
  test(run("{ val x = (11 - 3); min(x, (x + x), (x - x)) }"), List(0))
  test(run("min((4 + 4), { val x = 5; (x + { val x = 3; 10 }) }, 3)"), List(3))
  test(run("max((4 + 4), { val x = 5; (x + { val x = 3; 10 }) }, 3)"), List(15))
  test(run("{ val x = (10); max(x, 2, 3) }"), List(10))
  test(run("{ val x = max(2, 1, 3); (x + x) }"), List(6))
  test(run("{ val x = 2; min(x, 3, 10) }"), List(2))
  test(run("{ val x = 2; max(x, 3, 10) }"), List(10))
  test(run("min((4 + 4), 2, 3) "), List(2))
  test(run("max((4 + 4), 2, 3) "), List(8))
  test(run("{ val x = 10; min(x, 2, 3) }"), List(2))
  test(run("min((3 + 4), 5, 6)"), List(5))
  test(run("max((3 + 4), 5, 6)"), List(7))
  test(run("{ val x = (10); min(x, (3), (5)) }"), List(3))
  test(run("{ val x = (10); max(x, (3), (5)) }"), List(10))
  test(run("min((3), 4, 5)"), List(3))
  test(run("max((3), 4, (5))"), List(5))
  test(run("((10, 100, 1000, 10000) + min((3 - 4), 5, 6))"), List(9, 99, 999, 9999))

  testExc(run("{ val x = 1; y }"), "free identifier")
  testExc(run("{ val x = 5; (y + { val y = 3; 10 }) }"), "free identifier")

  /* Write your own tests */
}
