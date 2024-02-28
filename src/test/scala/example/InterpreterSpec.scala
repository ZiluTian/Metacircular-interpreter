package selfInterpretation

import Interpreter._

class InterpreterSpec extends munit.FunSuite {
  test("Condition") {
    assertEquals(intp(`@`(`@`(`@`(Cond, Zero), `@`(Succ, Zero)), `@`(Succ, `@`(Succ, Zero)))), `@`(Succ, Zero))
  }

  test("S combinator") {
    assertEquals(intp(`@`(`@`(`@`(S, Zero), Zero), Succ)), `@`(`@`(Zero,Succ),`@`(Zero,Succ)))
    println(intp(`@`(`@`(K, Zero), `@`(Succ, Zero))))
  }

  test("K combinator") {
    assertEquals(intp(`@`(`@`(K, Zero), `@`(Succ, Zero))), Zero)
  }

  test("Reification") {
    assertEquals(intp(`@`(Reif, `@`(`@`(`@`(Cond, Zero), `@`(Succ, Zero)), `@`(Succ, `@`(Succ, Zero))))), `@`(Reif, `@`(`@`(`@`(Cond, Zero), `@`(Succ, Zero)), `@`(Succ, `@`(Succ, Zero)))))
  }

  test("Eval") {
    assertEquals(intp(`@`(Eval, `@`(Reif, `@`(`@`(`@`(Cond, Zero), `@`(Succ, Zero)), `@`(Succ, `@`(Succ, Zero)))))), `@`(Reif, `@`(Succ, Zero)))
  }

  test("Refl") {
    assertEquals(intp(`@`(Refl, `@`(Reif, `@`(`@`(`@`(Cond, Zero), `@`(Succ, Zero)), `@`(Succ, `@`(Succ, Zero)))))), `@`(Succ, Zero))
  }
}
