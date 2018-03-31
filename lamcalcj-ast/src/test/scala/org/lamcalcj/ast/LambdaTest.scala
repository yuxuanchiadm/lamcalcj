package org.lamcalcj.ast

import org.scalatest.FunSpec
import org.lamcalcj.ast.Lambda._

class LambdaTest extends FunSpec {
  describe("Basic terms construction") {
    it("Should compiles") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assertCompiles("Var(id_x)")
      assertCompiles("Abs(Var(id_x), Abs(Var(id_y), App(Var(id_x), Var(id_y))))")
      assertCompiles("Abs { Var(id_x) } { Var(id_x) }")
    }
  }
  describe("Equality for terms") {
    it("Should have trivial equality for basic terms") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(Var(id_x) == Var(id_x))
      assert(Var(id_x) != Var(id_y))
      assert(Abs(Var(id_x), Var(id_x)) == Abs(Var(id_x), Var(id_x)))
      assert(Abs(Var(id_x), Var(id_x)) != Abs(Var(id_y), Var(id_y)))
      assert(App(Var(id_x), Var(id_x)) == App(Var(id_x), Var(id_x)))
      assert(App(Var(id_x), Var(id_x)) != App(Var(id_y), Var(id_y)))
    }
    it("Should not have equality for different identifier") {
      val id_x0: Identifier = Identifier("x")
      val id_x1: Identifier = Identifier("x")
      assert(id_x0 != id_x1)
      assert(Var(id_x0) != Var(id_x1))
      assert(Abs(Var(id_x0), Var(id_x0)) != Abs(Var(id_x1), Var(id_x1)))
    }
  }
}
