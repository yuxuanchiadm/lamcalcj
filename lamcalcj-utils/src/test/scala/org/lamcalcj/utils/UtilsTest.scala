package org.lamcalcj.utils

import org.scalatest.FunSpec
import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Utils._

class UtilsTest extends FunSpec {
  describe("Alpha conversion") {
    it("Trivial terms should not be converted") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(alphaConversion(Var(id_x)) == (Var(id_x)))
      assert(alphaConversion(Abs(Var(id_x), Var(id_x))) == (Abs(Var(id_x), Var(id_x))))
      assert(alphaConversion(
        Abs(Var(id_x)) {
          Abs(Var(id_y)) {
            App(Var(id_x), Var(id_y))
          }
        }) ==
        Abs(Var(id_x)) {
          Abs(Var(id_y)) {
            App(Var(id_x), Var(id_y))
          }
        })
    }
    it("Overlapping bounded variable should be converted") {
      val id_x0: Identifier = Identifier("x")
      val id_x1: Identifier = Identifier("x")
      assert(alphaConversion(Abs(Var(id_x0), Abs(Var(id_x1), Var(id_x1))))
        .asInstanceOf[Abs].term.asInstanceOf[Abs].variable.identifier != id_x1)
    }
    it("Overlapping free variable should not be converted") {
      val id_x0: Identifier = Identifier("x")
      val id_x1: Identifier = Identifier("x")
      assert(alphaConversion(Abs(Var(id_x0), Var(id_x1)))
        .asInstanceOf[Abs].term.asInstanceOf[Var].identifier == id_x1)
    }
  }
  describe("Free variables") {
    it("Trivial terms") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(freeVariables(Var(id_x)) == Set(id_x))
      assert(freeVariables(App(Var(id_x), Var(id_y))) == Set(id_x, id_y))
      assert(freeVariables(Abs(Var(id_x), Var(id_x))) == Set.empty)
    }
  }
  describe("Alpha equivalence") {
    it("Trivial terms") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(isAlphaEquivalent(Var(id_x), Var(id_x)))
      assert(isAlphaEquivalent(App(Var(id_x), Var(id_y)), App(Var(id_x), Var(id_y))))
      assert(isAlphaEquivalent(Abs(Var(id_x), Var(id_x)), Abs(Var(id_x), Var(id_x))))
    }
    it("Free variables not alpha equivalet") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(!isAlphaEquivalent(Var(id_x), Var(id_y)))
      assert(!isAlphaEquivalent(App(Var(id_x), Var(id_x)), App(Var(id_y), Var(id_y))))
    }
    it("Alpha equivalence hold if alpha conversion is possible") {
      val id_x0: Identifier = Identifier("x")
      val id_x1: Identifier = Identifier("x")
      assert(isAlphaEquivalent(Abs(Var(id_x0), Var(id_x0)), Abs(Var(id_x1), Var(id_x1))))
    }
  }
  describe("Valid terms") {
    it("Trivial terms") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(isTermValid(Var(id_x)))
      assert(isTermValid(App(Var(id_x), Var(id_y))))
      assert(isTermValid(Abs(Var(id_x), Var(id_x))))
    }
    it("Invalid terms") {
      val id_x: Identifier = Identifier("x")
      assert(!isTermValid(Abs(Var(id_x), Abs(Var(id_x), Var(id_x)))))
      assert(!isTermValid(App(Abs(Var(id_x), Var(id_x)), Abs(Var(id_x), Var(id_x)))))
      assert(!isTermValid(App(Abs(Var(id_x), Var(id_x)), Var(id_x))))
    }
  }
}
