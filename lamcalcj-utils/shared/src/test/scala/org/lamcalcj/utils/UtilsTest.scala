package org.lamcalcj.utils

import org.scalatest.FunSpec
import org.lamcalcj.ast.Lambda._

class UtilsTest extends FunSpec {
  describe("Alpha conversion") {
    it("Trivial terms should not be converted") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(Utils.alphaConversion(Var(id_x)) == Var(id_x))
      assert(Utils.alphaConversion(Abs(id_x, Var(id_x))) == Abs(id_x, Var(id_x)))
      assert(Utils.alphaConversion(
        Abs(id_x) {
          Abs(id_y) {
            App(Var(id_x), Var(id_y))
          }
        }) ==
        Abs(id_x) {
          Abs(id_y) {
            App(Var(id_x), Var(id_y))
          }
        })
    }
    it("Overlapping bounded variable should be converted") {
      val id_x0: Identifier = Identifier("x")
      val id_x1: Identifier = Identifier("x")
      assert(Utils.alphaConversion(Abs(id_x0, Abs(id_x1, Var(id_x1))))
        .asInstanceOf[Abs].term.asInstanceOf[Abs].binding != id_x1)
    }
    it("Overlapping free variable should not be converted") {
      val id_x0: Identifier = Identifier("x")
      val id_x1: Identifier = Identifier("x")
      assert(Utils.alphaConversion(Abs(id_x0, Var(id_x1)))
        .asInstanceOf[Abs].term.asInstanceOf[Var].identifier == id_x1)
    }
  }
  describe("Free variables") {
    it("Trivial terms") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(Utils.freeVariables(Var(id_x)) == Set(id_x))
      assert(Utils.freeVariables(App(Var(id_x), Var(id_y))) == Set(id_x, id_y))
      assert(Utils.freeVariables(Abs(id_x, Var(id_x))) == Set.empty)
    }
  }
  describe("Closed term") {
    it("Trivial terms") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(!Utils.isClosedTerm(Var(id_x)))
      assert(!Utils.isClosedTerm(App(Var(id_x), Var(id_y))))
      assert(Utils.isClosedTerm(Abs(id_x, Var(id_x))))
    }
  }
  describe("Alpha equivalence") {
    it("Trivial terms") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(Utils.isAlphaEquivalent(Var(id_x), Var(id_x)))
      assert(Utils.isAlphaEquivalent(App(Var(id_x), Var(id_y)), App(Var(id_x), Var(id_y))))
      assert(Utils.isAlphaEquivalent(Abs(id_x, Var(id_x)), Abs(id_x, Var(id_x))))
      assert(!Utils.isAlphaEquivalent(Abs(id_x, Var(id_x)), Var(id_x)))
      assert(!Utils.isAlphaEquivalent(App(Var(id_x), Var(id_x)), Var(id_x)))
      assert(!Utils.isAlphaEquivalent(Var(id_x), App(Var(id_x), Var(id_x))))
    }
    it("Free variables not alpha equivalent") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(!Utils.isAlphaEquivalent(Var(id_x), Var(id_y)))
      assert(!Utils.isAlphaEquivalent(App(Var(id_x), Var(id_x)), App(Var(id_y), Var(id_y))))
    }
    it("Alpha equivalence hold if alpha conversion is possible") {
      val id_x0: Identifier = Identifier("x")
      val id_x1: Identifier = Identifier("x")
      assert(Utils.isAlphaEquivalent(Abs(id_x0, Var(id_x0)), Abs(id_x1, Var(id_x1))))
    }
  }
  describe("Valid terms") {
    it("Trivial terms") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(Utils.isTermValid(Var(id_x)))
      assert(Utils.isTermValid(App(Var(id_x), Var(id_y))))
      assert(Utils.isTermValid(Abs(id_x, Var(id_x))))
    }
    it("Invalid terms") {
      val id_x: Identifier = Identifier("x")
      assert(!Utils.isTermValid(Abs(id_x, Abs(id_x, Var(id_x)))))
      assert(!Utils.isTermValid(App(Abs(id_x, Var(id_x)), Abs(id_x, Var(id_x)))))
      assert(!Utils.isTermValid(App(Abs(id_x, Var(id_x)), Var(id_x))))
    }
  }
  describe("Clone term") {
    it("Free variable should not be cloned") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(Utils.cloneTerm(Var(id_x)).asInstanceOf[Var].identifier == id_x)
      assert(Utils.cloneTerm(Abs(id_y, Var(id_x))).asInstanceOf[Abs].term.asInstanceOf[Var].identifier == id_x)
    }
    it("Bounded variable should be cloned") {
      val id_x: Identifier = Identifier("x")
      assert(Utils.cloneTerm(Abs(id_x, Var(id_x))).asInstanceOf[Abs].term.asInstanceOf[Var].identifier != id_x)
    }
  }
  describe("Term analysis") {
    it("Term size") {
      val id_x: Identifier = Identifier("x")
      assertResult(1)(Utils.analyzeTermSize(Var(id_x)))
      assertResult(2)(Utils.analyzeTermSize(Abs(id_x, Var(id_x))))
      assertResult(3)(Utils.analyzeTermSize(App(Var(id_x), Var(id_x))))
      assertResult(4)(Utils.analyzeTermSize(Abs(id_x, App(Var(id_x), Var(id_x)))))
    }
    it("Term depth") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assertResult(1)(Utils.analyzeTermDepth(Var(id_x)))
      assertResult(2)(Utils.analyzeTermDepth(App(Var(id_x), Var(id_x))))
      assertResult(3)(Utils.analyzeTermDepth(App(Var(id_x), App(Var(id_x), Var(id_x)))))
      assertResult(4)(Utils.analyzeTermDepth(App(Abs(id_y, Var(id_y)), App(Var(id_x), App(Var(id_x), Var(id_x))))))
    }
  }
  describe("Term substitution") {
    it("Trivial terms") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assertResult(Var(id_y))(Utils.substitute(Var(id_x), id_x, Var(id_y)))
      assertResult(Abs(id_x, Var(id_x)))(Utils.substitute(Abs(id_x, Var(id_x)), id_x, Var(id_y)))
      assertResult(App(Var(id_y), Var(id_y)))(Utils.substitute(App(Var(id_x), Var(id_x)), id_x, Var(id_y)))
    }
    it("Result term should be copied") {
      val id_x: Identifier = Identifier("x")
      val id_y: Identifier = Identifier("y")
      assert(Utils.substitute(Var(id_x), id_x, Abs(id_y, Var(id_y))) != Abs(id_y, Var(id_y)))
      assert(Utils.isAlphaEquivalent(Abs(id_y, Var(id_y)), Utils.substitute(Var(id_x), id_x, Abs(id_y, Var(id_y)))))
    }
  }
}
