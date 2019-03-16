package org.lamcalcj.reducer

import org.scalatest.FunSpec
import org.lamcalcj.compiler.Compiler
import org.lamcalcj.parser.Text
import org.lamcalcj.pretty.PrettyPrint
import org.lamcalcj.utils.Utils

class BetaReducerTest extends FunSpec {
  describe("Basic terms") {
    it("Should successfully beta reduce") {
      assertResult("x")(PrettyPrint.printLambda(BetaReducer.betaReduction(Compiler.runLambdaParser(Text("x")).right.get._2).term))
      assertResult("λx.x")(PrettyPrint.printLambda(BetaReducer.betaReduction(Compiler.runLambdaParser(Text("λx. x")).right.get._2).term))
      assertResult("x x")(PrettyPrint.printLambda(BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(x x)")).right.get._2).term))
      assertResult("0")(PrettyPrint.printLambda(BetaReducer.betaReduction(Compiler.runLambdaParser(Text("0")).right.get._2).term))
      assertResult("y")(PrettyPrint.printLambda(BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(λx.x) y")).right.get._2).term))
    }
    it("Should terminate") {
      assert(BetaReducer.betaReduction(
        Compiler.runLambdaParser(Text("(λx.x) (λx.x)")).right.get._2,
        Strategy(maxStep = Option(0xFF))).abortReason.successful)
      assert(!(BetaReducer.betaReduction(
        Compiler.runLambdaParser(Text("(λx.x x) (λx.x x)")).right.get._2,
        Strategy(maxStep = Option(0xFF))).abortReason.successful))
      assert(!(BetaReducer.betaReduction(
        Compiler.runLambdaParser(Text("(λx.x x x) (λx.x x x)")).right.get._2,
        Strategy(maxStep = Option(0xFF))).abortReason.successful))
    }
    it("Should have different bounded variable") {
      assert(Utils.isTermValid(BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(λx.x x) (0 (λy.y))")).right.get._2).term))
    }
    it("Ensure normal order reduce") {
      assert(BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(λx.0) ((λu.u u) (λu.u u))")).right.get._2).abortReason.successful)
      assert(BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(λx.λy.y ((λu.u u) (λu.u u))) 0 (λu.0)")).right.get._2).abortReason.successful)
      assert(BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(λx.(λy.λz.0) 0) 0 ((λu.u u) (λu.u u))")).right.get._2).abortReason.successful)
    }
  }
  describe("Church number") {
    it("Should successfully beta reduce") {
      assert(Utils.isAlphaEquivalent(
        Compiler.runLambdaParser(Text("λf x.f (f (f (f (f x))))")).right.get._2,
        BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(λm.λn.λf.λx.m f (n f x)) (λf.λx.f (f x)) (λf.λx.f (f (f x)))")).right.get._2).term))
      assert(Utils.isAlphaEquivalent(
        Compiler.runLambdaParser(Text("λf x.f (f (f (f x)))")).right.get._2,
        BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(λm.λn.λf.m (n f)) (λf.λx.f (f x)) (λf.λx.f (f x))")).right.get._2).term))
      assert(Utils.isAlphaEquivalent(
        Compiler.runLambdaParser(Text("λf x.f (f (f (f x)))")).right.get._2,
        BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(λb.λe.e b) (λf.λx.f (f x)) (λf.λx.f (f x))")).right.get._2).term))
      assert(Utils.isAlphaEquivalent(
        Compiler.runLambdaParser(Text("λf.λx.x")).right.get._2,
        BetaReducer.betaReduction(Compiler.runLambdaParser(Text("(λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)) (λf.λx.x)")).right.get._2).term))
    }
  }
}