package org.lamcalcj.pretty

import org.scalatest.FunSpec
import org.lamcalcj.compiler.Compiler
import org.lamcalcj.parser.Text

class PrettyPrintTest extends FunSpec {
  describe("Basic terms") {
    it("Should successfully pretty print") {
      assertResult("x")(PrettyPrint.printLambda(Compiler.runLambdaParser(Text("x")).right.get._2))
      assertResult("λx.x")(PrettyPrint.printLambda(Compiler.runLambdaParser(Text("λx. x")).right.get._2))
      assertResult("x x")(PrettyPrint.printLambda(Compiler.runLambdaParser(Text("(x x)")).right.get._2))
      assertResult("0")(PrettyPrint.printLambda(Compiler.runLambdaParser(Text("0")).right.get._2))
      assertResult("λg.(λx.g (x x)) (λx.g (x x))")(PrettyPrint.printLambda(Compiler.runLambdaParser(Text("λg.(λx.g (x x)) (λx.g (x x))")).right.get._2))
    }
  }
}