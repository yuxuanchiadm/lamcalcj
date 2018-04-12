package org.lamcalcj.pretty

import java.io.StringReader

import org.scalatest.FunSpec
import org.lamcalcj.ast.Lambda._
import org.lamcalcj.parser.syntax.Parser

class PrettyPrintTest extends FunSpec {
  describe("Basic terms") {
    it("Should successfully pretty print") {
      assertResult("x")(PrettyPrint.printLambda(Parser.parse(new StringReader("x")).right.get._2))
      assertResult("λx.x")(PrettyPrint.printLambda(Parser.parse(new StringReader("λx. x")).right.get._2))
      assertResult("x x")(PrettyPrint.printLambda(Parser.parse(new StringReader("(x x)")).right.get._2))
      assertResult("0")(PrettyPrint.printLambda(Parser.parse(new StringReader("0")).right.get._2))
      assertResult("λg.(λx.g (x x)) (λx.g (x x))")(PrettyPrint.printLambda(Parser.parse(new StringReader("λg.(λx.g (x x)) (λx.g (x x))")).right.get._2))
    }
  }
}