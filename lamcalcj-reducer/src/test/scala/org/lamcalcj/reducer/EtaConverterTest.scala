package org.lamcalcj.reducer

import java.io.StringReader

import org.scalatest.FunSpec
import org.lamcalcj.parser.syntax.Parser
import org.lamcalcj.pretty.PrettyPrint
import org.lamcalcj.utils.Utils

class EtaConverterTest extends FunSpec {
  describe("Basic terms") {
    it("Should successfully eta convert") {
      assertResult("x")(PrettyPrint.printLambda(EtaConverter.etaConversion(Parser.parse(new StringReader("x")).right.get._2)._2))
      assertResult("λx.x")(PrettyPrint.printLambda(EtaConverter.etaConversion(Parser.parse(new StringReader("λx. x")).right.get._2)._2))
      assertResult("x x")(PrettyPrint.printLambda(EtaConverter.etaConversion(Parser.parse(new StringReader("(x x)")).right.get._2)._2))
      assertResult("0")(PrettyPrint.printLambda(EtaConverter.etaConversion(Parser.parse(new StringReader("0")).right.get._2)._2))
      assertResult("a")(PrettyPrint.printLambda(EtaConverter.etaConversion(Parser.parse(new StringReader("λf.a f")).right.get._2)._2))
    }
    it("Should detect free occurrence of variable") {
      assertResult("λx.x x")(PrettyPrint.printLambda(EtaConverter.etaConversion(Parser.parse(new StringReader("λx.x x")).right.get._2)._2))
    }
  }
}