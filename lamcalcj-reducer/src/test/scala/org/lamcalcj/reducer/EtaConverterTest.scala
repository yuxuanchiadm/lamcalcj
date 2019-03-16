package org.lamcalcj.reducer

import org.scalatest.FunSpec
import org.lamcalcj.compiler.Compiler
import org.lamcalcj.parser.Text
import org.lamcalcj.pretty.PrettyPrint
import org.lamcalcj.utils.Utils

class EtaConverterTest extends FunSpec {
  describe("Basic terms") {
    it("Should successfully eta convert") {
      assertResult("x")(PrettyPrint.printLambda(EtaConverter.etaConversion(Compiler.runLambdaParser(Text("x")).right.get._2).term))
      assertResult("λx.x")(PrettyPrint.printLambda(EtaConverter.etaConversion(Compiler.runLambdaParser(Text("λx. x")).right.get._2).term))
      assertResult("x x")(PrettyPrint.printLambda(EtaConverter.etaConversion(Compiler.runLambdaParser(Text("(x x)")).right.get._2).term))
      assertResult("0")(PrettyPrint.printLambda(EtaConverter.etaConversion(Compiler.runLambdaParser(Text("0")).right.get._2).term))
      assertResult("a")(PrettyPrint.printLambda(EtaConverter.etaConversion(Compiler.runLambdaParser(Text("λf.a f")).right.get._2).term))
    }
    it("Should detect free occurrence of variable") {
      assertResult("λx.x x")(PrettyPrint.printLambda(EtaConverter.etaConversion(Compiler.runLambdaParser(Text("λx.x x")).right.get._2).term))
    }
  }
}