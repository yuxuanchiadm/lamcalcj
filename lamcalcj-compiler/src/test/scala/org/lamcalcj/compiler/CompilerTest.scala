package org.lamcalcj.compiler

import java.io.StringReader

import org.lamcalcj.parser.Text

import org.scalatest.FunSpec

class CompilerTest extends FunSpec {
  describe("Basic terms") {
    it("Should successfully parse") {
      assert(Compiler.runLambdaParser(Text("x")).isRight)
      assert(Compiler.runLambdaParser(Text("λx. x")).isRight)
      assert(Compiler.runLambdaParser(Text("(x x)")).isRight)
      assert(Compiler.runLambdaParser(Text("0")).isRight)
      assert(Compiler.runLambdaParser(Text("λg.(λx.g (x x)) (λx.g (x x))")).isRight)
    }
    it("Should reject illegal input") {
      assert(Compiler.runLambdaParser(Text("")).isLeft)
      assert(Compiler.runLambdaParser(Text("()")).isLeft)
      assert(Compiler.runLambdaParser(Text("λ.x")).isLeft)
    }
  }
}
