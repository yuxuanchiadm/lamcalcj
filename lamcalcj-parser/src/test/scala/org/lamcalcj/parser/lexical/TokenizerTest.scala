package org.lamcalcj.parser.lexical

import java.io.StringReader

import org.scalatest.FunSpec

class TokenizerTest extends FunSpec {
  describe("Basic terms") {
    it("Should successfully tokenize") {
      assert(Tokenizer.tokenize(new StringReader("x")).isRight)
      assert(Tokenizer.tokenize(new StringReader("λx. x")).isRight)
      assert(Tokenizer.tokenize(new StringReader("(x x)")).isRight)
      assert(Tokenizer.tokenize(new StringReader("0")).isRight)
      assert(Tokenizer.tokenize(new StringReader("λg.(λx.g (x x)) (λx.g (x x))")).isRight)
    }
    it("Should reject illegal input") {
      assert(Tokenizer.tokenize(new StringReader("#")).isLeft)
    }
  }
}
