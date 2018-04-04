package org.lamcalcj.parser.lexical

import org.scalatest.FunSpec
import java.io.StringReader

class TokenizerTest extends FunSpec {
  describe("Basic terms") {
    it("Should successfully tokenize") {
      assert(Tokenizer.tokenize(new StringReader("x")).isRight)
      assert(Tokenizer.tokenize(new StringReader("λx. x")).isRight)
      assert(Tokenizer.tokenize(new StringReader("(x x)")).isRight)
      assert(Tokenizer.tokenize(new StringReader("0")).isRight)
    }
    it("Should reject illegal input") {
      assert(Tokenizer.tokenize(new StringReader("#")).isLeft)
    }
  }
}
