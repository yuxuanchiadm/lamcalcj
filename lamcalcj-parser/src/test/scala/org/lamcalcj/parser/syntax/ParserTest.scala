package org.lamcalcj.parser.syntax

import java.io.StringReader

import org.scalatest.FunSpec
import org.lamcalcj.parser.lexical.Tokenizer

class ParserTest extends FunSpec {
  describe("Basic terms") {
    it("Should successfully parse") {
      assert(Parser.parse(Tokenizer.tokenize(new StringReader("x")).right.get).isRight)
      assert(Parser.parse(Tokenizer.tokenize(new StringReader("λx. x")).right.get).isRight)
      assert(Parser.parse(Tokenizer.tokenize(new StringReader("(x x)")).right.get).isRight)
      assert(Parser.parse(Tokenizer.tokenize(new StringReader("0")).right.get).isRight)
      assert(Parser.parse(Tokenizer.tokenize(new StringReader("λg.(λx.g (x x)) (λx.g (x x))")).right.get).isRight)
    }
    it("Should reject illegal input") {
      assert(Parser.parse(Tokenizer.tokenize(new StringReader("")).right.get).isLeft)
      assert(Parser.parse(Tokenizer.tokenize(new StringReader("()")).right.get).isLeft)
      assert(Parser.parse(Tokenizer.tokenize(new StringReader("λ.x")).right.get).isLeft)
    }
  }
}
