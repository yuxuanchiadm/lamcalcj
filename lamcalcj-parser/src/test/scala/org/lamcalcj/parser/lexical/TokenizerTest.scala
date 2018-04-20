package org.lamcalcj.parser.lexical

import java.io.StringReader

import org.lamcalcj.parser.lexical.Kind._
import org.lamcalcj.parser.lexical.TokenList._
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
  describe("Tokenizer behavior") {
    it("Should greedy match") {
      assertResult(List(Abstract, Identifier, Delimiter, Identifier, EOF))(Tokenizer.tokenize(
        new StringReader("lambda lambda_x. lambda_x"),
        new TokenizerBehavior(for {
          (kind, matcher) <- TokenizerBehavior.defaultMatchers
        } yield (kind -> (if (kind == Abstract) new StringTokenMatcher("lambda") else matcher))))
        .right.get.toList().map(_.kind))
    }
  }
}
