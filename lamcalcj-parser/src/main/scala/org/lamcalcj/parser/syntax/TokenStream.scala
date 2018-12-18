package org.lamcalcj.parser.syntax

import org.lamcalcj.parser.lexical._
import org.lamcalcj.parser.lexical.Kind._
import org.lamcalcj.parser.lexical.TokenList._
import org.lamcalcj.utils.Except._

class TokenStream(private var tokenList: TokenList) {
  def peek(expected: Kind*): Except[(List[Kind], TokenList), Token] =
    tokenList match {
      case Tail() => throwE((expected.toList, tokenList))
      case Entry(special, token, next) =>
        if (expected.contains(token.kind)) {
          unit(token)
        } else
          throwE((expected.toList, tokenList))
    }

  def next(expected: Kind*): Except[(List[Kind], TokenList), Token] =
    tokenList match {
      case Tail() => throwE((expected.toList, tokenList))
      case Entry(special, token, next) =>
        if (expected.contains(token.kind)) {
          tokenList = next
          unit(token)
        } else
          throwE((expected.toList, tokenList))
    }

  def split(until: Kind, suffix: TokenList = TokenList.empty): TokenStream = {
    val builder: TokenListBuilder = new TokenListBuilder
    do {} while (tokenList match {
      case Tail() => false
      case Entry(special, token, next) => {
        builder ++= special
        if (token.kind == until) false else {
          builder += token
          tokenList = next
          true
        }
      }
    });
    builder ++= suffix
    return new TokenStream(builder.result())
  }
}
