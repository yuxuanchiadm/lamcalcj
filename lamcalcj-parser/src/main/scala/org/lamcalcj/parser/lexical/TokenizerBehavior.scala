package org.lamcalcj.parser.lexical

import org.lamcalcj.parser.lexical.Kind._
import scala.collection.immutable.ListMap

object TokenizerBehavior {
  val defaultMatchers: ListMap[Kind, TokenMatcher] = ListMap(
    EOF -> new CodePointTokenMatcher(-1),
    Space -> new RepeatConditionTokenMatcher(cp => cp == ' ' | cp == '\r' | cp == '\n' | cp == '\t'),
    Abstract -> new CodePointTokenMatcher('λ'),
    Delimiter -> new CodePointTokenMatcher('.'),
    Identifier -> new RepeatConditionTokenMatcher(cp => cp != 'λ' && (Character.isLetter(cp) || Character.isDigit(cp) ||
      Character.getType(cp) == Character.MATH_SYMBOL || Character.getType(cp) == Character.OTHER_SYMBOL || cp == '$' || cp == '_')),
    Begin -> new CodePointTokenMatcher('('),
    End -> new CodePointTokenMatcher(')'))
}

sealed case class TokenizerBehavior(matchers: ListMap[Kind, TokenMatcher] = TokenizerBehavior.defaultMatchers)
