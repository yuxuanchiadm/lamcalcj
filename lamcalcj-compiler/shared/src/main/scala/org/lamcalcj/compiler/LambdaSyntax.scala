package org.lamcalcj.compiler

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.parser._
import org.lamcalcj.parser.Combinator._
import org.lamcalcj.parser.Parser._
import org.lamcalcj.parser.Text._
import org.lamcalcj.compiler.LambdaSyntax._

trait LambdaSyntax {
  def indentationP[U]: Parser[Text, U, Unit]

  def nameP[U]: Parser[Text, U, String]

  def identifierP[U](p: Parser[Text, U, String], f: String => Parser[Text, U, Identifier]): Parser[Text, U, Identifier]

  def variableP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A]

  def abstractionP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A]

  def abstractionInfixP[U]: Parser[Text, U, Unit]

  def bindingP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A]

  def bindingInfixP[U]: Parser[Text, U, Unit]

  def applicationP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A]

  def applicationInfixP[U]: Parser[Text, U, Unit]

  def argumentP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A]

  def argumentInfixP[U]: Parser[Text, U, Unit]

  def parenthesesP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A]

  def enclosingPolicy: EnclosingPolicy
}

object LambdaSyntax {
  sealed case class EnclosingPolicy(
    variableEnclosing: Boolean,
    abstractionEnclosing: Boolean,
    applicationEnclosing: Boolean)

  object DefaultLambdaSyntax extends LambdaSyntax {
    def indentationP[U]: Parser[Text, U, Unit] = skipSome(charSatisfy(Character.isWhitespace))

    def nameP[U]: Parser[Text, U, String] = stringSatisfy(c =>
      c != 'λ' && (
        c.isLetterOrDigit ||
        Character.getType(c) == Character.MATH_SYMBOL ||
        Character.getType(c) == Character.OTHER_SYMBOL ||
          c == '$' ||
          c == '_')).advancing() <#> MessageExpected("<Name>")

    def identifierP[U](p: Parser[Text, U, String], f: String => Parser[Text, U, Identifier]): Parser[Text, U, Identifier] = p >>= f

    def variableP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A] = p

    def abstractionP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A] = char('λ') >>& p

    def abstractionInfixP[U]: Parser[Text, U, Unit] = char('.') >>& unit(())

    def bindingP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A] = p

    def bindingInfixP[U]: Parser[Text, U, Unit] = unit(())

    def applicationP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A] = p

    def applicationInfixP[U]: Parser[Text, U, Unit] = unit(())

    def argumentP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A] = p

    def argumentInfixP[U]: Parser[Text, U, Unit] = unit(())

    def parenthesesP[U, A](p: Parser[Text, U, A]): Parser[Text, U, A] = between(char('('), char(')'), p)

    def enclosingPolicy: EnclosingPolicy = EnclosingPolicy(true, false, false)
  }

  def default: LambdaSyntax = DefaultLambdaSyntax
}
