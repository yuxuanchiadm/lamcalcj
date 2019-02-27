package org.lamcalcj.compiler

import java.io.Reader

import org.lamcalcj.parser._
import org.lamcalcj.parser.Combinator._
import org.lamcalcj.parser.Parser._
import org.lamcalcj.parser.Text._
import org.lamcalcj.ast.Lambda._

object Compiler {
  def runLambdaParser(
    source: Text,
    location: Location = Location(1, 1),
    freeVars: Map[String, Identifier] = Map.empty,
    lambdaSyntax: LambdaSyntax = LambdaSyntax.default): Either[ParserError, (Map[String, Identifier], Term)] =
    runParser(for {
      term <- LambdaParser(lambdaSyntax).lambdaTermP(Map.empty)
      _ <- eof
      freeVars <- getState
    } yield (freeVars, term), source, location, freeVars)

  sealed case class LambdaParser(lambdaSyntax: LambdaSyntax) {
    def lambdaSeparatorP[A](innerP: Parser[Text, Map[String, Identifier], A]): Parser[Text, Map[String, Identifier], A] =
      between(lambdaSyntax.indentationP, lambdaSyntax.indentationP, innerP)

    def lambdaIdentifierP(identifierLookup: String => Parser[Text, Map[String, Identifier], Identifier]): Parser[Text, Map[String, Identifier], Identifier] =
      for {
        name <- lambdaSyntax.identifierP
        identifier <- identifierLookup(name)
      } yield identifier

    def lambdaVariableP(boundVars: Map[String, Identifier]): Parser[Text, Map[String, Identifier], Term] =
      lambdaSyntax.variableP(lambdaSeparatorP(for {
        identifier <- lambdaIdentifierP(name => boundVars.get(name)
          .map(unit[Text, Map[String, Identifier], Identifier])
          .getOrElse(for {
            freeVars <- getState
            identifier <- freeVars.get(name)
              .map(unit[Text, Map[String, Identifier], Identifier])
              .getOrElse(for {
                identifier <- unit[Text, Map[String, Identifier], Identifier](Identifier(name))
                _ <- putState(freeVars + (name -> identifier))
              } yield identifier)
          } yield identifier))
      } yield Var(identifier)))

    def lambdaAbstractionP(boundVars: Map[String, Identifier]): Parser[Text, Map[String, Identifier], Term] =
      lambdaSyntax.abstractionP(lambdaSeparatorP(for {
        bindings <- lambdaSeparatorP(recursive(() => lambdaBindingP(boundVars)))
        _ <- lambdaSyntax.abstractionInfixP
        term <- bindings.foldRight((boundVars: Map[String, Identifier]) => lambdaSeparatorP(recursive(() => lambdaTermP(boundVars))))(
          (binding, f) => boundVars => for {
            term <- f(boundVars + (binding.name -> binding))
          } yield Abs(binding, term))(boundVars)
      } yield term))

    def lambdaBindingP(boundVars: Map[String, Identifier]): Parser[Text, Map[String, Identifier], List[Identifier]] =
      lambdaSyntax.bindingP(lambdaSeparatorP(manySepBy(lambdaSeparatorP(lambdaIdentifierP(name => unit(Identifier(name)))), lambdaSyntax.bindingInfixP)))

    def lambdaApplicationP(boundVars: Map[String, Identifier]): Parser[Text, Map[String, Identifier], Term] =
      lambdaSyntax.applicationP(lambdaSeparatorP(for {
        term <- lambdaSeparatorP(recursive(() => lambdaEnclosedTermP(boundVars)))
        _ <- lambdaSyntax.applicationInfixP
        arguments <- lambdaSeparatorP(recursive(() => lambdaArgumentP(boundVars)))
      } yield arguments.foldLeft(term)(App(_, _))))

    def lambdaArgumentP(boundVars: Map[String, Identifier]): Parser[Text, Map[String, Identifier], List[Term]] =
      lambdaSyntax.argumentP(lambdaSeparatorP(manySepBy(lambdaSeparatorP(recursive(() => lambdaEnclosedTermP(boundVars))), lambdaSyntax.argumentInfixP)))

    def lambdaSyntaxGroupP(boundVars: Map[String, Identifier]): Parser[Text, Map[String, Identifier], Term] =
      lambdaSyntax.parenthesesP(lambdaSeparatorP(recursive(() => lambdaTermP(boundVars))))

    def lambdaTermP(boundVars: Map[String, Identifier]): Parser[Text, Map[String, Identifier], Term] =
      (if (lambdaSyntax.enclosingPolicy.variableEnclosing) mzero[Text, Map[String, Identifier], Term] else attempt(recursive(() => lambdaVariableP(boundVars)))) <|>
        (if (lambdaSyntax.enclosingPolicy.abstractionEnclosing) mzero[Text, Map[String, Identifier], Term] else attempt(recursive(() => lambdaAbstractionP(boundVars)))) <|>
        (if (lambdaSyntax.enclosingPolicy.applicationEnclosing) mzero[Text, Map[String, Identifier], Term] else attempt(recursive(() => lambdaApplicationP(boundVars)))) <|>
        recursive(() => lambdaEnclosedTermP(boundVars))

    def lambdaEnclosedTermP(boundVars: Map[String, Identifier]): Parser[Text, Map[String, Identifier], Term] =
      (if (lambdaSyntax.enclosingPolicy.variableEnclosing) attempt(recursive(() => lambdaVariableP(boundVars))) else mzero[Text, Map[String, Identifier], Term]) <|>
        (if (lambdaSyntax.enclosingPolicy.abstractionEnclosing) attempt(recursive(() => lambdaAbstractionP(boundVars))) else mzero[Text, Map[String, Identifier], Term]) <|>
        (if (lambdaSyntax.enclosingPolicy.applicationEnclosing) attempt(recursive(() => lambdaApplicationP(boundVars))) else mzero[Text, Map[String, Identifier], Term]) <|>
        recursive(() => lambdaSyntaxGroupP(boundVars))
  }
}