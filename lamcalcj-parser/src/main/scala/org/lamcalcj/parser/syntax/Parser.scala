package org.lamcalcj.parser.syntax

import java.io.Reader

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.parser.lexical._
import org.lamcalcj.utils.Except._

import scala.util.Either

object Parser {
  def parse(
    reader: Reader,
    tokenizerBehavior: TokenizerBehavior = new TokenizerBehavior(),
    bounds: Map[String, Identifier] = Map.empty): Either[String, (Map[String, Identifier], Term)] = for {
    tokenList <- Tokenizer.tokenize(reader, tokenizerBehavior).left.map({
      case (loaction, image) => "Lexical error of token from [line " + loaction.beginLine + ", column " + loaction.beginColumn +
        "] at [line " + loaction.beginLine + ", column " + loaction.beginColumn + "]. Token image: " + image
    })
    result <- parseTokenList(new TokenStream(tokenList), bounds).left.map({
      case (expectedKinds, nextTokenList) => "Syntax error " + nextTokenList.tokenMessage +
        ". Expected kinds: " + expectedKinds.foldLeft("")((msg, kind) => msg + (if (msg.isEmpty) kind else ", " + kind))
    })
  } yield result

  def parseTokenList(
    tokenStream: TokenStream,
    bounds: Map[String, Identifier] = Map.empty): Either[(List[Kind], TokenList), (Map[String, Identifier], Term)] = {
    val parser: Parser = new Parser(tokenStream)
    return parser.parseLambda(bounds).runExcept.map((parser.freeVariables, _))
  }

  private class Parser(tokenStream: TokenStream) {
    var freeVariables: Map[String, Identifier] = Map.empty

    def parseLambda(bounds: Map[String, Identifier]): Except[(List[Kind], TokenList), Term] = for {
      term <- recursive(() => parseTermApp(bounds))
      _ <- tokenStream.next(Kind.EOF)
    } yield term

    def parseTermApp(bounds: Map[String, Identifier]): Except[(List[Kind], TokenList), Term] = for {
      termFirst <- recursive(() => parseTerm(bounds))
      term <- recursive(() => parseChainedApp(bounds, termFirst))
    } yield term

    def parseChainedApp(bounds: Map[String, Identifier], term: Term): Except[(List[Kind], TokenList), Term] = for {
      token <- tokenStream.peek(Kind.EOF, Kind.Abstract, Kind.Identifier, Kind.Begin, Kind.End)
      termChained <- token.kind match {
        case Kind.Abstract | Kind.Identifier | Kind.Begin => for {
          termArgument <- recursive(() => parseTerm(bounds))
          termApp <- recursive(() => parseChainedApp(bounds, App(term, termArgument)))
        } yield termApp
        case Kind.EOF | Kind.End => unit[(List[Kind], TokenList), Term](term)
      }
    } yield termChained

    def parseTerm(bounds: Map[String, Identifier]): Except[(List[Kind], TokenList), Term] = for {
      token <- tokenStream.peek(Kind.Abstract, Kind.Identifier, Kind.Begin)
      term <- token.kind match {
        case Kind.Abstract => recursive(() => parseAbs(bounds))
        case Kind.Identifier => recursive(() => parseVar(bounds))
        case Kind.Begin => recursive(() => parseGroup(bounds))
      }
    } yield term

    def parseAbs(bounds: Map[String, Identifier]): Except[(List[Kind], TokenList), Term] = for {
      _ <- tokenStream.next(Kind.Abstract)
      term <- recursive(() => parseAbsBody(bounds))
    } yield term

    def parseAbsBody(bounds: Map[String, Identifier]): Except[(List[Kind], TokenList), Term] = for {
      token <- tokenStream.next(Kind.Identifier)
      name = token.image
      identifier = new Identifier(name)
      token <- tokenStream.peek(Kind.Identifier, Kind.Delimiter)
      termBody <- token.kind match {
        case Kind.Identifier => recursive(() => parseAbsBody(bounds + (name -> identifier)))
        case Kind.Delimiter => for {
          _ <- tokenStream.next(Kind.Delimiter)
          term <- recursive(() => recursive(() => parseTermApp(bounds + (name -> identifier))))
        } yield term
      }
      term = Abs(Var(identifier), termBody)
    } yield term

    def parseVar(bounds: Map[String, Identifier]): Except[(List[Kind], TokenList), Term] = for {
      token <- tokenStream.next(Kind.Identifier)
      name = token.image
      term = Var(bounds.get(name).getOrElse(freeVariables.get(name).getOrElse({
        val identifier = new Identifier(name)
        freeVariables = freeVariables + (name -> identifier)
        identifier
      })))
    } yield term

    def parseGroup(bounds: Map[String, Identifier]): Except[(List[Kind], TokenList), Term] = for {
      _ <- tokenStream.next(Kind.Begin)
      term <- recursive(() => parseTermApp(bounds))
      _ <- tokenStream.next(Kind.End)
    } yield term
  }
}
