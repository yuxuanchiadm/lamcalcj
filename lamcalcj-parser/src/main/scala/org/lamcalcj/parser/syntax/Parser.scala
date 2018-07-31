package org.lamcalcj.parser.syntax

import java.io.Reader

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.parser.lexical._
import org.lamcalcj.parser.lexical.Kind._
import org.lamcalcj.parser.lexical.TokenList._
import org.monadscala.Typelevel._
import org.monadscala.instance.Either._
import org.monadscala.typeclass._

import scala.util.Either
import scala.collection.mutable.Queue

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
    return parser.parseLambda(bounds).map((parser.freeVariables, _))
  }

  private class Parser(tokenStream: TokenStream) {
    val monad: Monad[Curry2[Either]# <[String]# <|] = Monad[Curry2[Either]# <[String]# <|]
    import monad._

    var freeVariables: Map[String, Identifier] = Map.empty

    def parseLambda(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      term <- parseTermApp(bounds)
      _ <- tokenStream.next(Kind.EOF)
    } yield term

    def parseTermApp(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      termFirst <- parseTerm(bounds)
      term <- {
        def parseChainedApp(termReceiver: Term): Either[(List[Kind], TokenList), Term] = for {
          token <- tokenStream.peek(Kind.EOF, Kind.Abstract, Kind.Identifier, Kind.Begin, Kind.End)
          termChained <- token.kind match {
            case Kind.Abstract | Kind.Identifier | Kind.Begin => for {
              termArgument <- parseTerm(bounds)
              termApp <- parseChainedApp(App(termReceiver, termArgument))
            } yield termApp
            case Kind.EOF | Kind.End => Right(termReceiver)
          }
        } yield termChained
        parseChainedApp(termFirst)
      }
    } yield term

    def parseTerm(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      token <- tokenStream.peek(Kind.Abstract, Kind.Identifier, Kind.Begin)
      term <- token.kind match {
        case Kind.Abstract => parseAbs(bounds)
        case Kind.Identifier => parseVar(bounds)
        case Kind.Begin => parseGroup(bounds)
      }
    } yield term

    def parseAbs(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      _ <- tokenStream.next(Kind.Abstract)
      term <- parseAbsBody(bounds)
    } yield term

    def parseAbsBody(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      token <- tokenStream.next(Kind.Identifier)
      name = token.image
      identifier = new Identifier(name)
      token <- tokenStream.peek(Kind.Identifier, Kind.Delimiter)
      termBody <- token.kind match {
        case Kind.Identifier => parseAbsBody(bounds + (name -> identifier))
        case Kind.Delimiter => for {
          _ <- tokenStream.next(Kind.Delimiter)
          term <- parseTermApp(bounds + (name -> identifier))
        } yield term
      }
      term = Abs(Var(identifier), termBody)
    } yield term

    def parseVar(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      token <- tokenStream.next(Kind.Identifier)
      name = token.image
      term = Var(bounds.get(name).getOrElse(freeVariables.get(name).getOrElse({
        val identifier = new Identifier(name)
        freeVariables = freeVariables + (name -> identifier)
        identifier
      })))
    } yield term

    def parseGroup(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      _ <- tokenStream.next(Kind.Begin)
      term <- parseTermApp(bounds)
      _ <- tokenStream.next(Kind.End)
    } yield term
  }
}
