package org.lamcalcj.parser.syntax

import java.io.Reader

import org.lamcalcj.ast.Lambda._
import org.lamcalcj.parser.lexical._
import org.lamcalcj.parser.lexical.Kind._
import org.lamcalcj.parser.lexical.TokenList._
import org.monadscala.Either._
import org.monadscala.Typelevel._
import org.monadscala._

import scala.util.Either
import scala.collection.mutable.Queue

object Parser {
  def parse(reader: Reader): Either[String, (Map[String, Identifier], Term)] = for {
    tokenList <- Tokenizer.tokenize(reader).left.map({
      case (loaction, image) => "Lexical error of token from [line " + loaction.beginLine + ", column " + loaction.beginColumn +
        "] at [line " + loaction.beginLine + ", column " + loaction.beginColumn + "]. Token image: " + image
    })
    result <- parse(tokenList).left.map({
      case (expectedKinds, nextTokenList) => "Syntax error " + nextTokenList.tokenMessage +
        ". Expected kinds: " + ("" /: expectedKinds)((msg, kind) => msg + (if (msg.isEmpty) kind else ", " + kind))
    })
  } yield result

  def parse(tokenList: TokenList): Either[(List[Kind], TokenList), (Map[String, Identifier], Term)] = {
    val parser: Parser = new Parser(tokenList)
    return parser.parseLambda(Map.empty).map((parser.freeVariables, _))
  }

  private class Parser(inputTokenList: TokenList) {
    val monad: Monad[Currying[Either, String]#Type] = Monad[Currying[Either, String]#Type]
    import monad._

    var tokenList: TokenList = inputTokenList
    var freeVariables: Map[String, Identifier] = Map.empty

    def parseLambda(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      term <- parseTermApp(bounds)
      _ <- next(Kind.EOF)
    } yield term

    def parseTermApp(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = {
      var terms: Queue[Term] = Queue.empty
      var nextToken: Token = null
      do {
        val termE = parseTerm(bounds)
        if (termE.isLeft) return Left(termE.left.get)
        terms += termE.right.get
        val tokenE = look()
        if (tokenE.isLeft) return Left(tokenE.left.get)
        nextToken = tokenE.right.get
      } while (nextToken.kind == Kind.Abstract || nextToken.kind == Kind.Identifier || nextToken.kind == Kind.Begin)
      return Right(terms.reduceLeft(App))
    }

    def parseTerm(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      token <- peek(Kind.Abstract, Kind.Identifier, Kind.Begin)
      term <- token.kind match {
        case Kind.Abstract => parseAbs(bounds)
        case Kind.Identifier => parseVar(bounds)
        case Kind.Begin => parseGroup(bounds)
        case _ => throw new IllegalStateException
      }
    } yield term

    def parseAbs(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      _ <- next(Kind.Abstract)
      term <- parseAbsBody(bounds)
    } yield term

    def parseAbsBody(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      token <- next(Kind.Identifier)
      name = token.image
      identifier = new Identifier(name)
      token <- peek(Kind.Identifier, Kind.Delimiter)
      termBody <- token.kind match {
        case Kind.Identifier => parseAbsBody(bounds + (name -> identifier))
        case Kind.Delimiter => for {
          _ <- next(Kind.Delimiter)
          term <- parseTermApp(bounds + (name -> identifier))
        } yield term
        case _ => throw new IllegalStateException
      }
      term = Abs(Var(identifier), termBody)
    } yield term

    def parseVar(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      token <- next(Kind.Identifier)
      name = token.image
      term = Var(bounds.get(name).getOrElse(freeVariables.get(name).getOrElse({
        val identifier = new Identifier(name)
        freeVariables = freeVariables + (name -> identifier)
        identifier
      })))
    } yield term

    def parseGroup(bounds: Map[String, Identifier]): Either[(List[Kind], TokenList), Term] = for {
      _ <- next(Kind.Begin)
      term <- parseTermApp(bounds)
      _ <- next(Kind.End)
    } yield term

    def look(): Either[(List[Kind], TokenList), Token] =
      tokenList match {
        case Tail() => Left((List(Kind.EOF, Kind.Space, Kind.Abstract, Kind.Delimiter, Kind.Identifier, Kind.Begin, Kind.End), tokenList))
        case Entry(special, token, next) => Right(token)
      }

    def peek(expected: Kind*): Either[(List[Kind], TokenList), Token] =
      tokenList match {
        case Tail() => Left((expected.toList, tokenList))
        case Entry(special, token, next) =>
          if (expected.contains(token.kind)) {
            Right(token)
          } else
            Left((expected.toList, tokenList))
      }

    def next(expected: Kind*): Either[(List[Kind], TokenList), Token] =
      tokenList match {
        case Tail() => Left((expected.toList, tokenList))
        case Entry(special, token, next) =>
          if (expected.contains(token.kind)) {
            tokenList = next
            Right(token)
          } else
            Left((expected.toList, tokenList))
      }
  }
}
