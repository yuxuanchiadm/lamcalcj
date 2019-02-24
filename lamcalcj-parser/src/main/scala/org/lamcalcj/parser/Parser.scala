package org.lamcalcj.parser

import org.lamcalcj.parser.Parser._
import org.lamcalcj.utils.Trampoline._

sealed case class Parser[S, U, A](unParser: (S, Location, U) => Trampoline[(Boolean, Result[S, U, A])]) {
  final def map[B](f: A => B): Parser[S, U, B] =
    Parser((s, l, u) => unParser(s, l, u).map(v => v match {
      case (b, ResultSuccess(s0, l0, u0, a)) => (b, ResultSuccess(s0, l0, u0, f(a)))
      case (b, ResultError(e)) => (b, ResultError(e))
    }))

  final def flatMap[B](f: A => Parser[S, U, B]): Parser[S, U, B] =
    Parser((s, l, u) => unParser(s, l, u).flatMap(v => v match {
      case (b, ResultSuccess(s0, l0, u0, a)) => More(() => f(a).unParser(s0, l0, u0).map({ case (b0, r) => (b || b0, r) }))
      case (b, ResultError(e)) => Done((b, ResultError(e)))
    }))

  final def >>&[B](p: Parser[S, U, B]): Parser[S, U, B] = flatMap(_ => p)

  final def &>>[B](p: Parser[S, U, B]): Parser[S, U, A] = flatMap(a => p.map(_ => a))

  final def mplus(p: Parser[S, U, A]): Parser[S, U, A] =
    Parser((s, l, u) => unParser(s, l, u).flatMap(v => v match {
      case (b, ResultSuccess(s0, l0, u0, a)) => Done((b, ResultSuccess(s0, l0, u0, a)))
      case (false, ResultError(e)) => More(() => p.unParser(s, l, u).map(v => v match {
        case (b, ResultSuccess(s0, l0, u0, a)) => (b, ResultSuccess(s0, l0, u0, a))
        case (b, ResultError(e0)) => (b, ResultError(e ++ e0))
      }))
      case (true, ResultError(e)) => Done((true, ResultError(e)))
    }))

  final def <|>(p: Parser[S, U, A]): Parser[S, U, A] = mplus(p)

  final def message(messages: Message*): Parser[S, U, A] =
    Parser((s, l, u) => unParser(s, l, u).map(v => v match {
      case (b, ResultSuccess(s0, l0, u0, a)) => (b, ResultSuccess(s0, l0, u0, a))
      case (b, ResultError(e)) => (b, ResultError(ParserError(l, messages.toList)))
    }))

  final def <#>(m: Message): Parser[S, U, A] = message(m)

  final def advancing(): Parser[S, U, A] =
    Parser((s, l, u) => unParser(s, l, u).map(v => v match {
      case (false, ResultSuccess(s0, l0, u0, a)) => (false, ResultError(ParserError(l, List.empty)))
      case (true, ResultSuccess(s0, l0, u0, a)) => (true, ResultSuccess(s0, l0, u0, a))
      case (b, ResultError(e)) => (b, ResultError(e))
    }))

  final def localState[V](f: V => U): Parser[S, V, A] =
    Parser((s, l, u) => unParser(s, l, f(u)).map(v => v match {
      case (b, ResultSuccess(s0, l0, u0, a)) => (b, ResultSuccess(s0, l0, u, a))
      case (b, ResultError(e)) => (b, ResultError(e))
    }))
}

object Parser {
  final def runParser[S, U, A](p: Parser[S, U, A], s: S, l: Location, u: U): Either[ParserError, A] =
    p.unParser(s, l, u).runT._2 match {
      case ResultSuccess(s0, l0, u0, a) => Right(a)
      case ResultError(e) => Left(e)
    }

  final def unit[S, U, A](a: A): Parser[S, U, A] = Parser((s, l, u) => Done((false, ResultSuccess(s, l, u, a))))

  final def mzero[S, U, A]: Parser[S, U, A] = Parser((s, l, u) => Done((false, ResultError(ParserError(l, List.empty)))))

  final def unexpected[S, U, A](message: String): Parser[S, U, A] = Parser((s, l, u) => Done((false, ResultError(ParserError(l, List(MessageUnexpected(message)))))))

  final def lookahead[S, U, A](p: Parser[S, U, A]): Parser[S, U, A] =
    Parser((s, l, u) => p.unParser(s, l, u).map(v => v match {
      case (b, ResultSuccess(s0, l0, u0, a)) => (false, ResultSuccess(s0, l0, u0, a))
      case (b, ResultError(e)) => (b, ResultError(e))
    }))

  final def attempt[S, U, A](p: Parser[S, U, A]): Parser[S, U, A] =
    Parser((s, l, u) => p.unParser(s, l, u).map(v => v match {
      case (b, ResultSuccess(s0, l0, u0, a)) => (b, ResultSuccess(s0, l0, u0, a))
      case (b, ResultError(e)) => (false, ResultError(e))
    }))

  final def getInput[S, U]: Parser[S, U, S] = Parser((s, l, u) => Done((false, ResultSuccess(s, l, u, s))))

  final def setInput[S, U](s: S): Parser[S, U, Unit] = Parser((s0, l, u) => Done((false, ResultSuccess(s, l, u, ()))))

  final def getState[S, U]: Parser[S, U, U] = Parser((s, l, u) => Done((false, ResultSuccess(s, l, u, u))))

  final def putState[S, U](u: U): Parser[S, U, Unit] = Parser((s, l, u0) => Done((false, ResultSuccess(s, l, u, ()))))

  final def modifyState[S, U](f: U => U): Parser[S, U, Unit] = Parser((s, l, u) => Done((false, ResultSuccess(s, l, f(u), ()))))

  final def getLocation[S, U]: Parser[S, U, Location] = Parser((s, l, u) => Done((false, ResultSuccess(s, l, u, l))))

  final def setLocation[S, U](l: Location): Parser[S, U, Unit] = Parser((s, l0, u) => Done((false, ResultSuccess(s, l, u, ()))))

  final def recursive[S, U, A](k: () => Parser[S, U, A]): Parser[S, U, A] = Parser((s, l, u) => More(() => k().unParser(s, l, u)))

  sealed trait Result[S, U, A]
  sealed case class ResultSuccess[S, U, A](s: S, l: Location, u: U, a: A) extends Result[S, U, A]
  sealed case class ResultError[S, U, A](e: ParserError) extends Result[S, U, A]

  sealed case class Message(message: String)
  sealed class MessageUnexpected(message: String) extends Message(message: String)
  object MessageUnexpected { def apply(message: String): MessageUnexpected = new MessageUnexpected(message) }
  sealed class MessageExpected(message: String) extends Message(message)
  object MessageExpected { def apply(message: String): MessageExpected = new MessageExpected(message) }
  sealed class MessageRaw(message: String) extends Message(message)
  object MessageRaw { def apply(message: String): MessageRaw = new MessageRaw(message) }

  sealed case class Location(line: Int, column: Int) {
    final def compare(l: Location): Int =
      line.compare(l.line) match {
        case 0 => column.compare(l.column)
        case o => o
      }

    final def advanceString(s: String) = s.foldLeft(this)(_ advanceChar _)

    final def advanceChar(c: Char) = c match {
      case '\n' => Location(line + 1, 1)
      case _ => Location(line, column + 1)
    }

    override def toString: String = "(line: " + line + ", column: " + column + ")";
  }

  sealed case class ParserError(location: Location, messages: List[Message]) {
    final def ++(e: ParserError) =
      if (messages.isEmpty && !e.messages.isEmpty)
        e
      else if (!messages.isEmpty && e.messages.isEmpty)
        this
      else location.compare(e.location) match {
        case o if o < 0 => e
        case o if o == 0 => ParserError(location, messages ++ e.messages)
        case o if o > 0 => this
      }

    override def toString: String =
      "unexcepted: (" + String.join(", ", messages.filter(_.isInstanceOf[MessageUnexpected]).distinct.map(_.message).sorted.toArray: _*) + ")\n" +
        "excepted: (" + String.join(", ", messages.filter(_.isInstanceOf[MessageExpected]).distinct.map(_.message).sorted.toArray: _*) + ")\n" +
        "message: (" + String.join(", ", messages.filter(_.isInstanceOf[MessageRaw]).distinct.map(_.message).sorted.toArray: _*) + ")\n" +
        "at " + location
  }
}
