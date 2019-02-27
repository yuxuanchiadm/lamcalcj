package org.lamcalcj.ast

object Lambda {
  object Identifier {
    def apply(name: String): Identifier = new Identifier(name)
  }
  sealed class Identifier(val name: String) {
    override def toString: String = name + "@" + ##
  }

  sealed trait Term
  case class Var(identifier: Identifier) extends Term
  case class Abs(binding: Identifier, term: Term) extends Term
  case class App(term: Term, argument: Term) extends Term

  def Abs(binding: Identifier)(term: Term): Abs = Abs(binding, term)
  def App(term: Term)(argument: Term): App = App(term, argument)
}
