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
  case class Abs(variable: Var, term: Term) extends Term
  case class App(term: Term, argument: Term) extends Term

  def Abs(variable: Var)(term: Term): Abs = Abs(variable, term)
  def App(term: Term)(argument: Term): App = App(term, argument)
}
