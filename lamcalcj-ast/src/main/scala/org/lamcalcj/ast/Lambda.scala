package org.lamcalcj.ast

object Lambda {
  object Identifier {
    def apply(name: String): Identifier = new Identifier(name)
  }
  sealed class Identifier(val name: String) {
    override def toString: String = name + "@" + ##

    def cloneIdentifier(newName: String = name): Identifier = Identifier(newName)
  }

  sealed abstract class Term(val size: Int, val depth: Int)
  case class Var(identifier: Identifier) extends Term(1, 1)
  case class Abs(binding: Identifier, term: Term) extends Term(1 + term.size, 1 + term.depth)
  case class App(term: Term, argument: Term) extends Term(1 + term.size + argument.size, 1 + math.max(term.depth, argument.depth))

  def Abs(binding: Identifier)(term: Term): Abs = Abs(binding, term)
  def App(term: Term)(argument: Term): App = App(term, argument)
}
