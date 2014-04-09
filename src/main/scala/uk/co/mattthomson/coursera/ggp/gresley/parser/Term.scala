package uk.co.mattthomson.coursera.ggp.gresley.parser

trait Term {
  def substitute(values: Map[String, String]): Term

  def matches(term: Term): Option[Map[String, String]]
}

case class LiteralTerm(name: String) extends Term {
  override def substitute(values: Map[String, String]) = this

  override def matches(term: Term) = term match {
    case LiteralTerm(otherName) => if (name == otherName) Some(Map()) else None
    case VariableTerm(otherName) => Some(Map(otherName -> name))
  }
}

case class VariableTerm(name: String) extends Term {
  override def substitute(values: Map[String, String]) = values.get(name) match {
    case Some(value) => LiteralTerm(value)
    case None => this
  }

  override def matches(term: Term) = term match {
    case LiteralTerm(otherName) => Some(Map(name -> otherName))
    case VariableTerm(otherName) => if (name == otherName) Some(Map()) else None
  }
}

object Term {
  implicit def stringToLiteral(name: String) = LiteralTerm(name)
}