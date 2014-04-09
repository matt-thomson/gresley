package uk.co.mattthomson.coursera.ggp.gresley.parser

trait Fact extends Statement {
  def substitute(values: Map[String, String]): Fact

  def matches(completeFact: Fact, values: Map[String, String]): Option[Map[String, String]]
}

case class Role(private val nameTerm: Term) extends Fact {
  override def substitute(values: Map[String, String]) = Role(nameTerm.substitute(values))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Role(otherTerm) => nameTerm.matches(otherTerm)
    case _ => None
  }
}

case class Relation(name: String, terms: Seq[Term]) extends Fact {
  override def substitute(values: Map[String, String]) = Relation(name, terms.map(_.substitute(values)))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Relation(otherName, otherTerms) => if (name != otherName) None else Term.matchTerms(terms, otherTerms, values)
    case _ => None
  }
}

case class Base(fact: Fact) extends Fact {
  override def substitute(values: Map[String, String]) = Base(fact.substitute(values))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Base(otherFact) => fact.matches(otherFact, values)
    case _ => None
  }
}

case class Action(nameTerm: Term)

case class Input(role: Role, action: Action) extends Fact {
  override def substitute(values: Map[String, String]) = Input(role.substitute(values), Action(action.nameTerm.substitute(values)))

  override def matches(completeFact: Fact, values: Map[String, String]): Option[Map[String, String]] = ???
}