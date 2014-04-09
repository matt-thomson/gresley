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
