package uk.co.mattthomson.coursera.ggp.gresley.parser

import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser.Statement

class Fact(val name: String, val terms: Seq[Term]) {
  lazy val isComplete: Boolean = terms.forall(_.isInstanceOf[LiteralTerm])

  def substitute(values: Map[String, String]) = updateTerms(terms.map(_.substitute(values)))

  def matches(completeFact: Fact, values: Map[String, String]) = {
    if (!completeFact.isComplete) throw new IllegalArgumentException("Can't match against a partially defined fact")
    else if (completeFact.name != name) None
    else if (completeFact.terms.size != terms.size) None
    else {
      val substituted = substitute(values)
      val zippedTerms = substituted.terms.zip(completeFact.terms).map { case (t1, t2) => t1.matches(t2) }

      if (zippedTerms.contains(None)) None
      else {
        val newValues = zippedTerms.flatten.flatten
        val updatedValues = (newValues ++ values)
          .groupBy { case (v, _) => v }
          .mapValues { values => values.map { case (_, v) => v }.toSet }

        if (updatedValues.exists { case (x, vs) => vs.size > 1 }) None
        else Some(updatedValues.mapValues(_.head))
      }
    }
  }

  protected def updateTerms(terms: Seq[Term]) = new Fact(name, terms)

  override def toString = s"Fact($name, $terms)"
}

case class Role(private val nameTerm: Term) extends Fact("role", Seq(nameTerm)) with Statement {
  override protected def updateTerms(terms: Seq[Term]) = Role(terms.head)
}

case class Relation(override val name: String, override val terms: Seq[Term]) extends Fact(name, terms) with Statement {
  override protected def updateTerms(terms: Seq[Term]) = Relation(name, terms)
}

object Fact {
  def apply(name: String, terms: Seq[Term]) = new Fact(name, terms)

  def unapply(fact: Fact) = Some((fact.name, fact.terms))
}
