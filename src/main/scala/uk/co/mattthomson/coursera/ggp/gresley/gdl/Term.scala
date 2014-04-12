package uk.co.mattthomson.coursera.ggp.gresley.gdl

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

  override def toString = name
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

  override def toString = s"?$name"
}

object Term {
  implicit def stringToLiteral(name: String) = LiteralTerm(name)

  def matchTerms(variableTerms: Seq[Term], completeTerms: Seq[Term], values: Map[String, String]) = {
    if (!completeTerms.forall(_.isInstanceOf[LiteralTerm])) throw new IllegalArgumentException("Complete terms not complete")
    if (completeTerms.size != variableTerms.size) None
    else {
      val substituted = variableTerms.map { term => term.substitute(values) }
      val zippedTerms = substituted.zip(completeTerms).map { case (t1, t2) => t1.matches(t2) }

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
}