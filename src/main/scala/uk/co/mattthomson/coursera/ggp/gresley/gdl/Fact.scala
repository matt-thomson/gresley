package uk.co.mattthomson.coursera.ggp.gresley.gdl

trait Fact extends Statement {
  def substitute(values: Map[String, String]): Fact

  def matches(completeFact: Fact, values: Map[String, String]): Option[Map[String, String]]
}

trait ConstantFact extends Fact

case class Role(private val nameTerm: Term) extends ConstantFact {
  override def substitute(values: Map[String, String]) = Role(nameTerm.substitute(values))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Role(otherTerm) => nameTerm.substitute(values).matches(otherTerm.substitute(values)).map(_ ++ values)
    case _ => None
  }
}

case class Relation(name: String, terms: Seq[Term]) extends ConstantFact {
  override def substitute(values: Map[String, String]) = Relation(name, terms.map(_.substitute(values)))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Relation(otherName, otherTerms) => if (name != otherName) None else Term.matchTerms(terms, otherTerms, values)
    case _ => None
  }
}

case class Base(fact: Fact) extends ConstantFact {
  override def substitute(values: Map[String, String]) = Base(fact.substitute(values))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Base(otherFact) => fact.matches(otherFact, values)
    case _ => None
  }
}

case class Action(name: String, terms: Seq[Term]) extends Fact {
  override def substitute(values: Map[String, String]) = Action(name, terms.map(_.substitute(values)))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Action(otherName, otherTerms) => if (name != otherName) None else Term.matchTerms(terms, otherTerms, values)
    case _ => None
  }
}

case class Input(role: Role, action: Action) extends ConstantFact {
  override def substitute(values: Map[String, String]) = Input(role.substitute(values), action.substitute(values))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Input(otherRole, otherAction) =>
      action.matches(otherAction, values) match {
        case Some(v) => role.matches(otherRole, values ++ v)
        case None => None
      }
    case _ => None
  }
}

case class Init(fact: Fact) extends Fact {
  override def substitute(values: Map[String, String]) = Init(fact.substitute(values))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Init(otherFact) => fact.matches(otherFact, values)
    case _ => None
  }
}

case class Legal(role: Role, action: Action) extends Fact {
  override def substitute(values: Map[String, String]) = Legal(role.substitute(values), action.substitute(values))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Legal(otherRole, otherAction) =>
      action.matches(otherAction, values) match {
        case Some(v) => role.matches(otherRole, values ++ v)
        case None => None
      }
    case _ => None
  }
}

case class Next(fact: Fact) extends Fact {
  override def substitute(values: Map[String, String]) = Next(fact.substitute(values))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Next(otherFact) => fact.matches(otherFact, values)
    case _ => None
  }
}

case class Goal(role: Role, score: Term) extends Fact {
  override def substitute(values: Map[String, String]) = Goal(role.substitute(values), score.substitute(values))

  override def matches(completeFact: Fact, values: Map[String, String]) = completeFact match {
    case Goal(otherRole, otherScore) =>
      score.substitute(values).matches(otherScore.substitute(values)) match {
        case Some(v) => role.matches(otherRole, values ++ v)
        case None => None
      }
    case _ => None
  }

  lazy val value = score match {
    case LiteralTerm(s) => s.toInt
    case _ => throw new IllegalArgumentException("Can only get the value of a literal goal")
  }
}

case object Terminal extends Fact {
  override def substitute(values: Map[String, String]): Fact = Terminal

  override def matches(completeFact: Fact, values: Map[String, String]): Option[Map[String, String]] = completeFact match {
    case Terminal => Some(values)
    case _ => None
  }
}