package uk.co.mattthomson.coursera.ggp.gresley.gdl

trait Condition {
  def matches(completeFacts: Map[FactTag, Set[Fact]], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]): Set[Map[String, String]]
}

case class FactCondition(fact: Fact) extends Condition {
  override def matches(completeFacts: Map[FactTag, Set[Fact]], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]) =
    completeFacts.getOrElse(fact.tag, Set()).flatMap(fact.matches(_, values))
}

case class StateCondition(fact: Fact) extends Condition {
  override def matches(completeFacts: Map[FactTag, Set[Fact]], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]) = state match {
    case Some(s) => s.trueFacts.flatMap(fact.matches(_, values))
    case None => Set()
  }
}

case class ActionCondition(role: Role, action: Action) extends Condition {
  override def matches(completeFacts: Map[FactTag, Set[Fact]], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]) =
    moves.flatMap { case (r, a) => role.matches(r, values).flatMap(action.matches(a, _)) }.toSet
}

case class FalseCondition(condition: Condition) extends Condition {
  override def matches(completeFacts: Map[FactTag, Set[Fact]], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]) = condition match {
    case FactCondition(fact) => if (completeFacts.getOrElse(fact.tag, Set()).contains(fact.substitute(values))) Set() else Set(values)
    case StateCondition(fact) => state match {
      case Some(s) => s.falseFacts.flatMap(fact.matches(_, values))
      case None => Set()
    }
  }
}

case class OrCondition(condition: Seq[Condition]) extends Condition {
  override def matches(completeFacts: Map[FactTag, Set[Fact]], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]): Set[Map[String, String]] =
    condition.flatMap(_.matches(completeFacts, moves, state)(values)).toSet
}

case class DistinctCondition(terms: Seq[Term]) extends Condition {
  override def matches(completeFacts: Map[FactTag, Set[Fact]], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]) = {
    val substituted = terms.map(_.substitute(values))
    if (substituted.toSet.size == substituted.size) Set(values) else Set()
  }
}