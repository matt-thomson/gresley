package uk.co.mattthomson.coursera.ggp.gresley.gdl

trait Condition {
  def matches(completeFacts: Set[Fact], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]): Set[Map[String, String]]
}

case class FactCondition(fact: Fact) extends Condition {
  override def matches(completeFacts: Set[Fact], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]) =
    completeFacts.flatMap(fact.matches(_, values))
}

case class StateCondition(fact: Fact) extends Condition {
  override def matches(completeFacts: Set[Fact], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]) =
    state.get.facts.flatMap(fact.matches(_, values))
}

case class ActionCondition(role: Role, action: Action) extends Condition {
  override def matches(completeFacts: Set[Fact], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]) =
    moves.flatMap { case (r, a) => role.matches(r, values).flatMap(action.matches(a, _)) }.toSet
}

case class DistinctCondition(terms: Seq[Term]) extends Condition {
  override def matches(completeFacts: Set[Fact], moves: Map[Role, Action], state: Option[GameState])(values: Map[String, String]) = {
    if (values.values.toSet.size == values.size) Set(values) else Set()
  }
}