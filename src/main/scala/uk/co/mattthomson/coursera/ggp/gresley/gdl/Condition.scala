package uk.co.mattthomson.coursera.ggp.gresley.gdl

trait Condition {
  def bindings(allFacts: Map[FactTag, Set[Fact]])(values: Map[String, String]): Set[Map[String, String]]

  def substitute(values: Map[String, String]): Condition

  def prove(state: GameState, actions: Option[Map[Role, Action]] = None): Boolean
}

case class FactCondition(fact: Fact) extends Condition {
  override def bindings(allFacts: Map[FactTag, Set[Fact]])(values: Map[String, String]) =
    allFacts.getOrElse(fact.tag, Set()).flatMap(fact.matches(_, values))

  override def substitute(values: Map[String, String]) = FactCondition(fact.substitute(values))

  override def prove(state: GameState, actions: Option[Map[Role, Action]]) = state.prove(fact, actions)
}

case class StateCondition(fact: Fact) extends Condition {
  override def bindings(allFacts: Map[FactTag, Set[Fact]])(values: Map[String, String]) =
    allFacts.getOrElse(classOf[Base], Set()).flatMap(Base(fact).matches(_, values))

  override def substitute(values: Map[String, String]) = StateCondition(fact.substitute(values))

  override def prove(state: GameState, actions: Option[Map[Role, Action]]) = state.trueFacts.contains(fact)
}

case class ActionCondition(role: Role, action: Action) extends Condition {
  override def bindings(allFacts: Map[FactTag, Set[Fact]])(values: Map[String, String]) =
    allFacts.getOrElse(classOf[Input], Set()).flatMap(Input(role, action).matches(_, values))

  override def substitute(values: Map[String, String]) = ActionCondition(role.substitute(values), action.substitute(values))

  override def prove(state: GameState, actions: Option[Map[Role, Action]]) = actions.flatMap(_.get(role)) match {
    case Some(`action`) => true
    case _ => false
  }
}

case class FalseCondition(condition: Condition) extends Condition {
  override def bindings(allFacts: Map[FactTag, Set[Fact]])(values: Map[String, String]) =
    condition.bindings(allFacts)(values)

  override def substitute(values: Map[String, String]) = FalseCondition(condition.substitute(values))

  override def prove(state: GameState, actions: Option[Map[Role, Action]]) = !condition.prove(state, actions)
}

case class OrCondition(conditions: Seq[Condition]) extends Condition {
  override def bindings(allFacts: Map[FactTag, Set[Fact]])(values: Map[String, String]) =
    conditions.flatMap(_.bindings(allFacts)(values)).toSet

  override def substitute(values: Map[String, String]) = OrCondition(conditions.map(_.substitute(values)))

  override def prove(state: GameState, actions: Option[Map[Role, Action]]) = conditions.exists(_.prove(state, actions))
}

case class DistinctCondition(terms: Seq[Term]) extends Condition {
  override def bindings(allFacts: Map[FactTag, Set[Fact]])(values: Map[String, String]) = {
    val substituted = terms.map(_.substitute(values))
    if (substituted.toSet.size == substituted.size) Set(values) else Set()
  }

  override def substitute(values: Map[String, String]) = DistinctCondition(terms.map(_.substitute(values)))

  override def prove(state: GameState, actions: Option[Map[Role, Action]]) = true
}
