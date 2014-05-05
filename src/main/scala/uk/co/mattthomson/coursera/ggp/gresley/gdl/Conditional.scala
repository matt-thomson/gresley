package uk.co.mattthomson.coursera.ggp.gresley.gdl

case class Conditional(conclusion: Fact, conditions: Seq[Condition]) extends Statement {
  def propagate(facts: Map[FactTag, Set[Fact]], moves: Map[Role, Action], state: Option[GameState]): Map[FactTag, Set[Fact]] = {
    def matchCondition(values: Set[Map[String, String]], condition: Condition) = values.flatMap(condition.matches(facts, moves, state))

    val values = conditions.foldLeft(Set[Map[String, String]](Map()))(matchCondition)
    val newFacts = values.toSet.map(conclusion.substitute).groupBy(f => f.tag).toMap
    (newFacts.toSet ++ facts.toSet)
      .groupBy { case (c, _) => c}
      .map { case (c, fs) => (c, fs.map { case (_, f) => f}.flatten.toSet) }
      .toMap
  }

  def proves(fact: Fact, facts: Map[FactTag, Set[Fact]], moves: Map[Role, Action], state: Option[GameState]) = {
    def allTrue(conditions: Seq[Condition], values: Map[String, String]): Boolean = conditions match {
      case condition :: rest =>
        val newValues = condition.matches(facts, moves, state)(values)
        newValues.exists(v => allTrue(rest, v))
      case Nil => true
    }

    val values = conclusion.matches(fact, Map())
    values.exists(v => allTrue(conditions, v))
  }
}
