package uk.co.mattthomson.coursera.ggp.gresley.gdl

case class Conditional(conclusion: Fact, conditions: Seq[Condition]) extends Statement {
  def propagate(facts: Set[Fact], state: Option[GameState]): Set[Fact] = {
    def matchCondition(values: Set[Map[String, String]], condition: Condition) = values.flatMap(condition.matches(facts, state))

    val values = conditions.foldLeft(Set[Map[String, String]](Map()))(matchCondition)
    val newFacts = values.toSet.map(conclusion.substitute)
    (facts ++ newFacts).toSet
  }
}
