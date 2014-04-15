package uk.co.mattthomson.coursera.ggp.gresley.gdl

case class Conditional(conclusion: Fact, conditions: Seq[Condition]) extends Statement {
  def propagate(facts: Set[Fact]): Set[Fact] = {
    def matchCondition(values: Set[Map[String, String]], condition: Condition) = (for {
      f <- facts
      v <- values
      m <- condition.matches(f, v)
    } yield m).toSet

    val values = conditions.foldLeft(Set[Map[String, String]](Map()))(matchCondition)
    val newFacts = values.map(conclusion.substitute)
    (facts ++ newFacts).toSet
  }
}
