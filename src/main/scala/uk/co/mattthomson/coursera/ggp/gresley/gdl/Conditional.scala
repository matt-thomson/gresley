package uk.co.mattthomson.coursera.ggp.gresley.gdl

case class Conditional(conclusion: Fact, conditions: Seq[Fact]) extends Statement {
  def propagate(facts: Set[Fact]): Set[Fact] = {
    def matchCondition(facts: Set[Fact])(values: Set[Map[String, String]], condition: Fact) = (for {
      f <- facts
      v <- values
      m <- condition.matches(f, v)
    } yield m).toSet

    val values = conditions.foldLeft(Set[Map[String, String]](Map()))(matchCondition(facts))
    val newFacts = values.map(conclusion.substitute)
    (facts ++ newFacts).toSet
  }
}
