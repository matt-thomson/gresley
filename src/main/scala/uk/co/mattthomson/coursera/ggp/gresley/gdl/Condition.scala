package uk.co.mattthomson.coursera.ggp.gresley.gdl

trait Condition {
  def matches(completeFact: Fact, values: Map[String, String]): Option[Map[String, String]]
}

case class FactCondition(fact: Fact) extends Condition {
  override def matches(completeFact: Fact, values: Map[String, String]) = fact.matches(completeFact, values)
}

case class StateCondition(fact: Fact) extends Condition {
  override def matches(completeFact: Fact, values: Map[String, String]) = ???
}
