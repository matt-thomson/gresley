package uk.co.mattthomson.coursera.ggp.gresley.parser

import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser._

class GameDescription(private val statements: Seq[Statement]) {
  lazy val roles = statements.collect { case Role(LiteralTerm(role)) => role }

  lazy val facts = {
    val simpleFacts: Set[Fact] = statements.collect { case f: Fact => f }.toSet
    val conditionalFacts = statements.collect { case c: Conditional => c }.toSet

    propagateConditionals(simpleFacts, conditionalFacts)
  }

  private def propagateConditionals(simpleFacts: Set[Fact], conditionalFacts: Set[Conditional]): Set[Fact] = {
    def matchCondition(facts: Set[Fact])(values: Set[Map[String, String]], condition: Fact): Set[Map[String, String]] = (for {
      f <- facts
      v <- values
      m <- condition.matches(f, v)
    } yield m).toSet

    def propagateConditional(facts: Set[Fact], conditional: Conditional): Set[Fact] = {
      val values = conditional.conditions.foldLeft(Set[Map[String, String]](Map()))(matchCondition(facts))
      val newFacts = values.map(conditional.conclusion.substitute)
      (facts ++ newFacts).toSet
    }

    val updatedFacts = conditionalFacts.foldLeft(simpleFacts)(propagateConditional)
    
    if (simpleFacts == updatedFacts) simpleFacts else propagateConditionals(updatedFacts, conditionalFacts)
  }
}

object GameDescription {
  def apply(statements: Seq[Statement]): GameDescription = new GameDescription(statements)
}
