package uk.co.mattthomson.coursera.ggp.gresley.parser

class GameDescription(private val statements: Seq[Statement]) {
  lazy val roles = statements.collect { case Role(LiteralTerm(role)) => role }

  lazy val facts = {
    val simpleFacts: Set[Fact] = statements.collect { case f: Fact => f }.toSet
    val conditionalFacts = statements.collect { case c: Conditional => c }.toSet

    propagateConditionals(simpleFacts, conditionalFacts)
  }

  private def propagateConditionals(simpleFacts: Set[Fact], conditionalFacts: Set[Conditional]): Set[Fact] = {
    val updatedFacts = conditionalFacts.foldLeft(simpleFacts) { case (f, conditional) => conditional.propagate(f) }
    if (simpleFacts == updatedFacts) simpleFacts else propagateConditionals(updatedFacts, conditionalFacts)
  }
}

object GameDescription {
  def apply(statements: Seq[Statement]): GameDescription = new GameDescription(statements)
}
