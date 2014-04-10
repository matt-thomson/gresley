package uk.co.mattthomson.coursera.ggp.gresley.parser

class GameDescription(private val statements: Seq[Statement]) {
  lazy val constantFacts = {
    val simpleFacts: Set[Fact] = statements.collect { case f: Fact => f }.toSet
    val conditionalFacts = statements.collect { case c: Conditional => c }.toSet

    propagateConditionals(simpleFacts, conditionalFacts)
  }

  lazy val roles = constantFacts.collect { case Role(LiteralTerm(role)) => role }

  def inputs(role: String) = constantFacts.collect {
    case Input(Role(LiteralTerm(`role`)), Action(LiteralTerm(action))) => action
  }

  private def propagateConditionals(simpleFacts: Set[Fact], conditionalFacts: Set[Conditional]): Set[Fact] = {
    val updatedFacts = conditionalFacts.foldLeft(simpleFacts) { case (f, conditional) => conditional.propagate(f) }
    if (simpleFacts == updatedFacts) simpleFacts else propagateConditionals(updatedFacts, conditionalFacts)
  }
}

object GameDescription {
  def apply(statements: Seq[Statement]): GameDescription = new GameDescription(statements)

  def apply(gdl: String): GameDescription = {
    val parser = new GdlParser
    val statements = parser.parseAll(parser.game, gdl)
    GameDescription(statements.get)
  }
}
