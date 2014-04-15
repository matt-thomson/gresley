package uk.co.mattthomson.coursera.ggp.gresley.gdl

class GameState(private val game: GameDescription, val facts: Set[Fact]) {
  def legalActions(role: String): Set[Action] = {
    propagateConditionals(game.constantFacts, game.legalMoveRules)
      .collect { case Legal(Role(LiteralTerm(`role`)), action) => action }
  }

  private def propagateConditionals(simpleFacts: Set[Fact], conditionals: Set[Conditional]): Set[Fact] = {
    val updatedFacts = conditionals.foldLeft(simpleFacts) { case (f, conditional) => conditional.propagate(f, Some(this)) }
    if (simpleFacts == updatedFacts) simpleFacts else propagateConditionals(updatedFacts, conditionals)
  }
}
