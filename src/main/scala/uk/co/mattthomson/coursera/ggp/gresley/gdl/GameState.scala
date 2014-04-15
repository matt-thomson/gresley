package uk.co.mattthomson.coursera.ggp.gresley.gdl

class GameState(private val game: GameDescription, val facts: Set[Fact]) {
  def legalActions(role: String): Set[Action] = {
    propagateConditionals(game.constantFacts, Map(), game.legalMoveRules)
      .collect { case Legal(Role(LiteralTerm(`role`)), action) => action }
  }

  def update(actions: Seq[Action]): GameState = {
    val facts = propagateConditionals(game.constantFacts, game.roles.map(Role(_)).zip(actions).toMap, game.nextMoveRules)
      .collect { case Next(fact) => fact }

    new GameState(game, facts)
  }

  private def propagateConditionals(facts: Set[Fact], moves: Map[Role, Action], conditionals: Set[Conditional]): Set[Fact] = {
    val updatedFacts = conditionals.foldLeft(facts) { case (f, conditional) => conditional.propagate(f, moves, Some(this)) }
    if (facts == updatedFacts) facts else propagateConditionals(updatedFacts, moves, conditionals)
  }
}
