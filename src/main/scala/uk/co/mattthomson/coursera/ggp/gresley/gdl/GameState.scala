package uk.co.mattthomson.coursera.ggp.gresley.gdl

class GameState(private val game: GameDescription, val trueFacts: Set[Fact]) {
  def legalActions(role: String) = {
    propagateConditionals(game.constantFacts, Map(), game.legalMoveRules)
      .collect { case Legal(Role(LiteralTerm(`role`)), action) => action }
  }

  def update(actions: Seq[Action]) = {
    val facts = propagateConditionals(game.constantFacts, game.roles.map(Role(_)).zip(actions).toMap, game.nextMoveRules)
      .collect { case Next(fact) => fact }

    new GameState(game, facts)
  }

  lazy val falseFacts = game.baseFacts -- trueFacts

  lazy val isTerminal = propagateConditionals(game.constantFacts, Map(), game.terminalRules).contains(Terminal)

  def value(role: String) = propagateConditionals(game.constantFacts, Map(), game.goalRules)
    .collect { case g @ Goal(Role(LiteralTerm(`role`)), _) => g.value }
    .headOption
    .getOrElse(0)

  private def propagateConditionals(facts: Set[Fact], moves: Map[Role, Action], conditionals: Set[Conditional]): Set[Fact] = {
    val updatedFacts = conditionals.foldLeft(facts) { case (f, conditional) => conditional.propagate(f, moves, Some(this)) }
    if (facts == updatedFacts) facts else propagateConditionals(updatedFacts, moves, conditionals)
  }
}
