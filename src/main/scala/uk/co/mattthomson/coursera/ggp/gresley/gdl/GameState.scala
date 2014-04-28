package uk.co.mattthomson.coursera.ggp.gresley.gdl

case class GameState(game: GameDescription, trueFacts: Set[Fact]) {
  private lazy val stateFacts = propagateConditionals(game.constantFacts, Map(), game.stateRules)

  def legalActions(role: String) = {
    propagateConditionals(stateFacts, Map(), game.legalMoveRules)
      .collect { case Legal(Role(LiteralTerm(`role`)), action) => action }
  }

  def update(actions: Map[String, Action]) = {
    val actionsWithRoles: Map[Role, Action] = actions.map { case (role, action) => (Role(role), action) }.toMap
    val facts = propagateConditionals(stateFacts, actionsWithRoles, game.nextStateRules)
      .collect { case Next(fact) => fact }

    new GameState(game, facts)
  }

  lazy val falseFacts = game.baseFacts -- trueFacts

  lazy val isTerminal = {
    propagateConditionals(stateFacts, Map(), game.terminalRules).contains(Terminal)
  }

  def value(role: String) = propagateConditionals(stateFacts, Map(), game.goalRules)
    .collect { case g @ Goal(Role(LiteralTerm(`role`)), _) => g.value }
    .headOption
    .getOrElse(0)

  private def propagateConditionals(facts: Set[Fact], moves: Map[Role, Action], conditionals: Set[Conditional]): Set[Fact] = {
    val updatedFacts = conditionals.foldLeft(facts) { case (f, conditional) => conditional.propagate(f, moves, Some(this)) }
    if (facts == updatedFacts) facts else propagateConditionals(updatedFacts, moves, conditionals)
  }
}
