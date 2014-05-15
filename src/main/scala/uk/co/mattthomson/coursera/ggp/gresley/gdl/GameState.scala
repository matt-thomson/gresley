package uk.co.mattthomson.coursera.ggp.gresley.gdl

import com.twitter.util.Memoize

case class GameState(game: GameDescription, trueFacts: Set[Fact]) {
  lazy val legalActions = Memoize(legalActionsUnmemoized)

  private def legalActionsUnmemoized(role: String) = {
    game.actions.getOrElse(role, List()).filter(isLegal(role))
  }

  def isLegal(role: String)(action: Action) = prove(Legal(Role(role), action), None)

  def update(actions: Map[String, Action]) = {
    val actionsWithRoles = actions.map { case (role, action) => (Role(role), action) }.toMap
    val facts = game.baseFacts.filter(f => prove(Next(f), Some(actionsWithRoles)))

    new GameState(game, facts)
  }

  lazy val isTerminal = prove(Terminal, None)

  lazy val value = Memoize(valueUnmemoized)

  private def valueUnmemoized(role: String) = game.possibleValues.getOrElse(role, Set())
    .map(v => Goal(Role(role), LiteralTerm(v)))
    .find(prove(_, None))
    .fold(0)(_.value)

  val prove = Memoize(proveUnmemoized)

  private def proveUnmemoized(input: (Fact, Option[Map[Role, Action]])) = {
    val (fact, actions) = input
    if (game.constantFacts.getOrElse(fact.tag, Set()).contains(fact)) true
    else game.boundRules.getOrElse(fact, Set()).exists(_.prove(fact, this, actions))
  }
}
