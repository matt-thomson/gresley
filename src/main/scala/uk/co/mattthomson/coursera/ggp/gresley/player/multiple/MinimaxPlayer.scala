package uk.co.mattthomson.coursera.ggp.gresley.player.multiple

import uk.co.mattthomson.coursera.ggp.gresley.player.Player
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Action, GameState, GameDescription}
import akka.actor.ActorRef

class MinimaxPlayer extends Player[String] {
  override def initialize(game: GameDescription, role: String) = game.roles.filter(_ != role).head

  override def play(state: GameState, role: String, source: ActorRef, otherRole: String) = {
    val chosenAction = bestMove(state, role, otherRole)
    log.info(s"Chosen action: $chosenAction")

    source ! chosenAction
    otherRole
  }

  private def bestMove(state: GameState, role: String, otherRole: String) = {
    state.legalActions(role).maxBy(minScore(state, role, otherRole))
  }

  private def minScore(state: GameState, role: String, otherRole: String)(action: Action): Int = {
    val otherActions = state.legalActions(otherRole)
    val states: Set[GameState] = otherActions.map { otherAction =>
      val actions = Map(role -> action, otherRole -> otherAction)
      state.update(actions)
    }

    states.map(maxScore(role, otherRole)).min
  }

  private def maxScore(role: String, otherRole: String)(state: GameState): Int = {
    if (state.isTerminal) state.value(role)
    else state.legalActions(role).map(minScore(state, role, otherRole)).max
  }
}
