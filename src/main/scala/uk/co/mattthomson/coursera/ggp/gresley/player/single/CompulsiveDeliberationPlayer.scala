package uk.co.mattthomson.coursera.ggp.gresley.player.single

import uk.co.mattthomson.coursera.ggp.gresley.player.Player
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, GameState}
import akka.actor.ActorRef

class CompulsiveDeliberationPlayer extends Player[Unit] {
  override def initialize(game: GameDescription, role: String) = ()

  override def play(state: GameState, role: String, source: ActorRef, playerState: Unit) = {
    val chosenAction = bestMove(state, role)
    log.info(s"Chosen action: $chosenAction")

    source ! chosenAction
  }

  private def maxScore(state: GameState, role: String): Int = {
    if (state.isTerminal) state.value(role)
    else state.legalActions(role).map { action =>
      maxScore(state.update(Map(role -> action)), role)
    }.max
  }

  private def bestMove(state: GameState, role: String) = {
    state.legalActions(role).maxBy { action => maxScore(state.update(Map(role -> action)), role) }
  }
}
