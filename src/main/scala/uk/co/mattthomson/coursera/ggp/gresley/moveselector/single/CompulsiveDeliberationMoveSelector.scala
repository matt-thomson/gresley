package uk.co.mattthomson.coursera.ggp.gresley.moveselector.single

import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameState
import akka.actor.{Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{SelectedMove, Play, Initialized, Initialize}

class CompulsiveDeliberationMoveSelector extends Actor with ActorLogging {
  override def receive: Receive = {
    case Initialize(game, role) => sender ! Initialized(())
    case Play(_, state, role, _) =>
      val chosenAction = bestMove(state, role)
      log.info(s"Chosen action: $chosenAction")

      sender ! SelectedMove(chosenAction)
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
