package uk.co.mattthomson.coursera.ggp.gresley.moveselector.simple

import akka.actor.{Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{SelectedMove, Play, Initialized, Initialize}

class LegalMoveSelector extends Actor with ActorLogging {
  override def receive: Receive = {
    case Initialize(game, role) => sender ! Initialized(())
    case Play(_, state, role, _, _) =>
      val chosenAction = state.legalActions(role).head
      log.info(s"Chosen action: $chosenAction")

      sender ! SelectedMove(chosenAction)
  }
}
