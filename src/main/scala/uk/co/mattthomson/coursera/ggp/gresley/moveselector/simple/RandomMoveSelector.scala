package uk.co.mattthomson.coursera.ggp.gresley.moveselector.simple

import akka.actor.{ActorLogging, Actor}
import scala.util.Random
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{SelectedMove, Play, Initialized, Initialize}

class RandomMoveSelector extends Actor with ActorLogging {
  override def receive: Receive = {
    case Initialize(_, _, _) => sender ! Initialized(())
    case Play(_, state, role, _, _) =>
      val chosenAction = Random.shuffle(state.game.actions(role)).find(state.isLegal(role)).get
      log.info(s"Chosen action: $chosenAction")

      sender ! SelectedMove(chosenAction)
  }
}
