package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.ActorRef
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameState

class LegalPlayer extends Player {
  override def play(state: GameState, role: String, source: ActorRef): Unit = {
    val chosenAction = state.legalActions(role).head
    log.info(s"Chosen action: $chosenAction")

    source ! chosenAction
  }
}
