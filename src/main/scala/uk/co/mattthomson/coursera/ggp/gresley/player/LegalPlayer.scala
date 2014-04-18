package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.ActorRef
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, GameState}

class LegalPlayer extends Player[Unit] {
  override def initialize(game: GameDescription, role: String) = ()

  override def play(state: GameState, role: String, source: ActorRef, playerState: Unit): Unit = {
    val chosenAction = state.legalActions(role).head
    log.info(s"Chosen action: $chosenAction")

    source ! chosenAction
  }
}
