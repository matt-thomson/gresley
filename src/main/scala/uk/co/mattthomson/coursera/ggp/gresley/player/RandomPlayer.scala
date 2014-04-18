package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.{ActorRef, ActorLogging, Actor}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameState, GameDescription}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{SelectMove, PlayersMoved, NewGame}
import scala.util.Random

class RandomPlayer extends Player {
  def play(state: GameState, role: String, source: ActorRef) {
    val chosenAction = Random.shuffle(state.legalActions(role)).head
    log.info(s"Chosen action: $chosenAction")

    source ! chosenAction
  }
}
