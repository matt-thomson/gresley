package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.{ActorLogging, Actor}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameState, GameDescription}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{SelectMove, PlayersMoved, NewGame}
import scala.util.Random

class RandomPlayer extends Actor with ActorLogging {
  override def receive = {
    case NewGame(game, role) => context.become(handle(game, role, game.initialState))
  }

  def handle(game: GameDescription, role: String, state: GameState): Receive = {
    case PlayersMoved(moves) => context.become(handle(game, role, state.update(moves)))
    case SelectMove(source) =>
      log.info(s"Current state:\n${state.trueFacts.mkString("\n")}")
      log.info(s"Legal actions:\n${state.legalActions(role).mkString("\n")}")

      val chosenAction = Random.shuffle(state.legalActions(role)).head
      log.info(s"Chosen action: $chosenAction")

      source ! chosenAction
  }
}
