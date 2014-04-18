package uk.co.mattthomson.coursera.ggp.gresley.player

import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{SelectMove, PlayersMoved, NewGame}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameState, GameDescription}
import akka.actor.{ActorLogging, Actor, ActorRef}

abstract class Player extends Actor with ActorLogging {
  override def receive = {
    case NewGame(game, role) => context.become(handle(game, role, game.initialState))
  }

  def handle(game: GameDescription, role: String, state: GameState): Receive = {
    case PlayersMoved(moves) => context.become(handle(game, role, state.update(moves)))
    case SelectMove(source) =>
      log.info(s"Current state:\n${state.trueFacts.mkString("\n")}")
      log.info(s"Legal actions:\n${state.legalActions(role).mkString("\n")}")

      play(state, role, source)
  }

  def play(state: GameState, role: String, source: ActorRef)
}
