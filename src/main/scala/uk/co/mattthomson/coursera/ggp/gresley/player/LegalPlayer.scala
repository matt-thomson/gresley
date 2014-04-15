package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.Actor
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameState, GameDescription}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{NewGame, SelectMove}

class LegalPlayer extends Actor {
  override def receive = {
    case NewGame(game, role) => context.become(handle(game, role, game.initialState))
  }

  def handle(game: GameDescription, role: String, state: GameState): Receive = {
    case SelectMove(_, source) => source ! game.actions(role).head
  }
}
