package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.Actor
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Action, GameState, GameDescription}

class LegalPlayer extends Actor {
  override def receive = {
    case g: GameDescription => context.become(handle(g, g.initialState))
  }

  def handle(game: GameDescription, state: GameState): Receive = {
    case _ => sender ! Action("left")
  }
}
