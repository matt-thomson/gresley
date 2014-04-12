package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.{ActorRef, Props, Actor}
import uk.co.mattthomson.coursera.ggp.gresley.protocol.{Abort, Stop, Start, Info}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.GamesInProgress

class GameManager(playerProps: Props) extends Actor {
  override def receive: Receive = handle(Map())

  private def handle(games: Map[String, ActorRef]): Receive = {
    case Info =>
      sender ! "ready"
    case Start(id, _, game, _, _) =>
      val player = context.actorOf(playerProps, s"player-$id")
      player ! game
      sender ! "ready"
      context.become(handle(games + (id -> player)))
    case Stop(id, _) =>
      sender ! "done"
      context.become(handle(games - id))
    case Abort(id) =>
      sender ! "done"
      context.become(handle(games - id))

    case GamesInProgress => sender ! games.keys.toList
  }
}

object GameManager {
  case object GamesInProgress
}
