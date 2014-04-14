package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.{ActorRef, Props, Actor}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{SelectMove, GamesInProgress}
import uk.co.mattthomson.coursera.ggp.gresley.gdl._
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Action
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Stop
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Start
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Abort

class GameManager(playerProps: Props) extends Actor {
  override def receive: Receive = handle(Map())

  private def handle(players: Map[String, ActorRef]): Receive = {
    case Info =>
      sender ! "ready"
    case Start(id, _, game, _, _) =>
      val player = context.actorOf(playerProps, s"player-$id")
      player ! game
      sender ! "ready"
      context.become(handle(players + (id -> player)))
    case Play(id, moves) =>
      val player = players(id)
      player ! SelectMove(moves, sender)
    case Stop(id, _) =>
      sender ! "done"
      context.become(handle(players - id))
    case Abort(id) =>
      sender ! "done"
      context.become(handle(players - id))

    case action: Action => context.parent ! action

    case GamesInProgress => sender ! players.keys.toList
  }
}

object GameManager {
  case class SelectMove(moves: Option[Seq[String]], source: ActorRef)

  case object GamesInProgress
}
