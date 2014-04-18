package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.{PoisonPill, ActorRef, Props, Actor}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{PlayersMoved, NewGame, SelectMove, GamesInProgress}
import uk.co.mattthomson.coursera.ggp.gresley.gdl._
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Action
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Stop
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Start
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Abort

class GameManager(playerPropsFactory: PlayerPropsFactory) extends Actor {
  override def receive: Receive = handle(Map())

  private def handle(players: Map[String, ActorRef]): Receive = {
    case Info =>
      sender ! "((name gresley) (status available))"
    case Start(id, role, game, _, _) =>
      val playerProps = playerPropsFactory.forGame(game)
      val player = context.actorOf(playerProps, s"player-$id")
      player ! NewGame(game, role)
      sender ! "ready"
      context.become(handle(players + (id -> player)))
    case Play(id, moves) =>
      val player = players(id)

      moves match {
        case Some(m) => player ! PlayersMoved(m)
        case None =>
      }

      player ! SelectMove(sender)
    case Stop(id, _) =>
      sender ! "done"
      players(id) ! PoisonPill
      context.become(handle(players - id))
    case Abort(id) =>
      sender ! "done"
      players(id) ! PoisonPill
      context.become(handle(players - id))

    case action: Action => context.parent ! action

    case GamesInProgress => sender ! players.keys.toList
  }
}

object GameManager {
  case class NewGame(game: GameDescription, role: String)

  case class PlayersMoved(moves: Seq[Action])

  case class SelectMove(source: ActorRef)

  case object GamesInProgress
}
