package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.{PoisonPill, ActorRef, Props, Actor}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{NewGame, SelectMove, GamesInProgress}
import uk.co.mattthomson.coursera.ggp.gresley.gdl._
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Action
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Stop
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Start
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Abort
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorPropsFactory
import scala.concurrent.duration.FiniteDuration

class GameManager(playerFactory: Seq[Props] => Actor, moveSelectorPropsFactory: MoveSelectorPropsFactory) extends Actor {
  override def receive: Receive = handle(Map())

  private def handle(players: Map[String, (ActorRef, FiniteDuration)]): Receive = {
    case Info =>
      sender ! "((name gresley) (status available))"
    case Start(id, role, game, startClock, playClock) =>
      val moveSelectorProps = moveSelectorPropsFactory.forGame(game)
      val player = context.actorOf(Props(playerFactory(moveSelectorProps)), s"player-$id")
      player ! NewGame(game, role, sender, startClock)
      context.become(handle(players + (id -> (player, playClock))))
    case Play(id, moves) =>
      val (player, playClock) = players(id)
      player ! SelectMove(sender, moves, playClock)
    case Stop(id, _) =>
      sender ! "done"
      val (player, _) = players(id)
      player ! PoisonPill
      context.become(handle(players - id))
    case Abort(id) =>
      sender ! "done"
      val (player, _) = players(id)
      player ! PoisonPill
      context.become(handle(players - id))

    case action: Action => context.parent ! action

    case GamesInProgress => sender ! players.keys.toList
  }
}

object GameManager {
  case class NewGame(game: GameDescription, role: String, source: ActorRef, timeout: FiniteDuration)

  case class SelectMove(source: ActorRef, moves: Option[Seq[Action]], timeout: FiniteDuration)

  case object GamesInProgress
}
