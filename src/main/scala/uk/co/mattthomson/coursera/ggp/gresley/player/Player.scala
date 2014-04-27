package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor._
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Action, GameDescription, GameState}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player._
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{NewGame, PlayersMoved, SelectMove}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.PlayersMoved
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Action
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Play
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.NewGame
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialized
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialize
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.SelectMove
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.SelectedMove

class Player(moveSelectorProps: Props) extends Actor with ActorLogging {
  override def receive = {
    case NewGame(game, role, source) =>
      val moveSelector = context.actorOf(moveSelectorProps)
      moveSelector ! Initialize(game, role)

      context.become(awaitInitialize(game, role, source))
  }

  private def awaitInitialize(game: GameDescription, role: String, source: ActorRef): Receive = {
    case Initialized(playerState) =>
      sender ! PoisonPill
      source ! Ready
      context.become(handle(game, role, game.initialState, playerState))
  }

  private def handle(game: GameDescription, role: String, state: GameState, playerState: Any): Receive = {
    case PlayersMoved(moves) =>
      val actions = game.roles.zip(moves).toMap
      context.become(handle(game, role, state.update(actions), playerState))
    case SelectMove(source) =>
      log.info(s"Current state:\n${state.trueFacts.mkString("\n")}")
      log.info(s"Legal actions:\n${state.legalActions(role).mkString("\n")}")

      val moveSelector = context.actorOf(moveSelectorProps)
      moveSelector ! Play(game, state, role, playerState)
      context.become(awaitMove(game, role, state, source))
  }

  private def awaitMove(game: GameDescription, role: String, state: GameState, source: ActorRef): Receive = {
    case SelectedMove(action, playerState) =>
      source ! action
      sender ! PoisonPill
      context.become(handle(game, role, state, playerState))
  }
}

object Player {
  def apply(moveSelectorProps: Props): Player = new Player(moveSelectorProps)

  case class Initialize(game: GameDescription, role: String)

  case class Initialized(playerState: Any)

  case object Ready {
    override def toString = "ready"
  }

  case class Play(game: GameDescription, state: GameState, role: String, playerState: Any)

  case class SelectedMove(action: Action, newPlayerState: Any)
}