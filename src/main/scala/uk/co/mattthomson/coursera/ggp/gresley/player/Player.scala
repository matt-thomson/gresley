package uk.co.mattthomson.coursera.ggp.gresley.player

import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{SelectMove, PlayersMoved, NewGame}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameState, GameDescription}
import akka.actor.{ActorLogging, Actor, ActorRef}

abstract class Player[T] extends Actor with ActorLogging {
  override def receive = {
    case NewGame(game, role) =>
      val playerState = initialize(game, role)
      context.become(handle(game, role, game.initialState, playerState))
  }

  def handle(game: GameDescription, role: String, state: GameState, playerState: T): Receive = {
    case PlayersMoved(moves) =>
      val actions = game.roles.zip(moves).toMap
      context.become(handle(game, role, state.update(actions), playerState))
    case SelectMove(source) =>
      log.info(s"Current state:\n${state.trueFacts.mkString("\n")}")
      log.info(s"Legal actions:\n${state.legalActions(role).mkString("\n")}")

      val newPlayerState = play(state, role, source, playerState)
      context.become(handle(game, role, state, newPlayerState))
  }

  def initialize(game: GameDescription, role: String): T

  def play(state: GameState, role: String, source: ActorRef, playerState: T): T
}
