package uk.co.mattthomson.coursera.ggp.gresley.moveselector

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Props, ActorRef, ActorSystem}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, GameState}
import scala.concurrent.duration._
import scala.io.Source
import uk.co.mattthomson.coursera.ggp.gresley.player.Player._
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Play
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialized
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialize

abstract class MoveSelectorSpec extends TestKit(ActorSystem("TestActorSystem")) with FlatSpec with ImplicitSender with ShouldMatchers {
  protected def playGame(gameName: String, moveSelectorProps: Seq[Props]): GameState = {
    def play(game: GameDescription, state: GameState, players: Map[String, (ActorRef, Any)]): GameState = {
      if (state.isTerminal) state
      else {
        val selectedMoves = players.map { case (role, (player, playerState)) =>
          player ! Play(game, state, role, playerState)
          (role, receiveOne(10.seconds).asInstanceOf[SelectedMove])
        }.toMap

        val actions = selectedMoves.map { case (role, SelectedMove(action, _)) => (role, action)}.toMap
        val updatedState = state.update(actions)

        val updatedPlayers = selectedMoves.map { case (role, SelectedMove(_, playerState)) =>
          players(role) match {
            case (player, _) => (role, (player, playerState))
          }
        }.toMap

        play(game, updatedState, updatedPlayers)
      }
    }

    val game = GameDescription(Source.fromFile(s"src/test/resources/games/$gameName.kif").mkString)
    val players = game.roles.zip(moveSelectorProps.map { system.actorOf }).map { case (role, player) =>
      player ! Initialize(game, role)
      val playerState = receiveOne(10.seconds).asInstanceOf[Initialized].playerState
      (role, (player, playerState))
    }.toMap

    play(game, game.initialState, players)
  }
}
