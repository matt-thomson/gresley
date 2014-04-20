package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{ActorRef, ActorSystem}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, Action, GameState}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{NewGame, PlayersMoved, SelectMove}
import scala.concurrent.duration._
import scala.io.Source

abstract class PlayerSpec extends TestKit(ActorSystem("TestActorSystem")) with FlatSpec with ImplicitSender with ShouldMatchers {
  protected def playGame(gameName: String, players: Seq[ActorRef]): GameState = {
    def play(game: GameDescription, state: GameState): GameState = {
      if (state.isTerminal) state
      else {
        val actions = players.map { player =>
          player ! SelectMove(self)
          receiveOne(1.minute).asInstanceOf[Action]
        }

        players.foreach { player => player ! PlayersMoved(actions) }

        play(game, state.update(game.roles.zip(actions).toMap))
      }
    }

    val game = GameDescription(Source.fromFile(s"src/test/resources/games/$gameName.kif").mkString)
    game.roles.zip(players).map { case (role, player) => player ! NewGame(game, role) }

    play(game, game.initialState)
  }
}
