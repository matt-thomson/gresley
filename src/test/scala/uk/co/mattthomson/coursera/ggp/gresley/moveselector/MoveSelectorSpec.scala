package uk.co.mattthomson.coursera.ggp.gresley.moveselector

import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Props, ActorRef, ActorSystem}
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, Action, GameState}
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.{NewGame, PlayersMoved, SelectMove}
import scala.concurrent.duration._
import scala.io.Source
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Ready
import uk.co.mattthomson.coursera.ggp.gresley.player.Player

abstract class MoveSelectorSpec extends TestKit(ActorSystem("TestActorSystem")) with FlatSpec with ImplicitSender with ShouldMatchers {
  protected def playGame(gameName: String, moveSelectorProps: Seq[Props]): GameState = {
    def play(game: GameDescription, state: GameState, players: Seq[ActorRef]): GameState = {
      if (state.isTerminal) state
      else {
        val actions = players.map { player =>
          player ! SelectMove(self)
          receiveOne(10.seconds).asInstanceOf[Action]
        }

        players.foreach { player => player ! PlayersMoved(actions) }

        play(game, state.update(game.roles.zip(actions).toMap), players)
      }
    }

    val game = GameDescription(Source.fromFile(s"src/test/resources/games/$gameName.kif").mkString)
    val players = moveSelectorProps.map { case p => system.actorOf(Props(new Player(p))) }
    game.roles.zip(players).foreach { case (role, player) =>
      player ! NewGame(game, role, self)
      expectMsg(Ready)
    }

    play(game, game.initialState, players)
  }
}
