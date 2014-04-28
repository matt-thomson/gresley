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
import org.joda.time.DateTime

abstract class MoveSelectorSpec extends TestKit(ActorSystem("TestActorSystem")) with FlatSpec with ImplicitSender with ShouldMatchers {
  protected def playGame(gameName: String, moveSelectorProps: Seq[Props]): GameState = {
    def play(game: GameDescription, state: GameState, players: Map[String, (ActorRef, Any)]): GameState = {
      if (state.isTerminal) state
      else {
        val selectedMoves = players.map { case (role, (player, metadata)) =>
          player ! Play(game, state, role, DateTime.now().plus(10000), metadata)
          (role, receiveOne(10.seconds).asInstanceOf[SelectedMove])
        }.toMap

        val actions = selectedMoves.map { case (role, SelectedMove(action)) => (role, action) }.toMap
        val updatedState = state.update(actions)

        play(game, updatedState, players)
      }
    }

    val game = GameDescription(Source.fromFile(s"src/test/resources/games/$gameName.kif").mkString)
    val players = game.roles.zip(moveSelectorProps.map { system.actorOf }).map { case (role, player) =>
      player ! Initialize(game, role)
      val metadata = receiveOne(10.seconds).asInstanceOf[Initialized].metadata
      (role, (player, metadata))
    }.toMap

    play(game, game.initialState, players)
  }
}
