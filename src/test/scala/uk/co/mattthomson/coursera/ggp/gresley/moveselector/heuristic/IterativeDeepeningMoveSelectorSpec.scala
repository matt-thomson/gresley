package uk.co.mattthomson.coursera.ggp.gresley.moveselector.heuristic

import akka.actor.{PoisonPill, ActorSystem, Props}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameDescription, GameState}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{Initialized, Initialize, SelectedMove, Play}
import org.joda.time.DateTime
import scala.io.Source
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.flatspec._
import org.scalatest.matchers._
import scala.concurrent.duration._

class IterativeDeepeningMoveSelectorSpec extends TestKit(ActorSystem("TestActorSystem")) with AnyFlatSpecLike with ImplicitSender with should.Matchers {
  "The iterative deepening move selector" should "play Hunter" ignore {
    val finalState = playGame("hunter", List(Props(new IterativeDeepeningMoveSelector)))

    finalState.value("robot") should be (64)
  }

  protected def playGame(gameName: String, moveSelectorProps: Seq[Props]): GameState = {
    def play(game: GameDescription, state: GameState, players: Map[String, (Props, Any)]): GameState = {
      if (state.isTerminal) state
      else {
        val selectedMoves = players.map { case (role, (props, metadata)) =>
          val player = system.actorOf(props)
          player ! Play(game, state, role, DateTime.now().plus(10000), metadata)

          val received = receiveWhile(max = 10.seconds){ case s: SelectedMove => s }
          player ! PoisonPill

          (role, received.last)
        }.toMap

        val actions = selectedMoves.map { case (role, SelectedMove(action)) => (role, action) }.toMap
        val updatedState = state.update(actions)

        play(game, updatedState, players)
      }
    }

    val game = GameDescription(Source.fromFile(s"src/test/resources/games/$gameName.kif").mkString)
    val players = game.roles.zip(moveSelectorProps).map { case (role, props) =>
      val player = system.actorOf(props)
      player ! Initialize(game, role, DateTime.now().plus(10000))

      val metadata = receiveOne(10.seconds).asInstanceOf[Initialized].metadata
      player ! PoisonPill

      (role, (props, metadata))
    }.toMap

    play(game, game.initialState, players)
  }
}
