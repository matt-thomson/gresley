package uk.co.mattthomson.coursera.ggp.gresley.player.multiple

import uk.co.mattthomson.coursera.ggp.gresley.player.{LegalPlayer, PlayerSpec}
import akka.actor.Props

class MinimaxPlayerSpec extends PlayerSpec {
  "The minimax player" should "play Tic Tac Toe 3" in {
    val player = system.actorOf(Props[MinimaxPlayer])
    val opponent = system.actorOf(Props[LegalPlayer])

    val finalState = playGame("tictactoe3", List(player, opponent))

    finalState.value("white") should be (50)
  }

  it should "play Tic Tac Toe 5" in {
    val player = system.actorOf(Props[MinimaxPlayer])
    val opponent = system.actorOf(Props[LegalPlayer])

    val finalState = playGame("tictactoe5", List(player, opponent))

    finalState.value("white") should be (100)
  }

  it should "play Tic Tac Toe 7" in {
    val player = system.actorOf(Props[MinimaxPlayer])
    val opponent = system.actorOf(Props[LegalPlayer])

    val finalState = playGame("tictactoe7", List(player, opponent))

    finalState.value("white") should be (100)
  }
}