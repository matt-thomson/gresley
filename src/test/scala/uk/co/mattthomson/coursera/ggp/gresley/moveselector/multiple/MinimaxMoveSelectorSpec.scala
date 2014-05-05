package uk.co.mattthomson.coursera.ggp.gresley.moveselector.multiple

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorSpec
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.simple.LegalMoveSelector

class MinimaxMoveSelectorSpec extends MoveSelectorSpec {
  "The minimax move selector" should "play Tic Tac Toe 3" in {
    val finalState = playGame("tictactoe3", List(Props[MinimaxMoveSelector], Props[LegalMoveSelector]))

    finalState.value("white") should be (50)
  }

  it should "play Tic Tac Toe 5" in {
    val finalState = playGame("tictactoe5", List(Props[MinimaxMoveSelector], Props[LegalMoveSelector]))

    finalState.value("white") should be (100)
  }

  it should "play Tic Tac Toe 7" ignore {
    val finalState = playGame("tictactoe7", List(Props[MinimaxMoveSelector], Props[LegalMoveSelector]))

    finalState.value("white") should be (100)
  }
}
