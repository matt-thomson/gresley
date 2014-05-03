package uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorSpec
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.simple.LegalMoveSelector

class MonteCarloSearchMoveSelectorSpec extends MoveSelectorSpec {
  "The Monte Carlo search move selector" should "play Hunter" ignore {
    val finalState = playGame("hunter", List(Props[MonteCarloSearchMoveSelector]))

    finalState.value("robot") should be (87)
  }

  it should "play Alquerque" ignore {
    val finalState = playGame("alquerque", List(Props[MonteCarloSearchMoveSelector], Props[LegalMoveSelector]))

    finalState.value("red") should be (100)
  }
}

