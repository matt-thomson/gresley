package uk.co.mattthomson.coursera.ggp.gresley.moveselector.heuristic

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorSpec

class BoundedDepthSearchMoveSelectorSpec extends MoveSelectorSpec {
  "The bounded depth search move selector" should "play Hunter" in {
    val finalState = playGame("hunter", List(Props(new BoundedDepthSearchMoveSelector(3))))

    finalState.value("robot") should be (22)
  }
}
