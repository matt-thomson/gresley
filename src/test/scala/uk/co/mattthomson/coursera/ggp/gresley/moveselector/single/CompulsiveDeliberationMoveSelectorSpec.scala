package uk.co.mattthomson.coursera.ggp.gresley.moveselector.single

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorSpec

class CompulsiveDeliberationMoveSelectorSpec extends MoveSelectorSpec {
  "The compulsive deliberation move selector" should "play 3-Puzzle" in {
    val finalState = playGame("3puzzle", List(Props[CompulsiveDeliberationMoveSelector]))

    finalState.value("robot") should be (100)
  }

  it should "play Buttons and Lights" in {
    val finalState = playGame("buttonslights", List(Props[CompulsiveDeliberationMoveSelector]))

    finalState.value("robot") should be (100)
  }
}
