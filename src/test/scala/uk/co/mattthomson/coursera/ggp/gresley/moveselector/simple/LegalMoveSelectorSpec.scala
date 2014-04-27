package uk.co.mattthomson.coursera.ggp.gresley.moveselector.simple

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorSpec

class LegalMoveSelectorSpec extends MoveSelectorSpec {
  "The legal move selector" should "play Hunter" in {
    playGame("hunter", List(Props[LegalMoveSelector]))
  }
}
