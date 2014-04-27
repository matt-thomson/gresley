package uk.co.mattthomson.coursera.ggp.gresley.moveselector.simple

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorSpec

class RandomMoveSelectorSpec extends MoveSelectorSpec {
  "The random move selector" should "play Hunter" in {
    playGame("hunter", List(Props[RandomMoveSelector]))
  }

  it should "play Alquerque" ignore {
    playGame("alquerque", List(Props[RandomMoveSelector], Props[LegalMoveSelector]))
  }

  it should "play Chinese Checkers 4" ignore {
    playGame("chinesecheckers4", List(Props[RandomMoveSelector], Props[LegalMoveSelector], Props[LegalMoveSelector], Props[LegalMoveSelector]))
  }
}
