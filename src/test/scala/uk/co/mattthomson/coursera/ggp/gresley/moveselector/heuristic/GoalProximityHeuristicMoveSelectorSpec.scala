package uk.co.mattthomson.coursera.ggp.gresley.moveselector.heuristic

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorSpec
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.simple.LegalMoveSelector

class GoalProximityHeuristicMoveSelectorSpec extends MoveSelectorSpec {
  "The goal proximity heuristic search move selector" should "play Hunter" in {
    val finalState = playGame("hunter", List(Props(new GoalProximityHeuristicSearchMoveSelector(3))))

    finalState.value("robot") should be (75)
  }

  it should "play Alquerque" ignore {
    val finalState = playGame("alquerque", List(Props(new GoalProximityHeuristicSearchMoveSelector(2)), Props[LegalMoveSelector]))

    finalState.value("red") should be (100)
  }
}
