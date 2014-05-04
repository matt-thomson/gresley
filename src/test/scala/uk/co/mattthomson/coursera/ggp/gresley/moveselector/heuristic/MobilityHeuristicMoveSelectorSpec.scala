package uk.co.mattthomson.coursera.ggp.gresley.moveselector.heuristic

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorSpec
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.simple.LegalMoveSelector

class MobilityHeuristicMoveSelectorSpec extends MoveSelectorSpec {
   "The mobility heuristic search move selector" should "play Hunter" in {
     val finalState = playGame("hunter", List(Props(new MobilityHeuristicSearchMoveSelector(3))))

     finalState.value("robot") should be (22)
   }

   it should "play Alquerque" ignore {
     val finalState = playGame("alquerque", List(Props(new MobilityHeuristicSearchMoveSelector(2)), Props[LegalMoveSelector]))

     finalState.value("red") should be (100)
   }
 }
