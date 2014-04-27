package uk.co.mattthomson.coursera.ggp.gresley.moveselector.single

import akka.actor.Props
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorSpec

class SequentialPlanningMoveSelectorSpec extends MoveSelectorSpec {
   "The sequential planning move selector" should "play 3-Puzzle" in {
     val finalState = playGame("3puzzle", List(Props[SequentialPlanningMoveSelector]))

     finalState.value("robot") should be (100)
   }

   it should "play Buttons and Lights" in {
     val finalState = playGame("buttonslights", List(Props[SequentialPlanningMoveSelector]))

     finalState.value("robot") should be (100)
   }
 }
