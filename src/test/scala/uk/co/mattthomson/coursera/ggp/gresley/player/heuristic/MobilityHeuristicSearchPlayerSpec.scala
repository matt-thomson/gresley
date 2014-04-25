package uk.co.mattthomson.coursera.ggp.gresley.player.heuristic

import uk.co.mattthomson.coursera.ggp.gresley.player.{LegalPlayer, PlayerSpec}
import akka.actor.Props

class MobilityHeuristicSearchPlayerSpec extends PlayerSpec {
   "The bounded depth search player" should "play Hunter" in {
     val player = system.actorOf(Props(new MobilityHeuristicSearchPlayer(3)))

     val finalState = playGame("hunter", List(player))

     finalState.value("robot") should be (22)
   }

   it should "play Alquerque" ignore {
     val player = system.actorOf(Props(new MobilityHeuristicSearchPlayer(2)))
     val opponent = system.actorOf(Props[LegalPlayer])

     val finalState = playGame("alquerque", List(player, opponent))

     finalState.value("red") should be (100)
   }
 }
