package uk.co.mattthomson.coursera.ggp.gresley.player.heuristic

import uk.co.mattthomson.coursera.ggp.gresley.player.{PlayerSpec, LegalPlayer}
import akka.actor.Props

class BoundedDepthSearchPlayerSpec extends PlayerSpec {
  "The bounded depth search player" should "play Hunter" in {
    val player = system.actorOf(Props(new BoundedDepthSearchPlayer(3)))

    val finalState = playGame("hunter", List(player))

    finalState.value("robot") should be (75)
  }

  it should "play Alquerque" ignore {
    val player = system.actorOf(Props(new BoundedDepthSearchPlayer(2)))
    val opponent = system.actorOf(Props[LegalPlayer])

    val finalState = playGame("alquerque", List(player, opponent))

    finalState.value("red") should be (100)
  }
}
