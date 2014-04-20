package uk.co.mattthomson.coursera.ggp.gresley.player.single

import uk.co.mattthomson.coursera.ggp.gresley.player.PlayerSpec
import akka.actor.Props

class CompulsiveDeliberationPlayerSpec extends PlayerSpec {
  "The compulsive deliberation player" should "play 3-Puzzle" in {
    val player = system.actorOf(Props[CompulsiveDeliberationPlayer])
    val finalState = playGame("3puzzle", List(player))

    finalState.value("robot") should be (100)
  }

  it should "play Buttons and Lights" in {
    val player = system.actorOf(Props[CompulsiveDeliberationPlayer])
    val finalState = playGame("buttonslights", List(player))

    finalState.value("robot") should be (100)
  }
}
