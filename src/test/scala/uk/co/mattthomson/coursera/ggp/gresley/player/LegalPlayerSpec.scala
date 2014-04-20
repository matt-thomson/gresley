package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.Props

class LegalPlayerSpec extends PlayerSpec {
  "The legal player" should "play Hunter" in {
    val player = system.actorOf(Props[LegalPlayer])
    playGame("hunter", List(player))
  }
}
