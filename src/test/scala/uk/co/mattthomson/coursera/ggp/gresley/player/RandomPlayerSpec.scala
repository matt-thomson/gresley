package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.Props

class RandomPlayerSpec extends PlayerSpec {
  "The random player" should "play Hunter" in {
    val player = system.actorOf(Props[RandomPlayer])
    playGame("hunter", List(player))
  }

  it should "play Alquerque" ignore {
    val player = system.actorOf(Props[RandomPlayer])
    val opponent = system.actorOf(Props[LegalPlayer])
    playGame("alquerque", List(player, opponent))
  }

  it should "play Chinese Checkers 4" ignore {
    val player = system.actorOf(Props[RandomPlayer])
    val opponent1 = system.actorOf(Props[LegalPlayer])
    val opponent2 = system.actorOf(Props[LegalPlayer])
    val opponent3 = system.actorOf(Props[LegalPlayer])

    playGame("chinesecheckers4", List(player, opponent1, opponent2, opponent3))
  }
}
