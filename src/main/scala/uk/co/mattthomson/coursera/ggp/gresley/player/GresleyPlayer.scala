package uk.co.mattthomson.coursera.ggp.gresley.player

import akka.actor.Actor
import uk.co.mattthomson.coursera.ggp.gresley.protocol.Info

class GresleyPlayer extends Actor {
  override def receive: Receive = {
    case Info => sender ! "ready"
  }
}
