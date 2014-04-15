package uk.co.mattthomson.coursera.ggp.gresley

import org.scalatra.LifeCycle
import javax.servlet.ServletContext
import uk.co.mattthomson.coursera.ggp.gresley.servlet.GresleyServlet
import akka.actor.{Props, ActorSystem}
import uk.co.mattthomson.coursera.ggp.gresley.player.RandomPlayer

class GresleyBootstrap extends LifeCycle {
  private val system = ActorSystem()
  private val playerProps = Props[RandomPlayer]

  override def init(context: ServletContext) {
    context mount(new GresleyServlet(system, playerProps), "/")
  }
}
