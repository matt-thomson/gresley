package uk.co.mattthomson.coursera.ggp.gresley

import org.scalatra.LifeCycle
import javax.servlet.ServletContext
import uk.co.mattthomson.coursera.ggp.gresley.servlet.GresleyServlet
import akka.actor.{Props, ActorSystem}
import uk.co.mattthomson.coursera.ggp.gresley.player.LegalPlayer

class GresleyBootstrap extends LifeCycle {
  private val system = ActorSystem()
  private val playerProps = Props[LegalPlayer]

  override def init(context: ServletContext) {
    context mount(new GresleyServlet(system, playerProps), "/")
  }
}
