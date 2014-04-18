package uk.co.mattthomson.coursera.ggp.gresley

import akka.actor.ActorSystem
import org.scalatra.LifeCycle
import javax.servlet.ServletContext
import uk.co.mattthomson.coursera.ggp.gresley.servlet.GresleyServlet
import uk.co.mattthomson.coursera.ggp.gresley.player.PlayerPropsFactoryImpl

class GresleyBootstrap extends LifeCycle {
  private val system = ActorSystem()
  private val playerPropsFactory = new PlayerPropsFactoryImpl

  override def init(context: ServletContext) {
    context mount(new GresleyServlet(system, playerPropsFactory), "/")
  }
}
