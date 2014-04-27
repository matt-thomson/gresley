package uk.co.mattthomson.coursera.ggp.gresley

import akka.actor.ActorSystem
import org.scalatra.LifeCycle
import javax.servlet.ServletContext
import uk.co.mattthomson.coursera.ggp.gresley.servlet.GresleyServlet
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorPropsFactoryImpl

class GresleyBootstrap extends LifeCycle {
  private val system = ActorSystem()
  private val moveSelectorPropsFactory = new MoveSelectorPropsFactoryImpl

  override def init(context: ServletContext) {
    context mount(new GresleyServlet(system, moveSelectorPropsFactory), "/")
  }
}
