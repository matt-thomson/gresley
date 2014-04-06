package uk.co.mattthomson.coursera.ggp.gresley

import org.scalatra.LifeCycle
import javax.servlet.ServletContext
import uk.co.mattthomson.coursera.ggp.gresley.servlet.HelloServlet

class GresleyBootstrap extends LifeCycle {
  override def init(context: ServletContext) {
    context mount(classOf[HelloServlet], "/")
  }
}
