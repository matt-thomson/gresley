package uk.co.mattthomson.coursera.ggp.gresley

import org.scalatra.servlet.ScalatraListener
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext
import org.eclipse.jetty.servlet.DefaultServlet

object GresleyApp extends App {
  val context = new WebAppContext()
  context setContextPath "/"
  context.setResourceBase("src/main/webapp")
  context.addEventListener(new ScalatraListener)
  context.addServlet(classOf[DefaultServlet], "/")
  context.setInitParameter(ScalatraListener.LifeCycleKey, classOf[GresleyBootstrap].getName)

  val port = if(System.getenv("PORT") != null) System.getenv("PORT").toInt else 8080

  val server = new Server(port)
  server.setHandler(context)

  server.start()
  server.join()
}
