package uk.co.mattthomson.coursera.ggp.gresley.servlet

import org.scalatra.{AsyncResult, FutureSupport, ScalatraServlet}
import org.slf4j.LoggerFactory
import akka.actor.{Props, ActorSystem}
import akka.pattern.ask
import uk.co.mattthomson.coursera.ggp.gresley.protocol.GameProtocolMessage
import akka.util.Timeout
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager

class GresleyServlet(system: ActorSystem, playerProps: Props) extends ScalatraServlet with FutureSupport {
  private val logger = LoggerFactory.getLogger(getClass)
  private val manager = system.actorOf(Props(new GameManager(playerProps)))

  implicit val defaultTimeout = Timeout(1.minute)

  override protected implicit def executor = system.dispatcher

  get("/") {
    "Hello, my name is Gresley."
  }

  post("/") {
    logger.info(s"Received message: ${request.body}")
    val message = GameProtocolMessage(request.body)

    new AsyncResult { val is = manager ? message }
  }
}
