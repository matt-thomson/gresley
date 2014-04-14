package uk.co.mattthomson.coursera.ggp.gresley.servlet

import org.scalatra._
import org.slf4j.LoggerFactory
import _root_.akka.actor.{Props, ActorSystem}
import _root_.akka.pattern.ask
import _root_.akka.util.Timeout
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameProtocolMessage

class GresleyServlet(system: ActorSystem, playerProps: Props) extends ScalatraServlet with FutureSupport with CorsSupport {
  private val logger = LoggerFactory.getLogger(getClass)
  private val manager = system.actorOf(Props(new GameManager(playerProps)))

  implicit val defaultTimeout = Timeout(1.minute)

  override protected implicit def executor = system.dispatcher


  options("/*"){
    response.setHeader("Access-Control-Allow-Headers", request.getHeader("Access-Control-Request-Headers"));
  }

  get("/") {
    "Hello, my name is Gresley."
  }

  post("/") {
    val body = request.contentType match {
      case Some("application/x-www-form-urlencoded") => request.getParameterNames.nextElement()
      case _ => request.body
    }

    logger.info(s"Received message: $body")

    if (body.isEmpty) NoContent()
    else {
      val message = GameProtocolMessage(body)

      new AsyncResult {
        val is = manager ? message
      }
    }
  }
}
