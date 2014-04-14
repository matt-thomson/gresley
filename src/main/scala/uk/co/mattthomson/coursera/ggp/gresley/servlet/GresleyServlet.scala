package uk.co.mattthomson.coursera.ggp.gresley.servlet

import org.scalatra._
import org.slf4j.LoggerFactory
import _root_.akka.actor.{Props, ActorSystem}
import _root_.akka.pattern.ask
import _root_.akka.util.Timeout
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameProtocolMessage
import javax.servlet.http.HttpServletRequest
import org.scalatra.util.MultiMap

class GresleyServlet(system: ActorSystem, playerProps: Props) extends ScalatraServlet with FutureSupport with CorsSupport {
  private val logger = LoggerFactory.getLogger(getClass)
  private val manager = system.actorOf(Props(new GameManager(playerProps)))

  implicit val defaultTimeout = Timeout(1.minute)

  override protected implicit def executor = system.dispatcher

  options("/*"){
    response.setHeader("Access-Control-Allow-Headers", request.getHeader("Access-Control-Request-Headers"))
  }

  get("/") {
    "Hello, my name is Gresley."
  }

  post("/") {
    logger.info(s"Received message: ${request.body}")

    if (request.body.isEmpty) NoContent()
    else {
      val message = GameProtocolMessage(request.body)

      new AsyncResult {
        val is = manager ? message
      }
    }
  }

  override def multiParams(implicit request: HttpServletRequest) = new MultiMap
}
