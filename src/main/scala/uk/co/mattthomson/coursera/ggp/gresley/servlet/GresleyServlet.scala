package uk.co.mattthomson.coursera.ggp.gresley.servlet

import org.scalatra._
import org.slf4j.LoggerFactory
import _root_.akka.actor.{Props, ActorSystem}
import _root_.akka.pattern.ask
import _root_.akka.util.Timeout
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.player.{Player, GameManager}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameProtocolMessage
import javax.servlet.http.HttpServletRequest
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.MoveSelectorPropsFactory

class GresleyServlet(system: ActorSystem, moveSelectorPropsFactory: MoveSelectorPropsFactory) extends ScalatraServlet with FutureSupport with CorsSupport {

  private val logger = LoggerFactory.getLogger(getClass)
  private val manager = system.actorOf(Props(new GameManager(Player(_), moveSelectorPropsFactory)))

  implicit val defaultTimeout = Timeout(3.minutes)

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

  override def multiParams(implicit request: HttpServletRequest) = collection.immutable.SeqMap[String, Seq[String]]()
}
