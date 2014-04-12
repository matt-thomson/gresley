package uk.co.mattthomson.coursera.ggp.gresley.protocol

import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameDescription
import scala.concurrent.duration.FiniteDuration

trait GameProtocolMessage {
}

case object Info extends GameProtocolMessage

case class Start(id: String,
                 role: String,
                 description: GameDescription,
                 startClock: FiniteDuration,
                 playClock: FiniteDuration) extends GameProtocolMessage

case class Play(id: String, moves: Option[Seq[String]]) extends GameProtocolMessage

case class Stop(id: String, moves: Seq[String]) extends GameProtocolMessage

case class Abort(id: String) extends GameProtocolMessage

object GameProtocolMessage {
  def apply(message: String) = {
    val parser = new GameProtocolParser
    parser.parseAll(parser.message, message).get
  }
}
