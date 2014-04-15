package uk.co.mattthomson.coursera.ggp.gresley.gdl

import scala.concurrent.duration.FiniteDuration

trait GameProtocolMessage {
}

case object Info extends GameProtocolMessage

case class Start(id: String,
                 role: String,
                 description: GameDescription,
                 startClock: FiniteDuration,
                 playClock: FiniteDuration) extends GameProtocolMessage

case class Play(id: String, moves: Option[Seq[Action]]) extends GameProtocolMessage

case class Stop(id: String, moves: Seq[Action]) extends GameProtocolMessage

case class Abort(id: String) extends GameProtocolMessage

object GameProtocolMessage {
  def apply(message: String) = {
    val parser = new GdlParser
    val parsed = parser.parseAll(parser.message, message)

    if (parsed.successful) parsed.get else throw new IllegalArgumentException(parsed.toString)
  }
}
