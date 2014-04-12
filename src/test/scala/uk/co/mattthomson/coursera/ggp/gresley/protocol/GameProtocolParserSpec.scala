package uk.co.mattthomson.coursera.ggp.gresley.protocol

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Role, GameDescription}

class GameProtocolParserSpec extends FlatSpec with ShouldMatchers {
  private val parser = new GameProtocolParser

  "The parser" should "parse an info message" in {
    parse("(info)", Info)
  }

  it should "parse a start message" in {
    val game = new GameDescription(List(Role("white"), Role("black")))
    val start = Start("id", "white", game, 1.second, 2.seconds)

    parse("(start id white ((role white) (role black)) 1 2)", start)
  }

  it should "parse an initial play message" in {
    parse("(play id nil)", Play("id", None))
  }

  it should "parse a play message with moves" in {
    parse("(play id (left right))", Play("id", Some(List("left", "right"))))
  }

  it should "parse a real id" in {
    parse("(play kiosk.ticTacToe-1397318430823 NIL)", Play("kiosk.ticTacToe-1397318430823", None))
  }

  it should "parse a stop message" in {
    parse("(stop id (left right))", Stop("id", List("left", "right")))
  }

  it should "parse an abort message" in {
    parse("(abort id)", Abort("id"))
  }

  it should "be case-insensitive" in {
    parse("(INFO)", Info)
  }

  private def parse(input: String, expected: GameProtocolMessage) {
    val result = parser.parseAll(parser.message, input)
    if (result.successful) result.get should be(expected) else fail(result.toString)
  }
}
