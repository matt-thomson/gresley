package uk.co.mattthomson.coursera.ggp.gresley.protocol

import org.scalatest.flatspec._
import org.scalatest.matchers._
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.gdl._
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Role
import scala.Some

class GameProtocolParserSpec extends AnyFlatSpec with should.Matchers {
  private val parser = new GdlParser

  "The parser" should "parse an info message" in {
    parse("(info)", Info)
  }

  it should "parse a start message" in {
    val game = new GameDescription(List(Role("white"), Base(Relation("cell", List("a")))))
    val start = Start("id", "white", game, 1.second, 2.seconds)

    parse("(start id white ((role white) (base (cell a))) 1 2)", start)
  }

  it should "parse an initial play message" in {
    parse("(play id nil)", Play("id", None))
  }

  it should "parse a play message with moves" in {
    parse("(play id (left right))", Play("id", Some(List(Action("left", Nil), Action("right", Nil)))))
  }

  it should "parse a play message with complex moves" in {
    parse("(play id ((left 1) (right 2)))", Play("id", Some(List(Action("left", List("1")), Action("right", List("2"))))))
  }

  it should "parse a real id" in {
    parse("(play kiosk.ticTacToe-1397318430823 NIL)", Play("kiosk.ticTacToe-1397318430823", None))
  }

  it should "parse a stop message" in {
    parse("(stop id (left right))", Stop("id", List(Action("left", Nil), Action("right", Nil))))
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
