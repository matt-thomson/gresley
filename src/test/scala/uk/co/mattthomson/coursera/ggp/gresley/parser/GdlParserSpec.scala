package uk.co.mattthomson.coursera.ggp.gresley.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser._
import uk.co.mattthomson.coursera.ggp.gresley.parser.Term._
import scala.io.Source

class GdlParserSpec extends FlatSpec with ShouldMatchers {
  val gdlParser = new GdlParser

  "The parser" should "parse a role" in {
    parse("(role robot)", Role("robot"))
  }

  it should "parse an input" in {
    parse("(input robot move)", Input(Role("robot"), Action("move")))
  }

  it should "parse a base rule" in {
    parse("(base (cell a))", Base(Proposition("cell", List("a"))))
  }

  it should "parse a conditional" in {
    parse("(<= (step ?x) (succ 1 ?x))", Conditional(Relation("step", List(VariableTerm("x"))), List(Relation("succ", List("1", VariableTerm("x"))))))
  }

  it should "ignore a comment" in {
    parse("; this is a comment")
  }

  it should "parse a game" in {
    val game: String = Source.fromFile("src/test/resources/games/maze.kif").mkString
    parse(game,
      Role("robot"),

      Base(Proposition("cell", List("a"))),
      Base(Proposition("cell", List("b"))),
      Base(Proposition("cell", List("c"))),
      Base(Proposition("cell", List("d"))),
      Base(Proposition("gold", List("a"))),
      Base(Proposition("gold", List("b"))),
      Base(Proposition("gold", List("c"))),
      Base(Proposition("gold", List("d"))),
      Base(Proposition("gold", List("i"))),
      Base(Proposition("step", List("1"))),

      Input(Role("robot"), Action("move")),
      Input(Role("robot"), Action("grab")),
      Input(Role("robot"), Action("drop")),

      Relation("adjacent", List("a", "b")),
      Relation("adjacent", List("b", "c")),
      Relation("adjacent", List("c", "d")),
      Relation("adjacent", List("d", "a")),

      Relation("succ", List("1", "2")),
      Relation("succ", List("2", "3")),
      Relation("succ", List("3", "4")),
      Relation("succ", List("4", "5")),
      Relation("succ", List("5", "6")),
      Relation("succ", List("6", "7")),
      Relation("succ", List("7", "8")),
      Relation("succ", List("8", "9")),
      Relation("succ", List("9", "10"))
    )
  }

  private def parse(input: String, expected: Any*) {
    val result = gdlParser.parseAll(gdlParser.game, input)
    if (result.successful) result.get should be(expected) else fail(result.toString)
  }
}
