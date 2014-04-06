package uk.co.mattthomson.coursera.ggp.gresley.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser._
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
    parse("(base (cell a))", Base(Proposition("cell", List(LiteralTerm("a")))))
  }

  it should "parse a conditional base rule" in {
    parse("(<= (base (step ?x)) (succ ?y ?x))", Conditional(Base(Proposition("step", List(VariableTerm("x")))), List(Relation("succ", List(VariableTerm("y"), VariableTerm("x"))))))
  }

  it should "ignore a comment" in {
    parse("; this is a comment")
  }

  it should "parse a game" in {
    val game: String = Source.fromFile("src/test/resources/games/maze.kif").mkString
    parse(game,
      Role("robot"),

      Base(Proposition("cell", List(LiteralTerm("a")))),
      Base(Proposition("cell", List(LiteralTerm("b")))),
      Base(Proposition("cell", List(LiteralTerm("c")))),
      Base(Proposition("cell", List(LiteralTerm("d")))),
      Base(Proposition("gold", List(LiteralTerm("a")))),
      Base(Proposition("gold", List(LiteralTerm("b")))),
      Base(Proposition("gold", List(LiteralTerm("c")))),
      Base(Proposition("gold", List(LiteralTerm("d")))),
      Base(Proposition("gold", List(LiteralTerm("i")))),
      Base(Proposition("step", List(LiteralTerm("1")))),
      Conditional(Base(Proposition("step", List(VariableTerm("x")))), List(Relation("succ", List(VariableTerm("y"), VariableTerm("x"))))),

      Input(Role("robot"), Action("move")),
      Input(Role("robot"), Action("grab")),
      Input(Role("robot"), Action("drop")),

      Relation("adjacent", List(LiteralTerm("a"), LiteralTerm("b"))),
      Relation("adjacent", List(LiteralTerm("b"), LiteralTerm("c"))),
      Relation("adjacent", List(LiteralTerm("c"), LiteralTerm("d"))),
      Relation("adjacent", List(LiteralTerm("d"), LiteralTerm("a"))),

      Relation("succ", List(LiteralTerm("1"), LiteralTerm("2"))),
      Relation("succ", List(LiteralTerm("2"), LiteralTerm("3"))),
      Relation("succ", List(LiteralTerm("3"), LiteralTerm("4"))),
      Relation("succ", List(LiteralTerm("4"), LiteralTerm("5"))),
      Relation("succ", List(LiteralTerm("5"), LiteralTerm("6"))),
      Relation("succ", List(LiteralTerm("6"), LiteralTerm("7"))),
      Relation("succ", List(LiteralTerm("7"), LiteralTerm("8"))),
      Relation("succ", List(LiteralTerm("8"), LiteralTerm("9"))),
      Relation("succ", List(LiteralTerm("9"), LiteralTerm("10")))
    )
  }

  private def parse(input: String, expected: Any*) {
    val result = gdlParser.parseAll(gdlParser.game, input)
    if (result.successful) result.get should be(expected) else fail(result.toString)
  }
}
