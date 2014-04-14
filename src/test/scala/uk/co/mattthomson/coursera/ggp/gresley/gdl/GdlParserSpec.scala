package uk.co.mattthomson.coursera.ggp.gresley.gdl

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Term._
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
    parse("(base (cell a))", Base(Relation("cell", List("a"))))
  }

  it should "parse an initial state" in {
    parse("(init (cell a))", Init(Relation("cell", List("a"))))
  }

  it should "parse a conditional" in {
    parse("(<= (step ?x) (succ 1 ?x))", Conditional(
      Relation("step", List(VariableTerm("x"))),
      List(FactCondition(Relation("succ", List("1", VariableTerm("x")))))
    ))
  }

  it should "ignore a comment" in {
    parse("; this is a comment")
  }

  it should "parse a game" in {
    val game: String = Source.fromFile("src/test/resources/games/maze.kif").mkString
    parse(game,
      Role("robot"),

      Base(Relation("cell", List("a"))),
      Base(Relation("cell", List("b"))),
      Base(Relation("cell", List("c"))),
      Base(Relation("cell", List("d"))),
      Base(Relation("gold", List("a"))),
      Base(Relation("gold", List("b"))),
      Base(Relation("gold", List("c"))),
      Base(Relation("gold", List("d"))),
      Base(Relation("gold", List("i"))),
      Base(Relation("step", List("1"))),
      Conditional(Base(Relation("step", List(VariableTerm("x")))), List(FactCondition(Relation("succ", List(VariableTerm("y"), VariableTerm("x")))))),

      Input(Role("robot"), Action("move")),
      Input(Role("robot"), Action("grab")),
      Input(Role("robot"), Action("drop")),

      Init(Relation("cell", List("a"))),
      Init(Relation("gold", List("c"))),
      Init(Relation("step", List("1"))),

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
      Relation("succ", List("9", "10")),

      Conditional(Legal(Role("robot"), Action("move")), List(FactCondition(Relation("succ", List("1", "2"))))),
      Conditional(Legal(Role("robot"), Action("grab")), List(
        StateCondition(Relation("cell", List(VariableTerm("x")))),
        StateCondition(Relation("gold", List(VariableTerm("x"))))
      )),
      Conditional(Legal(Role("robot"), Action("drop")), List(
        StateCondition(Relation("gold", List("i")))
      ))
    )
  }

  private def parse(input: String, expected: Any*) {
    val result = gdlParser.parseAll(gdlParser.game, input)
    if (result.successful) result.get should be(expected) else fail(result.toString)
  }
}
