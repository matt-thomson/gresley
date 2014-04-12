package uk.co.mattthomson.coursera.ggp.gresley.gdl

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Term._
import scala.io.Source

class GameDescriptionSpec extends FlatSpec with ShouldMatchers {
  "A game description" should "process roles correctly" in {
    val description = GameDescription(List(
      Role("black"),
      Role("white")
    ))

    description.roles should be (List("black", "white"))
  }

  it should "process inputs correctly" in {
    val description = GameDescription(List(
      Input(Role("black"), Action("up")),
      Input(Role("white"), Action("left")),
      Input(Role("black"), Action("down"))
    ))

    description.inputs("black") should be (Set("up", "down"))
  }

  it should "process the initial state correctly" in {
    val description = GameDescription(List(
      Init(Relation("color", List("black"))),
      Init(Relation("color", List("white")))
    ))

    val initialState = description.initialState
    initialState.facts should be (Set(
      Relation("color", List("black")),
      Relation("color", List("white"))
    ))
  }

  it should "process relations correctly" in {
    val facts = List(
      Relation("row", List("1")),
      Relation("row", List("2")),
      Relation("col", List("3")),
      Relation("col", List("4"))
    )
    val description = GameDescription(facts)

    description.constantFacts should be (facts.toSet)
  }

  it should "process conditional relations correctly" in {
    val description = GameDescription(List(
      Relation("row", List("1")),
      Relation("row", List("2")),
      Conditional(Relation("cell", List("1", VariableTerm("x"))), List(
        Relation("row", List(VariableTerm("x")))
      ))
    ))

    description.constantFacts should be (Set(
      Relation("row", List("1")),
      Relation("row", List("2")),
      Relation("cell", List("1", "1")),
      Relation("cell", List("1", "2"))
    ))
  }

  it should "include roles in conditional relations" in {
    val description = GameDescription(List(
      Role("black"),
      Relation("move", List("black", "1")),
      Conditional(Relation("row", List(VariableTerm("y"))), List(
        Role(VariableTerm("x")),
        Relation("move", List(VariableTerm("x"), VariableTerm("y")))
      ))
    ))

    description.constantFacts should be (Set(
      Role("black"),
      Relation("move", List("black", "1")),
      Relation("row", List("1"))
    ))
  }

  it should "propagate conditionals fully" in {
    val description = GameDescription(List(
      Relation("next", List("1", "2")),
      Relation("next", List("2", "3")),
      Relation("number", List("1")),
      Conditional(Relation("number", List(VariableTerm("y"))), List(
        Relation("number", List(VariableTerm("x"))),
        Relation("next", List(VariableTerm("x"), VariableTerm("y")))
      ))
    ))

    description.constantFacts should be (Set(
      Relation("next", List("1", "2")),
      Relation("next", List("2", "3")),
      Relation("number", List("1")),
      Relation("number", List("2")),
      Relation("number", List("3"))
    ))
  }

  "A real game" should "have the correct roles" in {
    val game = Source.fromFile("src/test/resources/games/maze.kif").mkString
    val description = GameDescription(game)

    description.roles should be (List("robot"))
  }

  it should "have the correct relations" in {
    val game = Source.fromFile("src/test/resources/games/maze.kif").mkString
    val description = GameDescription(game)

    description.constantFacts.contains(Relation("succ", List("1", "2"))) should be (true)
    description.constantFacts.contains(Relation("adjacent", List("a", "b"))) should be (true)
  }

  it should "have the correct base relations" in {
    val game = Source.fromFile("src/test/resources/games/maze.kif").mkString
    val description = GameDescription(game)

    description.constantFacts.contains(Base(Relation("cell", List("a")))) should be (true)
    description.constantFacts.contains(Base(Relation("step", List("1")))) should be (true)
    description.constantFacts.contains(Base(Relation("step", List("10")))) should be (true)
  }

  it should "have the correct inputs" in {
    val game = Source.fromFile("src/test/resources/games/maze.kif").mkString
    val description = GameDescription(game)

    description.inputs("robot") should be (Set("drop", "grab", "move"))
  }

  it should "have the correct initial state" in {
    val game = Source.fromFile("src/test/resources/games/maze.kif").mkString
    val description = GameDescription(game)

    val initialState = description.initialState
    initialState.facts should be (Set(
      Relation("cell", List("a")),
      Relation("gold", List("c")),
      Relation("step", List("1"))
    ))
  }
}
