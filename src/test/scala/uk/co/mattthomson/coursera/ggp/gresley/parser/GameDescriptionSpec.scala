package uk.co.mattthomson.coursera.ggp.gresley.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser.Conditional
import uk.co.mattthomson.coursera.ggp.gresley.parser.Term._

class GameDescriptionSpec extends FlatSpec with ShouldMatchers {
  "A game description" should "process roles correctly" in {
    val description = GameDescription(List(
      Role("black"),
      Role("white")
    ))

    description.roles should be (List("black", "white"))
  }

  it should "process relations correctly" in {
    val facts = List(
      Relation("row", List("1")),
      Relation("row", List("2")),
      Relation("col", List("3")),
      Relation("col", List("4"))
    )
    val description = GameDescription(facts)

    description.facts should be (facts.toSet)
  }

  it should "process conditional relations correctly" in {
    val description = GameDescription(List(
      Relation("row", List("1")),
      Relation("row", List("2")),
      Conditional(Relation("cell", List("1", VariableTerm("x"))), List(
        Relation("row", List(VariableTerm("x")))
      ))
    ))

    description.facts should be (Set(
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

    description.facts should be (Set(
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

    description.facts should be (Set(
      Relation("next", List("1", "2")),
      Relation("next", List("2", "3")),
      Relation("number", List("1")),
      Relation("number", List("2")),
      Relation("number", List("3"))
    ))
  }
}
