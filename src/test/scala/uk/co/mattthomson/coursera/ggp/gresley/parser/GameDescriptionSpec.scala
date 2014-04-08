package uk.co.mattthomson.coursera.ggp.gresley.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser.{LiteralTerm, Relation, Role}

class GameDescriptionSpec extends FlatSpec with ShouldMatchers {
  "A game description" should "process roles correctly" in {
    val description = GameDescription(List(
      Role("black"),
      Role("white")
    ))

    description.roles should be (List("black", "white"))
  }

  it should "process relations correctly" in {
    val description = GameDescription(List(
      Relation("row", List(LiteralTerm("1"))),
      Relation("row", List(LiteralTerm("2"))),
      Relation("col", List(LiteralTerm("3"))),
      Relation("col", List(LiteralTerm("4")))
    ))

    description.relations("row") should be (List(List("1"), List("2")))
    description.relations("col") should be (List(List("3"), List("4")))
  }
}
