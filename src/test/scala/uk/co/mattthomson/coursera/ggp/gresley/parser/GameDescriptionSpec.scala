package uk.co.mattthomson.coursera.ggp.gresley.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser._
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser.Conditional
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser.LiteralTerm
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser.Role
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser.Relation

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

    description.relations("row") should be (List(RelationArgs("1"), RelationArgs("2")))
    description.relations("col") should be (List(RelationArgs("3"), RelationArgs("4")))
  }

  it should "process conditional relations correctly" ignore {
    val description = GameDescription(List(
      Relation("row", List(LiteralTerm("1"))),
      Relation("row", List(LiteralTerm("2"))),
      Conditional(Relation("cell", List(LiteralTerm("1"), VariableTerm("x"))), List(
        Relation("row", List(VariableTerm("x")))
      ))
    ))

    description.relations("cell") should be (List(RelationArgs("1"), RelationArgs("1"), (RelationArgs("1"), RelationArgs("2"))))
  }
}
