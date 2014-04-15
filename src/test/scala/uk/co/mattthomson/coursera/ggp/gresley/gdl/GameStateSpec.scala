package uk.co.mattthomson.coursera.ggp.gresley.gdl

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class GameStateSpec extends FlatSpec with ShouldMatchers {
  "A game state" should "find the legal moves" in {
    val game = new GameDescription(List(
      Relation("cell", List("1")),
      Relation("cell", List("2")),
      Conditional(Legal(Role("black"), Action("move", List(VariableTerm("x")))), List(
        FactCondition(Relation("cell", List(VariableTerm("x"))))
      ))
    ))

    val state = new GameState(game, Set())

    state.legalActions("black") should be (Set(
      Action("move", List("1")),
      Action("move", List("2"))
    ))
  }

  it should "find the legal moves based on the state" in {
    val game = new GameDescription(List(
      Conditional(Legal(Role("black"), Action("move", List(VariableTerm("x")))), List(
        StateCondition(Relation("cell", List(VariableTerm("x"))))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("cell", List("1")),
      Relation("cell", List("2"))
    ))

    state.legalActions("black") should be (Set(
      Action("move", List("1")),
      Action("move", List("2"))
    ))
  }

  it should "apply distinct moves" in {
    val game = new GameDescription(List(
      Conditional(Legal(Role("black"), Action("move", List(VariableTerm("x"), VariableTerm("y")))), List(
        StateCondition(Relation("cell", List(VariableTerm("x")))),
        StateCondition(Relation("cell", List(VariableTerm("y")))),
        DistinctCondition(List(VariableTerm("x"), VariableTerm("y")))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("cell", List("1")),
      Relation("cell", List("2"))
    ))

    state.legalActions("black") should be (Set(
      Action("move", List("1", "2")),
      Action("move", List("2", "1"))
    ))
  }
}
