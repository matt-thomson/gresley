package uk.co.mattthomson.coursera.ggp.gresley.gdl

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.io.Source

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
        TrueStateCondition(Relation("cell", List(VariableTerm("x"))))
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
        TrueStateCondition(Relation("cell", List(VariableTerm("x")))),
        TrueStateCondition(Relation("cell", List(VariableTerm("y")))),
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

  it should "move to the next state" in {
    val game = new GameDescription(List(
      Relation("succ", List("1", "2")),
      Conditional(Next(Relation("step", List(VariableTerm("y")))), List(
        TrueStateCondition(Relation("step", List(VariableTerm("x")))),
        FactCondition(Relation("succ", List(VariableTerm("x"), VariableTerm("y"))))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("step", List("1"))
    ))

    state.update(Seq()).trueFacts should be (Set(
      Relation("step", List("2"))
    ))
  }

  it should "move to the next state using false rules" in {
    val game = new GameDescription(List(
      Base(Relation("step", List("1"))),
      Base(Relation("step", List("2"))),
      Conditional(Next(Relation("step", List(VariableTerm("x")))), List(
        FalseStateCondition(Relation("step", List(VariableTerm("x"))))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("step", List("1"))
    ))

    state.update(Seq()).trueFacts should be (Set(
      Relation("step", List("2"))
    ))
  }

  it should "move to the next state based on moves" in {
    val game = new GameDescription(List(
      Role("black"),
      Conditional(Next(Relation("position", List(VariableTerm("x")))), List(
        ActionCondition(Role("black"), Action("move", List(VariableTerm("x"))))
      ))
    ))

    val state = new GameState(game, Set())
    val moves = List(Action("move", List("1")))

    state.update(moves).trueFacts should be (Set(
      Relation("position", List("1"))
    ))
  }

  it should "find the false facts" in {
    val game = new GameDescription(List(
      Base(Relation("cell", List("1"))),
      Base(Relation("cell", List("2")))
    ))

    val state = new GameState(game, Set(Relation("cell", List("1"))))

    state.falseFacts should be (Set(
      Relation("cell", List("2"))
    ))
  }

  it should "process distinct correctly" in {
    val game = GameDescription(Source.fromFile("src/test/resources/games/maze.kif").mkString)
    val state = new GameState(game, Set(
      Relation("cell", List("c")),
      Relation("gold", List("i"))
    ))

    val nextState = state.update(List(Action("drop", Nil)))

    nextState.trueFacts should be (Set(
      Relation("cell", List("c")),
      Relation("gold", List("c"))
    ))
  }
}
