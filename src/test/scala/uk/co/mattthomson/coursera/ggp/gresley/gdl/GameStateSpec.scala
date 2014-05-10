package uk.co.mattthomson.coursera.ggp.gresley.gdl

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import scala.io.Source

class GameStateSpec extends FlatSpec with ShouldMatchers {
  "A game state" should "find the legal moves" in {
    val game = new GameDescription(List(
      Relation("cell", List("1")),
      Relation("cell", List("2")),
      Input(Role("black"), Action("move", List("1"))),
      Input(Role("black"), Action("move", List("2"))),
      Input(Role("black"), Action("move", List("3"))),
      Conditional(Legal(Role("black"), Action("move", List(VariableTerm("x")))), List(
        FactCondition(Relation("cell", List(VariableTerm("x"))))
      ))
    ))

    val state = new GameState(game, Set())

    state.legalActions("black") should be (List(
      Action("move", List("2")),
      Action("move", List("1"))
    ))
  }

  it should "find the legal moves based on the state" in {
    val game = new GameDescription(List(
      Input(Role("black"), Action("move", List("1"))),
      Input(Role("black"), Action("move", List("2"))),
      Input(Role("black"), Action("move", List("3"))),
      Conditional(Legal(Role("black"), Action("move", List(VariableTerm("x")))), List(
        StateCondition(Relation("cell", List(VariableTerm("x"))))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("cell", List("1")),
      Relation("cell", List("2"))
    ))

    state.legalActions("black") should be (List(
      Action("move", List("1")),
      Action("move", List("2"))
    ))
  }

  it should "apply distinct moves" in {
    val game = new GameDescription(List(
      Input(Role("black"), Action("move", List("1", "2"))),
      Input(Role("black"), Action("move", List("2", "1"))),
      Input(Role("black"), Action("move", List("1", "1"))),
      Input(Role("black"), Action("move", List("2", "2"))),
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

    state.legalActions("black") should be (List(
      Action("move", List("1", "2")),
      Action("move", List("2", "1"))
    ))
  }

  it should "move to the next state" in {
    val game = new GameDescription(List(
      Relation("succ", List("1", "2")),
      Base(Relation("step", List("1"))),
      Base(Relation("step", List("2"))),
      Base(Relation("step", List("3"))),
      Conditional(Next(Relation("step", List(VariableTerm("y")))), List(
        StateCondition(Relation("step", List(VariableTerm("x")))),
        FactCondition(Relation("succ", List(VariableTerm("x"), VariableTerm("y"))))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("step", List("1"))
    ))

    state.update(Map()).trueFacts should be (Set(
      Relation("step", List("2"))
    ))
  }

  it should "move to the next state using false fact rules" in {
    val game = new GameDescription(List(
      Relation("step", List("1")),
      Base(Relation("step", List("1"))),
      Base(Relation("step", List("2"))),
      Base(Relation("step", List("3"))),
      Conditional(Next(Relation("step", List("1"))), List(
        FalseCondition(FactCondition(Relation("step", List("1"))))
      )),
      Conditional(Next(Relation("step", List("2"))), List(
        FalseCondition(FactCondition(Relation("step", List("2"))))
      ))
    ))

    val state = new GameState(game, Set())

    state.update(Map()).trueFacts should be (Set(
      Relation("step", List("2"))
    ))
  }

  it should "move to the next state using false state rules" in {
    val game = new GameDescription(List(
      Base(Relation("step", List("1"))),
      Base(Relation("step", List("2"))),
      Conditional(Next(Relation("step", List(VariableTerm("x")))), List(
        FalseCondition(StateCondition(Relation("step", List(VariableTerm("x")))))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("step", List("1"))
    ))

    state.update(Map()).trueFacts should be (Set(
      Relation("step", List("2"))
    ))
  }

  it should "move to the next state based on moves" in {
    val game = new GameDescription(List(
      Role("black"),
      Base(Relation("position", List("1"))),
      Base(Relation("position", List("2"))),
      Conditional(Next(Relation("position", List(VariableTerm("x")))), List(
        ActionCondition(Role("black"), Action("move", List(VariableTerm("x"))))
      ))
    ))

    val state = new GameState(game, Set())
    val moves = Map("black" -> Action("move", List("1")))

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

    val nextState = state.update(Map("robot" -> Action("drop", Nil)))

    nextState.trueFacts should be (Set(
      Relation("cell", List("c")),
      Relation("gold", List("c"))
    ))
  }

  it should "mark a state as terminal correctly" in {
    val game = new GameDescription(List(
      Conditional(Terminal, List(
        StateCondition(Relation("cell", List("1")))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("cell", List("1"))
    ))

    state.isTerminal should be (true)
  }

  it should "mark a state as not terminal correctly" in {
    val game = new GameDescription(List(
      Conditional(Terminal, List(
        StateCondition(Relation("cell", List("1")))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("cell", List("2"))
    ))

    state.isTerminal should be (false)
  }

  it should "mark a state as terminal correctly after applying rules" in {
    val game = new GameDescription(List(
      Relation("succ", List("1", "2")),
      Conditional(Relation("step", List(VariableTerm("y"))), List(
        StateCondition(Relation("step", List(VariableTerm("x")))),
        FactCondition(Relation("succ", List(VariableTerm("x"), VariableTerm("y"))))
      )),
      Conditional(Terminal, List(
        FactCondition(Relation("step", List("2")))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("step", List("1"))
    ))

    state.isTerminal should be (true)
  }

  it should "calculate the value of a state" in {
    val game = new GameDescription(List(
      Conditional(Goal(Role("black"), "50"), List(
        StateCondition(Relation("cell", List("1")))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("cell", List("1"))
    ))

    state.value("black") should be (50)
  }

  it should "return 0 if the state value is not specified" in {
    val game = new GameDescription(List(
      Conditional(Goal(Role("black"), "50"), List(
        StateCondition(Relation("cell", List("1")))
      ))
    ))

    val state = new GameState(game, Set(
      Relation("cell", List("1"))
    ))

    state.value("white") should be (0)
  }

  it should "treat the closed state of Tic-Tac-Toe as terminal" in {
    val game = GameDescription(Source.fromFile("src/test/resources/games/tictactoe.kif").mkString)
    val state = new GameState(game, Set(
      Relation("cell", List("1", "1", "x")),
      Relation("cell", List("1", "2", "x")),
      Relation("cell", List("1", "3", "o")),
      Relation("cell", List("2", "1", "o")),
      Relation("cell", List("2", "2", "o")),
      Relation("cell", List("2", "3", "x")),
      Relation("cell", List("3", "1", "x")),
      Relation("cell", List("3", "2", "o")),
      Relation("cell", List("3", "3", "x"))
    ))

    state.isTerminal should be (true)
    state.value("oplayer") should be (50)
    state.value("xplayer") should be (50)
  }

  it should "treat a line state of Tic-Tac-Toe as terminal" in {
    val game = GameDescription(Source.fromFile("src/test/resources/games/tictactoe.kif").mkString)
    val state = new GameState(game, Set(
      Relation("cell", List("1", "1", "x")),
      Relation("cell", List("1", "2", "x")),
      Relation("cell", List("1", "3", "x")),
      Relation("cell", List("2", "1", "b")),
      Relation("cell", List("2", "2", "b")),
      Relation("cell", List("2", "3", "b")),
      Relation("cell", List("3", "1", "b")),
      Relation("cell", List("3", "2", "b")),
      Relation("cell", List("3", "3", "b"))
    ))

    state.isTerminal should be (true)
    state.value("oplayer") should be (0)
    state.value("xplayer") should be (100)
  }

  it should "score Tic-Tac-Toe correctly" in {
    val game = GameDescription(Source.fromFile("src/test/resources/games/tictactoe.kif").mkString)
    val state = new GameState(game, Set(
      Relation("cell", List("1", "1", "x")),
      Relation("cell", List("1", "2", "o")),
      Relation("cell", List("1", "3", "x")),
      Relation("cell", List("2", "1", "o")),
      Relation("cell", List("2", "2", "o")),
      Relation("cell", List("2", "3", "x")),
      Relation("cell", List("3", "1", "o")),
      Relation("cell", List("3", "2", "x")),
      Relation("cell", List("3", "3", "x"))
    ))

    state.isTerminal should be (true)
    state.value("oplayer") should be (0)
    state.value("xplayer") should be (100)
  }

  it should "move to the next state in Chinese Checkers correctly" in {
    val game = GameDescription(Source.fromFile("src/test/resources/games/chinesecheckers1.kif").mkString)
    game.initialState.trueFacts should be (Set(
      Relation("cell", List("a1", "red")),
      Relation("cell", List("b1", "red")),
      Relation("cell", List("b2", "red")),
      Relation("step", List("1"))
    ))

    val state = game.initialState.update(Map("red" -> Action("move", List("a1", "c3"))))

    state.trueFacts should be (Set(
      Relation("cell", List("b1", "red")),
      Relation("cell", List("b2", "red")),
      Relation("cell", List("c3", "red")),
      Relation("step", List("2"))
    ))
  }
}
