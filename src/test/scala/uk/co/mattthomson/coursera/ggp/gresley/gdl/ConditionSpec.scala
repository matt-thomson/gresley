package uk.co.mattthomson.coursera.ggp.gresley.gdl

import org.mockito.Mockito._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar

class ConditionSpec extends FlatSpec with ShouldMatchers with MockitoSugar {
  "A fact condition" should "bind variables to facts" in {
    val fact = Relation("cell", List(VariableTerm("x")))
    val condition = FactCondition(fact)

    val fact1 = Relation("cell", List("1"))
    val fact2 = Relation("cell", List("2"))
    val baseFact = Base(Relation("cell", List("3")))

    val allFacts = Map[FactTag, Set[Fact]](
      fact1.tag -> Set(fact1, fact2),
      baseFact.tag -> Set(baseFact)
    )

    condition.bindings(allFacts)(Map()) should be (Set(
      Map("x" -> "1"),
      Map("x" -> "2")
    ))
  }

  it should "update existing bindings" in {
    val fact = Relation("cell", List(VariableTerm("x"), VariableTerm("y")))
    val condition = FactCondition(fact)

    val fact1 = Relation("cell", List("1", "2"))

    val allFacts = Map[FactTag, Set[Fact]](
      fact1.tag -> Set(fact1)
    )

    condition.bindings(allFacts)(Map("y" -> "2", "z" -> "3")) should be (Set(
      Map("x" -> "1", "y" -> "2", "z" -> "3")
    ))
  }

  it should "substitute correctly" in {
    val fact = Relation("cell", List(VariableTerm("x"), VariableTerm("y")))
    val condition = FactCondition(fact)

    val values = Map("x" -> "1", "z" -> "2")

    condition.substitute(values) should be (FactCondition(Relation("cell", List("1", VariableTerm("y")))))
  }

  it should "prove true using the state" in {
    val state = mock[GameState]

    val fact = Relation("cell", List("1"))
    when(state.prove).thenReturn { f: (Fact, Option[Map[Role, Action]]) => true }

    val condition = FactCondition(fact)
    condition.prove(state) should be (true)
  }

  it should "prove false using the state" in {
    val state = mock[GameState]

    val fact = Relation("cell", List("1"))
    when(state.prove).thenReturn { f: (Fact, Option[Map[Role, Action]]) => false }

    val condition = FactCondition(fact)
    condition.prove(state) should be (false)
  }

  "A state condition" should "bind variables to base facts" in {
    val stateFact = Relation("cell", List(VariableTerm("x")))
    val condition = StateCondition(stateFact)

    val fact = Relation("cell", List("1"))
    val baseFact1 = Base(Relation("cell", List("2")))
    val baseFact2 = Base(Relation("cell", List("3")))

    val allFacts = Map[FactTag, Set[Fact]](
      fact.tag -> Set(fact),
      baseFact1.tag -> Set(baseFact1, baseFact2)
    )

    condition.bindings(allFacts)(Map()) should be (Set(
      Map("x" -> "2"),
      Map("x" -> "3")
    ))
  }

  it should "update existing bindings" in {
    val fact = Relation("cell", List(VariableTerm("x"), VariableTerm("y")))
    val condition = StateCondition(fact)

    val fact1 = Base(Relation("cell", List("1", "2")))

    val allFacts = Map[FactTag, Set[Fact]](
      fact1.tag -> Set(fact1)
    )

    condition.bindings(allFacts)(Map("y" -> "2", "z" -> "3")) should be (Set(
      Map("x" -> "1", "y" -> "2", "z" -> "3")
    ))
  }

  it should "substitute correctly" in {
    val fact = Relation("cell", List(VariableTerm("x"), VariableTerm("y")))
    val condition = StateCondition(fact)

    val values = Map("x" -> "1", "z" -> "2")

    condition.substitute(values) should be (StateCondition(Relation("cell", List("1", VariableTerm("y")))))
  }

  it should "prove true using the state" in {
    val state = mock[GameState]

    val fact = Relation("cell", List("1"))
    when(state.trueFacts).thenReturn(Set[Fact](fact))

    val condition = StateCondition(fact)
    condition.prove(state) should be (true)
  }

  it should "prove false using the state" in {
    val state = mock[GameState]

    val fact = Relation("cell", List("1"))
    when(state.trueFacts).thenReturn(Set[Fact](fact))

    val otherFact = Relation("cell", List("2"))
    val condition = StateCondition(otherFact)
    condition.prove(state) should be (false)
  }

  "An action condition" should "bind variables to input facts" in {
    val condition = ActionCondition(Role(VariableTerm("x")), Action("move", List(VariableTerm("y"))))

    val inputFact1 = Input(Role("black"), Action("move", List("left")))
    val inputFact2 = Input(Role("white"), Action("move", List("right")))

    val allFacts = Map[FactTag, Set[Fact]](
      inputFact1.tag -> Set(inputFact1, inputFact2)
    )

    condition.bindings(allFacts)(Map()) should be (Set(
      Map("x" -> "black", "y" -> "left"),
      Map("x" -> "white", "y" -> "right")
    ))
  }

  it should "update existing bindings" in {
    val condition = ActionCondition(Role(VariableTerm("x")), Action("move", List(VariableTerm("y"))))

    val inputFact1 = Input(Role("black"), Action("move", List("left")))
    val inputFact2 = Input(Role("white"), Action("move", List("right")))

    val allFacts = Map[FactTag, Set[Fact]](
      inputFact1.tag -> Set(inputFact1, inputFact2)
    )

    condition.bindings(allFacts)(Map("x" -> "black", "z" -> "1")) should be (Set(
      Map("x" -> "black", "y" -> "left", "z" -> "1")
    ))
  }

  it should "substitute correctly" in {
    val condition = ActionCondition(Role(VariableTerm("x")), Action("move", List(VariableTerm("y"), VariableTerm("z"))))

    val values = Map("x" -> "white", "z" -> "2")

    condition.substitute(values) should be (ActionCondition(Role("white"), Action("move", List(VariableTerm("y"), "2"))))
  }

  it should "prove true using the actions" in {
    val state = mock[GameState]

    val role = Role("black")
    val action = Action("move", List("left"))
    val condition = ActionCondition(role, action)
    val actions = Map(role -> action)

    condition.prove(state, Some(actions)) should be (true)
  }

  it should "prove false using the actions" in {
    val state = mock[GameState]

    val role = Role("black")
    val action = Action("move", List("left"))
    val condition = ActionCondition(role, action)
    val actions = Map(role -> Action("move", List("right")))

    condition.prove(state, Some(actions)) should be (false)
  }

  it should "prove false when there are no actions" in {
    val state = mock[GameState]

    val role = Role("black")
    val action = Action("move", List("left"))
    val condition = ActionCondition(role, action)
    val actions = Map(role -> Action("move", List("right")))

    condition.prove(state) should be (false)
  }

  "A false condition" should "bind by delegating" in {
    val fact = Relation("cell", List(VariableTerm("x")))
    val condition = FalseCondition(FactCondition(fact))

    val fact1 = Relation("cell", List("1"))
    val fact2 = Relation("cell", List("2"))
    val baseFact = Base(Relation("cell", List("3")))

    val allFacts = Map[FactTag, Set[Fact]](
      fact1.tag -> Set(fact1, fact2),
      baseFact.tag -> Set(baseFact)
    )

    condition.bindings(allFacts)(Map()) should be (Set(
      Map("x" -> "1"),
      Map("x" -> "2")
    ))
  }

  it should "substitute correctly" in {
    val fact = Relation("cell", List(VariableTerm("x"), VariableTerm("y")))
    val condition = FalseCondition(FactCondition(fact))

    val values = Map("x" -> "1", "z" -> "2")

    condition.substitute(values) should be (FalseCondition(FactCondition(Relation("cell", List("1", VariableTerm("y"))))))
  }

  it should "prove true using the state" in {
    val state = mock[GameState]

    val fact = Relation("cell", List("1"))
    when(state.prove).thenReturn { f: (Fact, Option[Map[Role, Action]]) => false }

    val condition = FalseCondition(FactCondition(fact))
    condition.prove(state) should be (true)
  }

  it should "prove false using the state" in {
    val state = mock[GameState]

    val fact = Relation("cell", List("1"))
    when(state.prove).thenReturn { f: (Fact, Option[Map[Role, Action]]) => true }

    val condition = FalseCondition(FactCondition(fact))
    condition.prove(state) should be (false)
  }

  "An or condition" should "bind by delegating" in {
    val fact1 = Relation("cell", List(VariableTerm("x")))
    val fact2 = Relation("block", List(VariableTerm("y")))
    val condition = OrCondition(List(FactCondition(fact1), StateCondition(fact2)))

    val fact = Relation("cell", List("1"))
    val baseFact1 = Base(Relation("block", List("2")))

    val allFacts = Map[FactTag, Set[Fact]](
      fact.tag -> Set(fact),
      baseFact1.tag -> Set(baseFact1)
    )

    condition.bindings(allFacts)(Map()) should be (Set(
      Map("x" -> "1"),
      Map("y" -> "2")
    ))
  }

  it should "substitute correctly" in {
    val fact1 = Relation("cell", List(VariableTerm("x"), VariableTerm("y")))
    val fact2 = Relation("row", List(VariableTerm("z")))
    val condition = OrCondition(List(FactCondition(fact1), FactCondition(fact2)))

    val values = Map("x" -> "1", "z" -> "2")

    condition.substitute(values) should be (OrCondition(List(
      FactCondition(Relation("cell", List("1", VariableTerm("y")))),
      FactCondition(Relation("row", List("2")))
    )))
  }

  it should "prove true using the state" in {
    val state = mock[GameState]

    val fact1 = Relation("cell", List("1"))
    val fact2 = Relation("cell", List("2"))
    when(state.trueFacts).thenReturn(Set[Fact](fact2))

    val condition = OrCondition(List(StateCondition(fact1), StateCondition(fact2)))
    condition.prove(state) should be (true)
  }

  it should "prove false using the state" in {
    val state = mock[GameState]

    val fact1 = Relation("cell", List("1"))
    val fact2 = Relation("cell", List("2"))
    val fact3 = Relation("cell", List("3"))
    when(state.trueFacts).thenReturn(Set[Fact](fact3))

    val condition = OrCondition(List(StateCondition(fact1), StateCondition(fact2)))
    condition.prove(state) should be (false)
  }

  "A distinct condition" should "keep when the terms are distinct" in {
    val condition = DistinctCondition(List("1", VariableTerm("x"), VariableTerm("y")))
    val values = Map("x" -> "2", "y" -> "3")

    condition.bindings(Map())(values) should be (Set(values))
  }

  it should "discard when the variables match" in {
    val condition = DistinctCondition(List("1", VariableTerm("x"), VariableTerm("y")))
    val values = Map("x" -> "2", "y" -> "2")

    condition.bindings(Map())(values) should be (Set())
  }

  it should "discard when a variable matches a literal" in {
    val condition = DistinctCondition(List("1", VariableTerm("x"), VariableTerm("y")))
    val values = Map("x" -> "2", "y" -> "1")

    condition.bindings(Map())(values) should be (Set())
  }

  it should "substitute correctly" in {
    val condition = DistinctCondition(List(VariableTerm("x"), VariableTerm("y"), VariableTerm("z")))

    val values = Map("x" -> "1", "z" -> "2")

    condition.substitute(values) should be (DistinctCondition(List("1", VariableTerm("y"), "2")))
  }

  it should "prove true always" in {
    val state = mock[GameState]

    val condition = DistinctCondition(List(VariableTerm("x"), VariableTerm("y"), VariableTerm("z")))
    condition.prove(state) should be (true)
  }
}
