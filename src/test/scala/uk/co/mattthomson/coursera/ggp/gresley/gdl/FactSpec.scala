package uk.co.mattthomson.coursera.ggp.gresley.gdl

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Term._

class FactSpec extends FlatSpec with ShouldMatchers {
  "A role" should "substitute its name" in {
    val fact = Role(VariableTerm("x"))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result should be (Role("1"))
  }

  it should "match terms to values" in {
    val partialFact = Role(VariableTerm("x"))
    val completeFact = Role("1")

    partialFact.matches(completeFact, Map("y" -> "2")) should be (Some(Map("x" -> "1", "y" -> "2")))
  }

  it should "not allow contradictions" in {
    val partialFact = Role(VariableTerm("x"))
    val completeFact = Role("1")

    partialFact.matches(completeFact, Map("x" -> "2")) should be (None)
  }

  it should "not match against a different fact" in {
    val partialFact = Role(VariableTerm("x"))
    val completeFact = Relation("test", List("1"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  "A relation" should "substitute values where possible" in {
    val fact = Relation(VariableTerm("y"), List(VariableTerm("x"), "a", VariableTerm("q"), VariableTerm("z")))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    fact.substitute(values) should be (Relation("2", List("1", "a", VariableTerm("q"), "3")))
  }

  it should "match terms to values" in {
    val partialFact = Relation(VariableTerm("z"), List("1", VariableTerm("y"), "3"))
    val completeFact = Relation("test", List("1", "2", "3"))

    partialFact.matches(completeFact, Map("x" -> "1")) should be (Some(Map("x" -> "1", "y" -> "2", "z" -> "test")))
  }

  it should "not match against a relation with a different name" in {
    val partialFact = Relation("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Relation("other", List("1", "2", "3"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different fact" in {
    val partialFact = Relation("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  "An action" should "substitute values where possible" in {
    val fact = Action("test", List(VariableTerm("x"), "a", VariableTerm("q"), VariableTerm("z")))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result.name should be ("test")

    val expectedTerms: List[Term] = List("1", "a", VariableTerm("q"), "3")
    result.terms should be (expectedTerms)
  }

  it should "match terms to values" in {
    val partialFact = Action("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Action("test", List("1", "2", "3"))

    partialFact.matches(completeFact, Map("x" -> "1")) should be (Some(Map("x" -> "1", "y" -> "2")))
  }

  it should "not match against an action with a different name" in {
    val partialFact = Action("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Action("other", List("1", "2", "3"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different fact" in {
    val partialFact = Action("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "print a simple action" in {
    Action("test", Nil).toString should be ("test")
  }

  it should "print an action with terms" in {
    Action("test", List("a", "b")).toString should be ("(test a b)")
  }

  "A base rule" should "substitute values where possible" in {
    val fact = Base(Relation("test", List("1", VariableTerm("y"), "3")))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result.fact should be(Relation("test", List("1", "2", "3")))
  }

  it should "match against a matching fact" in {
    val partialFact = Base(Relation("test", List("1", VariableTerm("y"), "3")))
    val completeFact = Base(Relation("test", List("1", "2", "3")))

    partialFact.matches(completeFact, Map("x" -> "1")) should be (Some(Map("x" -> "1", "y" -> "2")))
  }

  it should "not match against a different fact" in {
    val partialFact = Base(Relation("test", List("1", VariableTerm("y"), "3")))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different inner fact" in {
    val partialFact = Base(Relation("test", List("1", VariableTerm("y"), "3")))
    val completeFact = Base(Role("black"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  "An input" should "substitute values where possible" in {
    val fact = Input(Role(VariableTerm("x")), Action("robot", List(VariableTerm("z"))))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result should be (Input(Role("1"), Action("robot", List("3"))))
  }

  it should "match against a matching input" in {
    val partialFact = Input(Role(VariableTerm("x")), Action("robot", List(VariableTerm("y"))))
    val completeFact = Input(Role("black"), Action("robot", List("up")))

    partialFact.matches(completeFact, Map()) should be (Some(Map("x" -> "black", "y" -> "up")))
  }

  it should "not match against a non-matching input" in {
    val partialFact = Input(Role("white"), Action("robot", Nil))
    val completeFact = Input(Role("black"), Action("up", Nil))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different fact" in {
    val partialFact = Input(Role(VariableTerm("x")), Action("robot", Nil))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match if the parts are inconsistent" in {
    val partialFact = Input(Role(VariableTerm("x")), Action("robot", List(VariableTerm("x"))))
    val completeFact = Input(Role("black"), Action("up", Nil))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  "An init rule" should "substitute values where possible" in {
    val fact = Init(Relation("test", List("1", VariableTerm("y"), "3")))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result.fact should be(Relation("test", List("1", "2", "3")))
  }

  it should "match against a matching fact" in {
    val partialFact = Init(Relation("test", List("1", VariableTerm("y"), "3")))
    val completeFact = Init(Relation("test", List("1", "2", "3")))

    partialFact.matches(completeFact, Map("x" -> "1")) should be (Some(Map("x" -> "1", "y" -> "2")))
  }

  it should "not match against a different fact" in {
    val partialFact = Init(Relation("test", List("1", VariableTerm("y"), "3")))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different inner fact" in {
    val partialFact = Init(Relation("test", List("1", VariableTerm("y"), "3")))
    val completeFact = Init(Role("black"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  "A legal input" should "substitute values where possible" in {
    val fact = Legal(Role(VariableTerm("x")), Action("robot", List(VariableTerm("z"))))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result should be (Legal(Role("1"), Action("robot", List("3"))))
  }

  it should "match against a matching legal" in {
    val partialFact = Legal(Role(VariableTerm("x")), Action("robot", List(VariableTerm("y"))))
    val completeFact = Legal(Role("black"), Action("robot", List("up")))

    partialFact.matches(completeFact, Map()) should be (Some(Map("x" -> "black", "y" -> "up")))
  }

  it should "not match against a non-matching legal" in {
    val partialFact = Legal(Role("white"), Action("robot", Nil))
    val completeFact = Legal(Role("black"), Action("up", Nil))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different fact" in {
    val partialFact = Legal(Role(VariableTerm("x")), Action("robot", Nil))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match if the parts are inconsistent" in {
    val partialFact = Legal(Role(VariableTerm("x")), Action("robot", List(VariableTerm("x"))))
    val completeFact = Legal(Role("black"), Action("up", Nil))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  "A next rule" should "substitute values where possible" in {
    val fact = Next(Relation("test", List("1", VariableTerm("y"), "3")))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result.fact should be(Relation("test", List("1", "2", "3")))
  }

  it should "match against a matching fact" in {
    val partialFact = Next(Relation("test", List("1", VariableTerm("y"), "3")))
    val completeFact = Next(Relation("test", List("1", "2", "3")))

    partialFact.matches(completeFact, Map("x" -> "1")) should be (Some(Map("x" -> "1", "y" -> "2")))
  }

  it should "not match against a different fact" in {
    val partialFact = Next(Relation("test", List("1", VariableTerm("y"), "3")))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different inner fact" in {
    val partialFact = Next(Relation("test", List("1", VariableTerm("y"), "3")))
    val completeFact = Next(Role("black"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  "A goal" should "substitute values where possible" in {
    val fact = Goal(Role(VariableTerm("x")), VariableTerm("y"))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result should be(Goal(Role("1"), LiteralTerm("2")))
  }

  it should "match against a matching fact" in {
    val partialFact = Goal(Role(VariableTerm("x")), VariableTerm("y"))
    val completeFact = Goal(Role("1"), LiteralTerm("2"))

    partialFact.matches(completeFact, Map()) should be (Some(Map("x" -> "1", "y" -> "2")))
  }

  it should "not match against a different fact" in {
    val partialFact = Goal(Role(VariableTerm("y")), LiteralTerm("50"))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different score" in {
    val partialFact = Goal(Role(VariableTerm("y")), LiteralTerm("50"))
    val completeFact = Goal(Role("2"), LiteralTerm("100"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "return the value if valid" in {
    Goal(Role("black"), LiteralTerm("50")).value should be (50)
  }

  it should "throw an exception when getting the value of a variable" in {
    intercept[IllegalArgumentException] {
      Goal(Role("black"), VariableTerm("x")).value
    }
  }

  "A terminal" should "be unchanged by substitution" in {
    val fact = Terminal
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result should be(Terminal)
  }

  it should "match against a matching fact" in {
    val partialFact = Terminal
    val completeFact = Terminal

    partialFact.matches(completeFact, Map("x" -> "1")) should be (Some(Map("x" -> "1")))
  }

  it should "not match against a different fact" in {
    val partialFact = Terminal
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }
}
