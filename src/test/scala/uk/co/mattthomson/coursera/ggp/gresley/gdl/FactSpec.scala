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
    val fact = Relation("test", List(VariableTerm("x"), "a", VariableTerm("q"), VariableTerm("z")))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result.name should be ("test")

    val expectedTerms: List[Term] = List("1", "a", VariableTerm("q"), "3")
    result.terms should be (expectedTerms)
  }

  it should "match terms to values" in {
    val partialFact = Relation("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Relation("test", List("1", "2", "3"))

    partialFact.matches(completeFact, Map("x" -> "1")) should be (Some(Map("x" -> "1", "y" -> "2")))
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
    val fact = Input(Role(VariableTerm("x")), Action(VariableTerm("y"), List(VariableTerm("z"))))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result should be (Input(Role("1"), Action("2", List("3"))))
  }

  it should "match against a matching input" in {
    val partialFact = Input(Role(VariableTerm("x")), Action(VariableTerm("y"), List("1")))
    val completeFact = Input(Role("black"), Action("up", List("1")))

    partialFact.matches(completeFact, Map()) should be (Some(Map("x" -> "black", "y" -> "up")))
  }

  it should "not match against a non-matching input" in {
    val partialFact = Input(Role("white"), Action(VariableTerm("y"), Nil))
    val completeFact = Input(Role("black"), Action("up", Nil))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different fact" in {
    val partialFact = Input(Role(VariableTerm("x")), Action(VariableTerm("y"), Nil))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match if the parts are inconsistent" in {
    val partialFact = Input(Role(VariableTerm("x")), Action(VariableTerm("x"), Nil))
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
    val fact = Legal(Role(VariableTerm("x")), Action(VariableTerm("y"), List(VariableTerm("z"))))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result should be (Legal(Role("1"), Action("2", List("3"))))
  }

  it should "match against a matching legal" in {
    val partialFact = Legal(Role(VariableTerm("x")), Action(VariableTerm("y"), List("1")))
    val completeFact = Legal(Role("black"), Action("up", List("1")))

    partialFact.matches(completeFact, Map()) should be (Some(Map("x" -> "black", "y" -> "up")))
  }

  it should "not match against a non-matching legal" in {
    val partialFact = Legal(Role("white"), Action(VariableTerm("y"), Nil))
    val completeFact = Legal(Role("black"), Action("up", Nil))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different fact" in {
    val partialFact = Legal(Role(VariableTerm("x")), Action(VariableTerm("y"), Nil))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match if the parts are inconsistent" in {
    val partialFact = Legal(Role(VariableTerm("x")), Action(VariableTerm("x"), Nil))
    val completeFact = Legal(Role("black"), Action("up", Nil))

    partialFact.matches(completeFact, Map()) should be (None)
  }
}
