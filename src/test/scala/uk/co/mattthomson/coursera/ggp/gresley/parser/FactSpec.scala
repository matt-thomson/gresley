package uk.co.mattthomson.coursera.ggp.gresley.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.parser.Term._

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
    val fact = Input(Role(VariableTerm("x")), Action(VariableTerm("y")))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result should be (Input(Role("1"), Action("2")))
  }

  it should "match against a matching input" in {
    val partialFact = Input(Role(VariableTerm("x")), Action(VariableTerm("y")))
    val completeFact = Input(Role("black"), Action("up"))

    partialFact.matches(completeFact, Map()) should be (Some(Map("x" -> "black", "y" -> "up")))
  }

  it should "not match against a non-matching input" in {
    val partialFact = Input(Role("white"), Action(VariableTerm("y")))
    val completeFact = Input(Role("black"), Action("up"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a different fact" in {
    val partialFact = Input(Role(VariableTerm("x")), Action(VariableTerm("y")))
    val completeFact = Role("black")

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match if the parts are inconsistent" in {
    val partialFact = Input(Role(VariableTerm("x")), Action(VariableTerm("x")))
    val completeFact = Input(Role("black"), Action("up"))

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
}
