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

    partialFact.matches(completeFact, Map()) should be (Some(Map("x" -> "1")))
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

    partialFact.matches(completeFact, Map()) should be (Some(Map("y" -> "2")))
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
}
