package uk.co.mattthomson.coursera.ggp.gresley.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.parser.Term._

class FactSpec extends FlatSpec with ShouldMatchers {
  "A fact" should "substitute values where possible" in {
    val fact = Fact("test", List(VariableTerm("x"), "a", VariableTerm("q"), VariableTerm("z")))
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    val result = fact.substitute(values)
    result.name should be ("test")

    val expectedTerms: List[Term] = List("1", "a", VariableTerm("q"), "3")
    result.terms should be (expectedTerms)
  }

  it should "return all defined if no variables" in {
    val fact = Fact("test", List("1", "2", "3"))

    fact.isComplete should be (true)
  }

  it should "not return all defined if at least one variable" in {
    val fact = Fact("test", List("1", VariableTerm("y"), "3"))

    fact.isComplete should be (false)
  }

  it should "match a variable to a literal" in {
    val partialFact = Fact("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Fact("test", List("1", "2", "3"))

    partialFact.matches(completeFact, Map()) should be (Some(Map("y" -> "2")))
  }

  it should "update the existing matches" in {
    val partialFact = Fact("test", List(VariableTerm("x"), VariableTerm("y"), "3"))
    val completeFact = Fact("test", List("1", "2", "3"))

    partialFact.matches(completeFact, Map("x" -> "1")) should be (Some(Map("x" -> "1", "y" -> "2")))
  }

  it should "not match if there is a conflict" in {
    val partialFact = Fact("test", List(VariableTerm("x"), VariableTerm("y"), "3"))
    val completeFact = Fact("test", List("4", "2", "3"))

    partialFact.matches(completeFact, Map("x" -> "1")) should be (None)
  }

  it should "not match against a fact with a different name" in {
    val partialFact = Fact("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Fact("other", List("1", "2", "3"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match against a fact with a different number of terms" in {
    val partialFact = Fact("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Fact("test", List("1", "2"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match if a term can't be matched" in {
    val partialFact = Fact("test", List("1", VariableTerm("y"), "3"))
    val completeFact = Fact("test", List("1", "2", "4"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "not match if a term must take two values" in {
    val partialFact = Fact("test", List("1", VariableTerm("y"), VariableTerm("y")))
    val completeFact = Fact("test", List("1", "2", "3"))

    partialFact.matches(completeFact, Map()) should be (None)
  }

  it should "throw when trying to match against a fact with variables" in {
    val partialFact = Fact("test", List("1", VariableTerm("y"), "3"))

    intercept[IllegalArgumentException] {
      partialFact.matches(partialFact, Map())
    }
  }
}
