package uk.co.mattthomson.coursera.ggp.gresley.gdl

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Term._

class TermSpec extends FlatSpec with ShouldMatchers {
  "A literal term" should "be constructable from a string" in {
    val term: Term = "y"

    term should be (LiteralTerm("y"))
  }

  it should "not be substitutable" in {
    val term = LiteralTerm("y")
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    term.substitute(values) should be (term)
  }

  it should "match against itself" in {
    val term: Term = "y"

    term.matches(term) should be (Some(Map()))
  }

  it should "not match against another literal" in {
    val term: Term = "y"

    term.matches("z") should be (None)
  }

  it should "match against a variable" in {
    val term: Term = "y"

    term.matches(VariableTerm("x")) should be (Some(Map("x" -> "y")))
  }

  "A variable term" should "substitute for a matching value" in {
    val term = VariableTerm("y")
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    term.substitute(values) should be (LiteralTerm("2"))
  }

  it should "not substitute if no matching value" in {
    val term = VariableTerm("a")
    val values = Map("x" -> "1", "y" -> "2", "z" -> "3")

    term.substitute(values) should be(term)
  }

  it should "match against itself" in {
    val term = VariableTerm("x")

    term.matches(term) should be (Some(Map()))
  }

  it should "not match against another variable" in {
    val term = VariableTerm("x")

    term.matches(VariableTerm("y")) should be (None)
  }

  it should "match against a literal" in {
    val term = VariableTerm("x")

    term.matches("y") should be (Some(Map("x" -> "y")))
  }

  "The term matcher" should "match a variable to a literal" in {
    val partialTerms: List[Term] = List("1", VariableTerm("y"), "3")
    val completeTerms: List[Term] = List("1", "2", "3")

    Term.matchTerms(partialTerms, completeTerms, Map()) should be (Some(Map("y" -> "2")))
  }

  it should "update the existing matches" in {
    val partialTerms: List[Term] = List(VariableTerm("x"), VariableTerm("y"), "3")
    val completeTerms: List[Term] = List("1", "2", "3")

    Term.matchTerms(partialTerms, completeTerms, Map("x" -> "1")) should be (Some(Map("x" -> "1", "y" -> "2")))
  }

  it should "not match if there is a conflict" in {
    val partialTerms: List[Term] = List(VariableTerm("x"), VariableTerm("y"), "3")
    val completeTerms: List[Term] = List("4", "2", "3")

    Term.matchTerms(partialTerms, completeTerms, Map("x" -> "1")) should be (None)
  }

  it should "not match against a different number of terms" in {
    val partialTerms: List[Term] = List("1", VariableTerm("y"), "3")
    val completeTerms: List[Term] = List("1", "2")

    Term.matchTerms(partialTerms, completeTerms, Map()) should be (None)
  }

  it should "not match if a term can't be matched" in {
    val partialTerms: List[Term] = List("1", VariableTerm("y"), "3")
    val completeTerms: List[Term] = List("1", "2", "4")

    Term.matchTerms(partialTerms, completeTerms, Map()) should be (None)
  }

  it should "not match if a term must take two values" in {
    val partialTerms: List[Term] = List("1", VariableTerm("y"), VariableTerm("y"))
    val completeTerms: List[Term] = List("1", "2", "3")

    Term.matchTerms(partialTerms, completeTerms, Map()) should be (None)
  }

  it should "throw when trying to match against a fact with variables" in {
    val partialTerms: List[Term] = List("1", VariableTerm("y"), "3")

    intercept[IllegalArgumentException] {
      Term.matchTerms(partialTerms, partialTerms, Map())
    }
  }
}
