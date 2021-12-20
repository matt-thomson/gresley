package uk.co.mattthomson.coursera.ggp.gresley.gdl

import org.scalatest.flatspec._
import org.scalatest.matchers._

class RuleSpec extends AnyFlatSpec with should.Matchers {
  "A rule" should "bind based on the supplied facts" in {
    val rule = Rule(Input(Role("black"), Action("move", List(VariableTerm("x")))), List(
      StateCondition(Relation("cell", List(VariableTerm("x"))))
    ))

    val fact = Base(Relation("cell", List("1")))
    val facts = Map[FactTag, Set[Fact]](fact.tag -> Set(fact))

    rule.bind(facts) should be (Set(Rule(Input(Role("black"), Action("move", List("1"))), List(
      StateCondition(Relation("cell", List("1")))
    ))))
  }

  it should "iteratively bind" in {
    val rule = Rule(Base(Relation("step", List(VariableTerm("x")))), List(
      StateCondition(Relation("step", List(VariableTerm("y")))),
      FactCondition(Relation("next", List(VariableTerm("y"), VariableTerm("x"))))
    ))

    val fact1 = Relation("next", List("1", "2"))
    val fact2 = Relation("next", List("2", "3"))
    val baseFact = Base(Relation("step", List("1")))

    val facts = Map[FactTag, Set[Fact]](
      fact1.tag -> Set(fact1, fact2),
      baseFact.tag -> Set(baseFact)
    )

    rule.bind(facts) should be (Set(
      Rule(Base(Relation("step", List("2"))), List(
        StateCondition(Relation("step", List("1"))),
        FactCondition(Relation("next", List("1", "2")))
      )),
      Rule(Base(Relation("step", List("3"))), List(
        StateCondition(Relation("step", List("2"))),
        FactCondition(Relation("next", List("2", "3")))
      ))
    ))
  }
}
