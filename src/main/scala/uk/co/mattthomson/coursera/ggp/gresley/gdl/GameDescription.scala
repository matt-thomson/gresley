package uk.co.mattthomson.coursera.ggp.gresley.gdl

import uk.co.mattthomson.coursera.ggp.gresley.gdl.FactTag._
import com.twitter.util.Memoize

case class GameDescription(statements: Seq[Statement]) {
  lazy val constantFacts = {
    val simpleFacts = statements
      .collect { case f: Fact => f }
      .toSet
      .groupBy { f: Fact => f.tag }
      .toMap

    val rules: Set[Rule] = statements
      .collect { case c: Rule => c }
      .filter { c => c.conditions.forall(_.isInstanceOf[FactCondition]) }
      .toSet

    propagateRules(simpleFacts, rules)
  }

  lazy val roles = statements.collect { case Role(LiteralTerm(role)) => role }

  lazy val initialState = new GameState(this, constantFacts.getOrElse(classOf[Init], Set()).map { case Init(fact) => fact })

  lazy val baseFacts = constantFacts.getOrElse(classOf[Base], Set()).map { case Base(fact) => fact }

  lazy val actions = constantFacts.getOrElse(classOf[Input], Set())
    .map { case Input(Role(LiteralTerm(role)), action) => (role, action) }
    .groupBy { case (role, _) => role }
    .map { case (role, as) => (role, as.map { case (_, a) => a}.toList) }
    .toMap

  private lazy val allRules = statements
    .collect { case c: Rule => c }
    .groupBy(_.conclusion.tag)

  val rules = Memoize(rulesUnmemoized)

  private def rulesUnmemoized(conclusion: Fact) = allRules
    .getOrElse(conclusion.tag, Seq())
    .flatMap(_.bind(conclusion))

  lazy val allFacts: Map[FactTag, Set[Fact]] = {
    val relationRules = statements
      .collect { case c: Rule => c }
      .filter(_.conclusion.isInstanceOf[Relation])

    findAllFacts(constantFacts, relationRules)
  }

  // TODO better way of doing this
  def possibleValues(role: String) = (0 to 100).map(_.toString)

  private def propagateRules(soFar: Map[FactTag, Set[Fact]], rules: Set[Rule]): Map[FactTag, Set[Fact]] = {
    val updatedFacts = rules.foldLeft(soFar) { case (facts, rule) =>
      val rules = rule.bind(facts)
      if (rules.isEmpty) facts else {
        val tag = rules.head.conclusion.tag
        facts + (tag -> (facts.getOrElse(tag, Set()) ++ rules.map(_.conclusion)))
      }
    }

    def totalSize(m: Map[_, Set[_]]) = m.map { case (_, v) => v.size}.sum
    if (totalSize(soFar) == totalSize(updatedFacts)) soFar else propagateRules(updatedFacts, rules)
  }

  private def findAllFacts(soFar: Map[FactTag, Set[Fact]], rules: Seq[Rule]): Map[FactTag, Set[Fact]] = {
    val updated = rules.foldLeft(soFar) { case (oldFacts, rule) => rule.updateWithConclusions(oldFacts) }

    def totalSize(m: Map[_, Set[_]]) = m.map { case (_, v) => v.size }.sum
    if (totalSize(soFar) == totalSize(updated)) soFar else findAllFacts(updated, rules)
  }
}

object GameDescription {
  def apply(gdl: String): GameDescription = {
    val parser = new GdlParser
    val statements = parser.parseAll(parser.game, gdl)

    if (statements.successful) GameDescription(statements.get)
    else throw new IllegalArgumentException(statements.toString)
  }
}
