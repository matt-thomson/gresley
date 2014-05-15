package uk.co.mattthomson.coursera.ggp.gresley.gdl

import uk.co.mattthomson.coursera.ggp.gresley.gdl.FactTag._

case class GameDescription(statements: Seq[Statement]) {
  // TODO can this be private?
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

  lazy val baseFacts = constantFacts.getOrElse(classOf[Base], Set())
    .map { case Base(fact) => fact }

  lazy val actions = constantFacts.getOrElse(classOf[Input], Set())
    .map { case Input(Role(LiteralTerm(role)), action) => (role, action) }
    .groupBy { case (role, _) => role }
    .map { case (role, as) => (role, as.map { case (_, a) => a}.toList) }
    .toMap

  lazy val boundRules = {
    val (_, rules) = bindRules((constantFacts, Map()), statements.collect { case c: Rule => c}.toSet)
    rules
  }

  lazy val nextStateRules = boundRules.getOrElse(classOf[Next], Set())

  lazy val stateRules = {
    def conditionHasState(stateRules: Set[Rule])(condition: Condition): Boolean = condition match {
      case OrCondition(cs) => cs.exists(conditionHasState(stateRules))
      case FactCondition(fact) => if (!fact.isInstanceOf[Relation]) false else {
        val f = fact.asInstanceOf[Relation]
        stateRules.exists { stateRule =>
          if (!stateRule.conclusion.isInstanceOf[Relation]) false
          else {
            val r = stateRule.conclusion.asInstanceOf[Relation]
            f.name.matches(r.name).isDefined
          }
        }
      }
      case _ => true
    }

    def findStateRulesStep(remainingRules: Seq[Rule], stateRules: Set[Rule]): Set[Rule] = remainingRules match {
      case rule :: rest => if (rule.conditions.exists(conditionHasState(stateRules))) findStateRulesStep(rest, stateRules + rule) else findStateRulesStep(rest, stateRules)
      case Nil => stateRules
    }

    def findStateRules(rules: Seq[Rule], current: Set[Rule]): Set[Rule] = {
      val stateRules = findStateRulesStep(rules, current)
      if (stateRules.size == current.size) current else findStateRules(rules, stateRules)
    }

    val relationRules = statements
      .collect { case c: Rule => c }
      .filter { c => c.conclusion.isInstanceOf[Relation] }

    findStateRules(relationRules, Set())
  }

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

  private def bindRules(soFar: (Map[FactTag, Set[Fact]], Map[FactTag, Set[Rule]]), rules: Set[Rule]): (Map[FactTag, Set[Fact]], Map[FactTag, Set[Rule]]) = {
    val updated = rules.foldLeft(soFar) { case ((oldFacts, oldBounds), rule) =>
      val newlyBounds = rule.bind(oldFacts)
      if (newlyBounds.isEmpty) (oldFacts, oldBounds) else {
        val tag = newlyBounds.head.conclusion.tag

        val updatedBound = oldBounds.get(tag) match {
          case Some(oldBound) => oldBound ++ newlyBounds
          case None => newlyBounds
        }

        val newFact = updatedBound.map(_.conclusion)
        val updatedFacts = oldFacts.get(tag) match {
          case Some(oldFact) => oldFact ++ newFact
          case None => newFact
        }

        (oldFacts + (tag -> updatedFacts), oldBounds + (tag -> updatedBound))
      }
    }

    def totalSize(m: Map[_, Set[_]]) = m.map { case (_, v) => v.size}.sum
    if (totalSize(soFar._2) == totalSize(updated._2)) soFar else bindRules(updated, rules)
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
