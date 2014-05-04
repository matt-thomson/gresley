package uk.co.mattthomson.coursera.ggp.gresley.gdl

case class GameDescription(statements: Seq[Statement]) {
  lazy val constantFacts = {
    val simpleFacts = statements.collect { case f: Fact => f }
      .toSet
      .groupBy { f: Fact => f.getClass.asInstanceOf[Class[_]] }
      .toMap

    val conditionals: Set[Conditional] = statements
      .collect { case c: Conditional => c }
      .filter { c => c.conditions.forall(_.isInstanceOf[FactCondition]) }
      .toSet

    propagateConditionals(simpleFacts, conditionals)
  }

  lazy val roles = statements.collect { case Role(LiteralTerm(role)) => role }

  lazy val initialState = {
    val simpleFacts: Set[Fact] = statements.collect { case Init(fact) => fact }.toSet
    val conditionals: Set[Conditional] = statements
      .collect { case c: Conditional => c }
      .filter { c => c.conclusion.isInstanceOf[Init] }
      .toSet

    val allFacts: Map[Class[_], Set[Fact]] = propagateConditionals(constantFacts + (classOf[Init] -> simpleFacts), conditionals)
    new GameState(this, allFacts.getOrElse(classOf[Init], Set()))
  }

  lazy val baseFacts = constantFacts.getOrElse(classOf[Base], Set())
    .map { case Base(fact) => fact }

  lazy val legalMoveRules = statements
    .collect { case c: Conditional => c }
    .filter { c => c.conclusion.isInstanceOf[Legal] }
    .toSet

  lazy val nextStateRules = statements
    .collect { case c: Conditional => c }
    .filter { c => c.conclusion.isInstanceOf[Next] }
    .toSet

  lazy val terminalRules = statements
    .collect { case c: Conditional => c }
    .filter { c => c.conclusion == Terminal }
    .toSet

  lazy val goalRules = statements
    .collect { case c: Conditional => c }
    .filter { c => c.conclusion.isInstanceOf[Goal] }
    .toSet

  lazy val stateRules = {
    def conditionHasState(stateRules: Set[Conditional])(condition: Condition): Boolean = condition match {
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

    def findStateRulesStep(remainingRules: Seq[Conditional], stateRules: Set[Conditional]): Set[Conditional] = remainingRules match {
      case rule :: rest => if (rule.conditions.exists(conditionHasState(stateRules))) findStateRulesStep(rest, stateRules + rule) else findStateRulesStep(rest, stateRules)
      case Nil => stateRules
    }

    def findStateRules(rules: Seq[Conditional], current: Set[Conditional]): Set[Conditional] = {
      val stateRules = findStateRulesStep(rules, current)
      if (stateRules.size == current.size) current else findStateRules(rules, stateRules)
    }

    val relationRules = statements
      .collect { case c: Conditional => c }
      .filter { c => c.conclusion.isInstanceOf[Relation] }

    findStateRules(relationRules, Set())
  }

  lazy val actions = constantFacts.getOrElse(classOf[Input], Set())
    .map { case Input(Role(LiteralTerm(role)), action) => (role, action) }
    .groupBy { case (role, _) => role }
    .map { case (role, as) => (role, as.map { case (_, a) => a}) }
    .toMap

  private def propagateConditionals(simpleFacts: Map[Class[_], Set[Fact]], conditionals: Set[Conditional]): Map[Class[_], Set[Fact]] = {
    val updatedFacts = conditionals.foldLeft(simpleFacts) { case (f, conditional) => conditional.propagate(f, Map(), None) }

    def totalSize(m: Map[_, Set[_]]) = m.map { case (_, v) => v.size}.sum
    if (totalSize(simpleFacts) == totalSize(updatedFacts)) simpleFacts else propagateConditionals(updatedFacts, conditionals)
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
