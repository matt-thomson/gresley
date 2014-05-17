package uk.co.mattthomson.coursera.ggp.gresley.gdl

case class Rule(conclusion: Fact, conditions: Seq[Condition]) extends Statement {
  def bind(facts: Map[FactTag, Set[Fact]]): Set[Rule] = {
    def bindInner(facts: Map[FactTag, Set[Fact]], rules: Set[Rule]): Set[Rule] = {
      def bindCondition(values: Set[Map[String, String]], condition: Condition) = values.flatMap(condition.bindings(facts))

      val values = conditions.foldLeft(Set[Map[String, String]](Map()))(bindCondition)
      val newRules = values.map(v => Rule(conclusion.substitute(v), conditions.map(_.substitute(v))))

      if (rules == newRules) rules else {
        val newFacts = facts + (conclusion.tag -> (facts.getOrElse(conclusion.tag, Set()) ++ newRules.map(_.conclusion)))
        bindInner(newFacts, newRules)
      }
    }

    bindInner(facts, Set())
  }
  
  def updateWithConclusions(allFacts: Map[FactTag, Set[Fact]]): Map[FactTag, Set[Fact]] = {
    def bindCondition(values: Set[Map[String, String]], condition: Condition) = values flatMap { v =>
      if (conclusion.substitute(v).isComplete) Set(v) else condition.bindings(allFacts)(v)
    }

    val facts = allFacts.getOrElse(conclusion.tag, Set())

    val values = conditions.foldLeft(Set[Map[String, String]](Map()))(bindCondition)
    val updatedFacts = values.map(conclusion.substitute) ++ facts

    if (facts.size == updatedFacts.size) allFacts else {
      val newAllFacts = allFacts + (conclusion.tag -> (facts ++ updatedFacts))
      updateWithConclusions(newAllFacts)
    }
  }

  def bind(fact: Fact) = conclusion.matches(fact, Map()) match {
    case Some(values) => Some(Rule(fact, conditions.map(_.substitute(values))))
    case None => None
  }

  def prove(fact: Fact, state: GameState, actions: Option[Map[Role, Action]]) = {
    def bind(conditions: Seq[Condition]): Seq[Seq[Condition]] = conditions match {
      case condition :: rest => condition.bindings(state.game, state, actions).view.flatMap { v =>
        bind(rest.map(_.substitute(v))).map(condition.substitute(v) +: _)
      }
      case Nil => Seq(Seq())
    }

    if (fact == conclusion) bind(conditions).view.exists(_.forall(_.prove(state, actions))) else false
  }
}
