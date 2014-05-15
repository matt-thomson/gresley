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

  def prove(fact: Fact, state: GameState, actions: Option[Map[Role, Action]]) =
    if (fact == conclusion) conditions.forall(_.prove(state, actions))
    else false
}
