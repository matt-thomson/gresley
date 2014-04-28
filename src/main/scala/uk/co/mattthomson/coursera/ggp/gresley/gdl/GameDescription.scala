package uk.co.mattthomson.coursera.ggp.gresley.gdl

case class GameDescription(statements: Seq[Statement]) {
  lazy val constantFacts = {
    val simpleFacts: Set[Fact] = statements.collect { case f: Fact => f }.toSet
    val conditionals: Set[Conditional] = statements
      .collect { case c: Conditional => c }
      .filter { c => c.conditions.forall(_.isInstanceOf[FactCondition]) }
      .toSet

    propagateConditionals(simpleFacts, conditionals)
  }

  lazy val roles = statements.collect { case Role(LiteralTerm(role)) => role }

  lazy val initialState = {
    val simpleFacts: Set[Fact] = statements.collect { case f: Init => f }.toSet
    val conditionals: Set[Conditional] = statements
      .collect { case c: Conditional => c }
      .filter { c => c.conclusion.isInstanceOf[Init] }
      .toSet

    val allFacts = propagateConditionals(constantFacts ++ simpleFacts, conditionals)
    new GameState(this, allFacts.collect { case f: Init => f.fact })
  }

  lazy val baseFacts = constantFacts.collect {
    case Base(fact) => fact
  }

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

  lazy val stateRules = statements
    .collect { case c: Conditional => c }
    .filter { c => c.conclusion.isInstanceOf[Relation] }
    .toSet

  def actions(role: String) = constantFacts.collect {
    case Input(Role(LiteralTerm(`role`)), action) => action
  }

  private def propagateConditionals(simpleFacts: Set[Fact], conditionals: Set[Conditional]): Set[Fact] = {
    val updatedFacts = conditionals.foldLeft(simpleFacts) { case (f, conditional) => conditional.propagate(f, Map(), None) }
    if (simpleFacts == updatedFacts) simpleFacts else propagateConditionals(updatedFacts, conditionals)
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
