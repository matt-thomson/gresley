package uk.co.mattthomson.coursera.ggp.gresley.gdl

class GameDescription(private val statements: Seq[Statement]) {
  lazy val constantFacts = {
    val simpleFacts: Set[Fact] = statements.collect { case f: ConstantFact => f }.toSet
    val conditionals: Set[Conditional] = statements
      .collect { case c: Conditional => c }
      .filter {c => c.conclusion.isInstanceOf[ConstantFact] }
      .toSet

    propagateConditionals(simpleFacts, conditionals)
  }

  lazy val roles = statements.collect { case Role(LiteralTerm(role)) => role }

  lazy val initialState = {
    val simpleFacts: Set[Fact] = statements.collect { case f: Init => f }.toSet
    val conditionals: Set[Conditional] = statements
      .collect { case c: Conditional => c }
      .filter {c => c.conclusion.isInstanceOf[Init] }
      .toSet

    val allFacts = propagateConditionals(constantFacts ++ simpleFacts, conditionals)
    new GameState(this, allFacts.collect { case f: Init => f.fact })
  }

  lazy val baseFacts = constantFacts.collect {
    case Base(fact) => fact
  }

  lazy val legalMoveRules = statements
    .collect { case c: Conditional => c }
    .filter {c => c.conclusion.isInstanceOf[Legal] }
    .toSet

  lazy val nextMoveRules = statements
    .collect { case c: Conditional => c }
    .filter {c => c.conclusion.isInstanceOf[Next] }
    .toSet

  def actions(role: String) = constantFacts.collect {
    case Input(Role(LiteralTerm(`role`)), action) => action
  }

  private def propagateConditionals(simpleFacts: Set[Fact], conditionals: Set[Conditional]): Set[Fact] = {
    val updatedFacts = conditionals.foldLeft(simpleFacts) { case (f, conditional) => conditional.propagate(f, Map(), None) }
    if (simpleFacts == updatedFacts) simpleFacts else propagateConditionals(updatedFacts, conditionals)
  }

  private def canEqual(other: Any): Boolean = other.isInstanceOf[GameDescription]

  override def equals(other: Any): Boolean = other match {
    case that: GameDescription =>
      (that canEqual this) &&
        statements == that.statements
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(statements)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object GameDescription {
  def apply(statements: Seq[Statement]): GameDescription = new GameDescription(statements)

  def apply(gdl: String): GameDescription = {
    val parser = new GdlParser
    val statements = parser.parseAll(parser.game, gdl)

    if (statements.successful) GameDescription(statements.get)
    else throw new IllegalArgumentException(statements.toString)
  }
}
