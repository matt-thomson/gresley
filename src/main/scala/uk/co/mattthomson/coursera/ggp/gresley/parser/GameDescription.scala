package uk.co.mattthomson.coursera.ggp.gresley.parser

import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser._

class GameDescription(val roles: Seq[String], val relations: Map[String, Seq[RelationArgs]]) {
}

object GameDescription {
  def apply(statements: Seq[Statement]): GameDescription = {
    val roles = statements.collect { case Role(role) => role }
    val relations = statements.collect { case r: Relation => r }
      .groupBy(_.name)
      .mapValues { relations => relations.map { r => RelationArgs(r.terms.map(extractLiteralTerm)) } }

    val relationRules = statements.collect { case Conditional(r: Relation, conditions) => (r, conditions) }

    new GameDescription(roles, relations)
  }

  private def extractLiteralTerm(term: Term) = term match {
    case LiteralTerm(t) => t
    case VariableTerm(_) => throw new IllegalArgumentException("Not a literal term")
  }
}
