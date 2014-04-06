package uk.co.mattthomson.coursera.ggp.gresley.parser

import scala.util.parsing.combinator.RegexParsers
import uk.co.mattthomson.coursera.ggp.gresley.parser.GdlParser._
import scala.util.matching.Regex

class GdlParser extends RegexParsers {
  override protected val whiteSpace: Regex = """(\s|;.*)+""".r

  def game = statement.*

  private def reserved = "role" | "input" | "base"

  private def name: Parser[String] = not(reserved) ~> "[a-zA-Z0-9]+".r

  private def term = variableTerm | literalTerm
  private def variableTerm = "?" ~> name ^^ { case name => VariableTerm(name) }
  private def literalTerm = name ^^ { case name => LiteralTerm(name) }

  private def proposition = "(" ~> name ~ term.* <~ ")" ^^ { case name ~ terms => Proposition(name, terms) }
  private def relation = "(" ~> name ~ term.* <~ ")" ^^ { case name ~ terms => Relation(name, terms) }

  private def statement: Parser[Statement] = role | input | base | baseRule | relation

  private def role = "(role" ~> name <~ ")" ^^ { case role => Role(role) }
  private def input = "(input" ~> name ~ name <~ ")" ^^ { case role ~ action => Input(Role(role), Action(action)) }

  private def base = "(base" ~> proposition <~ ")" ^^ { case prop => Base(prop) }
  private def baseRule = "(<=" ~> base ~ (base | relation).* <~ ")" ^^ { case conclusion ~ conditions => Conditional(conclusion, conditions) }
}

object GdlParser {
  trait Term
  case class LiteralTerm(name: String) extends Term
  case class VariableTerm(name: String) extends Term

  case class Proposition(name: String, terms: Seq[Term])
  case class Action(name: String)

  trait Statement
  case class Role(name: String) extends Statement
  case class Input(role: Role, action: Action) extends Statement
  case class Base(proposition: Proposition) extends Statement
  case class Relation(name: String, terms: Seq[Term]) extends Statement
  case class Conditional(conclusion: Statement, conditions: Seq[Statement]) extends Statement
}
