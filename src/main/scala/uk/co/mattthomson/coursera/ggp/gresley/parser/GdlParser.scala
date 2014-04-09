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

  private def relation = "(" ~> name ~ term.* <~ ")" ^^ { case name ~ terms => Relation(name, terms) }

  private def statement: Parser[Statement] = input | conditional | fact
  private def fact: Parser[Fact] = role | relation | base

  private def role = "(role" ~> name <~ ")" ^^ { case role => Role(role) }
  private def input = "(input" ~> name ~ name <~ ")" ^^ { case role ~ action => Input(Role(role), Action(action)) }

  private def base = "(base" ~> fact <~ ")" ^^ { case fact => Base(fact) }

  private def conditional = "(<=" ~> fact ~ fact.* <~ ")" ^^ { case conclusion ~ conditions => Conditional(conclusion, conditions) }
}

object GdlParser {
  case class Action(name: String)

  case class Input(role: Role, action: Action) extends Statement
}
