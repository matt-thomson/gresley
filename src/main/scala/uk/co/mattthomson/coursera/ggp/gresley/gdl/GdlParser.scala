package uk.co.mattthomson.coursera.ggp.gresley.gdl

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

class GdlParser extends RegexParsers {
  override protected val whiteSpace: Regex = """(\s|;.*)+""".r

  def game = statement.*

  private def reserved = "role" | "input" | "base" | "init"

  private def name: Parser[String] = not(reserved) ~> "[a-zA-Z0-9]+".r

  private def term = variableTerm | literalTerm
  private def variableTerm = "?" ~> name ^^ { name => VariableTerm(name) }
  private def literalTerm = name ^^ { name => LiteralTerm(name) }

  private def relation = "(" ~> name ~ term.* <~ ")" ^^ { case name ~ terms => Relation(name, terms) }

  private def statement: Parser[Statement] = conditional | fact
  private def fact: Parser[Fact] = role | relation | base | input | init

  private def role = """\(\s*role""".r ~> name <~ ")" ^^ { role => Role(role) }
  private def input = """\(\s*input""".r ~> name ~ name <~ ")" ^^ { case role ~ action => Input(Role(role), Action(action)) }

  private def base = """\(\s*base""".r ~> fact <~ ")" ^^ { fact => Base(fact) }
  private def init = """\(\s*init""".r ~> fact <~ ")" ^^ { fact => Init(fact) }

  private def conditional = """\(\s*<=""".r ~> fact ~ fact.* <~ ")" ^^ { case conclusion ~ conditions => Conditional(conclusion, conditions) }
}

