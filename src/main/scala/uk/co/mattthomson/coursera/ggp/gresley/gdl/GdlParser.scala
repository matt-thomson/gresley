package uk.co.mattthomson.coursera.ggp.gresley.gdl

import scala.concurrent.duration._
import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex
import scala.Some

class GdlParser extends RegexParsers {
  override protected val whiteSpace: Regex = """(\s|;.*)+""".r

  def game = statement.*

  private def reserved = "role" | "input" | "base" | "init" | "legal"

  private def name: Parser[String] = not(reserved) ~> "[a-zA-Z0-9]+".r
  private def id: Parser[String] = """[a-zA-Z0-9\.\-]+""".r

  private def term = variableTerm | literalTerm
  private def variableTerm = "?" ~> name ^^ { name => VariableTerm(name) }
  private def literalTerm = name ^^ { name => LiteralTerm(name) }

  private def statement: Parser[Statement] = conditional | fact
  private def fact: Parser[Fact] = role | relation | base | input | init

  private def role = """\(\s*role""".r ~> name <~ ")" ^^ { role => Role(role) }
  private def relation = "(" ~> name ~ term.* <~ ")" ^^ { case name ~ terms => Relation(name, terms) }
  private def input = """\(\s*input""".r ~> name ~ name <~ ")" ^^ { case role ~ action => Input(Role(role), Action(action)) }
  private def base = """\(\s*base""".r ~> fact <~ ")" ^^ { fact => Base(fact) }
  private def init = """\(\s*init""".r ~> fact <~ ")" ^^ { fact => Init(fact) }

  private def condition: Parser[Condition] = factCondition

  private def factCondition = fact ^^ { fact => FactCondition(fact) }

  private def conditional = """\(\s*<=""".r ~> fact ~ condition.* <~ ")" ^^ { case conclusion ~ conditions => Conditional(conclusion, conditions) }

  def message = info | start | play | stop | abort

  private def clock = "[0-9]+".r ^^ { _.toInt.seconds }

  private def nilMove = """(?i)nil""".r ^^^ None
  private def someMoves = "(" ~> name.* <~ ")" ^^ { l => Some(l) }
  private def moves = nilMove ||| someMoves

  private def info = """(?i)\(\s*info\s*\)""".r ^^^ Info
  private def start = """(?i)\(\s*start""".r ~> id ~ name ~ "(" ~ game ~ ")" ~ clock ~ clock <~ ")" ^^ {
    case id ~ role ~ _ ~ game ~ _ ~ startClock ~ playClock => Start(id, role, new GameDescription(game), startClock, playClock)
  }
  private def play = """(?i)\(\s*play""".r ~> id ~ moves <~ ")" ^^ { case id ~ moves => Play(id, moves) }
  private def stop = """(?i)\(\s*stop""".r ~> id ~ someMoves <~ ")" ^^ { case id ~ Some(moves) => Stop(id, moves) }
  private def abort = """(?i)\(\s*abort""".r ~> id  <~ ")" ^^ Abort
}

