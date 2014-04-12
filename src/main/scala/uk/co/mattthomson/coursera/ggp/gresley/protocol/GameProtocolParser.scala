package uk.co.mattthomson.coursera.ggp.gresley.protocol

import scala.concurrent.duration._
import scala.util.parsing.combinator.RegexParsers
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameDescription

class GameProtocolParser extends RegexParsers {
  private def name = """[A-Za-z0-9\-\.]+""".r
  private def game = """\([^\(\)]*\)""".r.* ^^ { s => GameDescription(s.mkString("\n")) }
  private def clock = "[0-9]+".r ^^ { _.toInt.seconds }

  private def nilMove = """(?i)nil""".r ^^^ None
  private def someMoves = "(" ~> name.* <~ ")" ^^ { l => Some(l) }
  private def moves = nilMove ||| someMoves

  def message = info | start | play | stop | abort

  private def info = """(?i)\(\s*info\s*\)""".r ^^^ Info
  private def start = """(?i)\(\s*start""".r ~> name ~ name ~ "(" ~ game ~ ")" ~ clock ~ clock <~ ")" ^^ {
    case id ~ role ~ _ ~ game ~ _ ~ startClock ~ playClock => Start(id, role, game, startClock, playClock)
  }
  private def play = """(?i)\(\s*play""".r ~> name ~ moves <~ ")" ^^ { case id ~ moves => Play(id, moves) }
  private def stop = """(?i)\(\s*stop""".r ~> name ~ someMoves <~ ")" ^^ { case id ~ Some(moves) => Stop(id, moves) }
  private def abort = """(?i)\(\s*abort""".r ~> name  <~ ")" ^^ Abort
}

