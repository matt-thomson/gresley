package uk.co.mattthomson.coursera.ggp.gresley.protocol

import scala.concurrent.duration._
import scala.util.parsing.combinator.RegexParsers
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameDescription

class GameProtocolParser extends RegexParsers {
  private def name = "[a-zA-Z0-9]+".r
  private def game = """\([^\(\)]*\)""".r.* ^^ { s => GameDescription(s.mkString("\n")) }
  private def clock = "[0-9]+".r ^^ { _.toInt.seconds }

  private def nilMove = "nil" ^^^ None
  private def someMoves = "(" ~> name.* <~ ")" ^^ { l => Some(l) }
  private def moves = nilMove ||| someMoves

  def message = info | start | play | stop | abort

  private def info = """(?i)\(info\)""".r ^^^ Info
  private def start = """(?i)\(start""".r ~> name ~ name ~ "(" ~ game ~ ")" ~ clock ~ clock <~ ")" ^^ {
    case id ~ role ~ _ ~ game ~ _ ~ startClock ~ playClock => Start(id, role, game, startClock, playClock)
  }
  private def play = """(?i)\(play""".r ~> name ~ moves <~ ")" ^^ { case id ~ moves => Play(id, moves) }
  private def stop = """(?i)\(stop""".r ~> name ~ someMoves <~ ")" ^^ { case id ~ Some(moves) => Stop(id, moves) }
  private def abort = """(?i)\(abort""".r ~> name  <~ ")" ^^ Abort
}

