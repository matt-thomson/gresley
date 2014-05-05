package uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical

import akka.actor.{Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameState
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical.MonteCarloSearchMoveSelector.{Explore, ExplorationResult}
import scala.util.Random
import akka.event.LoggingReceive
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.TimeoutChecker
import org.joda.time.DateTime._
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical.MonteCarloSearchMoveSelector.Explore
import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameState
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical.MonteCarloSearchMoveSelector.ExplorationResult
import org.joda.time.DateTime

class MonteCarloTreeExplorer(state: GameState, role: String, endTime: DateTime) extends Actor with ActorLogging {
  val otherRoles = state.game.roles.filter(_ != role)

  override def receive: Receive = {
    case Explore(nextMove) =>
      val actions = selectActions(state, otherRoles) + (role -> nextMove)
      val value = exploreFrom(state.update(actions))

      sender ! ExplorationResult(nextMove, value)
  }

  private def exploreFrom(state: GameState): Int = {
    if (now.isAfter(endTime)) 0
    else if (state.isTerminal) state.value(role)
    else {
      val actions = selectActions(state, state.game.roles)
      exploreFrom(state.update(actions))
    }
  }

  private def selectActions(state: GameState, roles: Seq[String]) = {
    roles.map { r => (r, Random.shuffle(state.game.actions(r)).find(state.isLegal(r)).get) }.toMap
  }
}
