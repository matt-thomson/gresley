package uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical

import akka.actor.{Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{GameState, Action}
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical.MonteCarloSearchMoveSelector.{ExplorationResult, Explore}
import scala.util.Random
import akka.event.LoggingReceive

class MonteCarloTreeExplorer(state: GameState, role: String, nextMove: Action) extends Actor with ActorLogging {
  override def receive: Receive = LoggingReceive {
    case Explore =>
      val otherRoles = state.game.roles.filter(_ != role)
      val actions = selectActions(state, otherRoles) + (role -> nextMove)
      val value = exploreFrom(state.update(actions))

      sender ! ExplorationResult(nextMove, value)
  }

  private def exploreFrom(state: GameState): Int = {
    if (state.isTerminal) state.value(role)
    else {
      val actions = selectActions(state, state.game.roles)
      exploreFrom(state.update(actions))
    }
  }

  private def selectActions(state: GameState, roles: Seq[String]) = {
    roles.map { r => (r, Random.shuffle(state.legalActions(r)).head) }.toMap
  }
}
