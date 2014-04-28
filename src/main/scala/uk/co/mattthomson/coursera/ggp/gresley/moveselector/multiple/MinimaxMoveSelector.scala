package uk.co.mattthomson.coursera.ggp.gresley.moveselector.multiple

import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Action, GameState}
import akka.actor.{Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{SelectedMove, Play, Initialized, Initialize}

class MinimaxMoveSelector extends Actor with ActorLogging {
  override def receive: Receive = {
    case Initialize(game, role) => sender ! Initialized(game.roles.filter(_ != role).head)
    case Play(game, state, role, metadata) =>
      val otherRole = metadata.asInstanceOf[String]
      val chosenAction = bestMove(state, role, otherRole)
      log.info(s"Chosen action: $chosenAction")

      sender ! SelectedMove(chosenAction)
  }

  private def bestMove(state: GameState, role: String, otherRole: String) = {
    val legalActions = state.legalActions(role)
    if (legalActions.size == 1) legalActions.head else {
      val initialAction: Option[Action] = None
      val (_, bestAction) = legalActions.foldLeft((-1, initialAction))(tryNextMinScore(state, role, otherRole))
      bestAction.get
    }
  }

  private def minScore(state: GameState, role: String, otherRole: String)(action: Action): Int = {
    val otherActions = state.legalActions(otherRole)
    val states: Set[GameState] = otherActions.map { otherAction =>
      val actions = Map(role -> action, otherRole -> otherAction)
      state.update(actions)
    }

    val initialState: Option[GameState] = None
    val (worstScore, _) = states.foldLeft((101, initialState))(tryNextMaxScore(role, otherRole))
    worstScore
  }

  private def maxScore(role: String, otherRole: String)(state: GameState): Int = {
    if (state.isTerminal) state.value(role) else {
      val initialAction: Option[Action] = None
      val (bestScore, _) = state.legalActions(role).foldLeft((-1, initialAction))(tryNextMinScore(state, role, otherRole))
      bestScore
    }
  }

  private def tryNextMinScore(state: GameState, role: String, otherRole: String)(bestSoFar: (Int, Option[Action]), action: Action) = {
    val (bestScoreSoFar, _) = bestSoFar
    if (bestScoreSoFar == 100) bestSoFar
    else {
      val score = minScore(state, role, otherRole)(action)
      if (score > bestScoreSoFar) (score, Some(action)) else bestSoFar
    }
  }

  private def tryNextMaxScore(role: String, otherRole: String)(worstSoFar: (Int, Option[GameState]), state: GameState): (Int, Option[GameState]) = {
    val (worstScoreSoFar, _) = worstSoFar
    if (worstScoreSoFar == 0) worstSoFar
    else {
      val score = maxScore(role, otherRole)(state)
      if (score < worstScoreSoFar) (score, Some(state)) else worstSoFar
    }
  }
}
