package uk.co.mattthomson.coursera.ggp.gresley.player.multiple

import uk.co.mattthomson.coursera.ggp.gresley.player.Player
import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Action, GameState, GameDescription}
import akka.actor.ActorRef

class AlphaBetaPlayer extends Player[String] {
  override def initialize(game: GameDescription, role: String) = game.roles.filter(_ != role).head

  override def play(state: GameState, role: String, source: ActorRef, otherRole: String) = {
    val chosenAction = bestMove(state, role, otherRole)
    log.info(s"Chosen action: $chosenAction")

    source ! chosenAction
    otherRole
  }

  private def bestMove(state: GameState, role: String, otherRole: String) = {
    val legalActions = state.legalActions(role)
    if (legalActions.size == 1) legalActions.head else {
      val initialAction: Option[Action] = None
      val (_, bestAction) = legalActions.foldLeft((-1, initialAction))(tryNextMinScore(state, role, otherRole, 101))
      bestAction.get
    }
  }

  private def minScore(state: GameState, role: String, otherRole: String, alpha: Int, beta: Int)(action: Action): Int = {
    state.legalActions(otherRole).foldLeft(beta)(tryNextMaxScore(state, action, role, otherRole, alpha))
  }

  private def maxScore(role: String, otherRole: String)(state: GameState, alpha: Int, beta: Int): Int = {
    if (state.isTerminal) state.value(role) else {
      val initialAction: Option[Action] = None
      val (bestScore, _) = state.legalActions(role).foldLeft((alpha, initialAction))(tryNextMinScore(state, role, otherRole, beta))
      bestScore
    }
  }

  private def tryNextMinScore(state: GameState, role: String, otherRole: String, beta: Int)(bestSoFar: (Int, Option[Action]), action: Action) = {
    val (alpha, _) = bestSoFar
    if (alpha == 100) bestSoFar
    else if (alpha >= beta) (beta, None)
    else {
      val score = minScore(state, role, otherRole, alpha, beta)(action)
      if (score > alpha) (score, Some(action)) else bestSoFar
    }
  }

  private def tryNextMaxScore(state: GameState, action: Action, role: String, otherRole: String, alpha: Int)(beta: Int, otherAction: Action): Int = {
    if (beta == 0) beta
    else if (beta <= alpha) alpha
    else {
      val actions = Map(role -> action, otherRole -> otherAction)
      val newState = state.update(actions)
      val score = maxScore(role, otherRole)(newState, alpha, beta)
      if (score < beta) score else beta
    }
  }
}
