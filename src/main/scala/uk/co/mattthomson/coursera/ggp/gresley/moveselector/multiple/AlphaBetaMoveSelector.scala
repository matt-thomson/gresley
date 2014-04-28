package uk.co.mattthomson.coursera.ggp.gresley.moveselector.multiple

import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Action, GameState}
import akka.actor.{Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{SelectedMove, Play, Initialized, Initialize}

class AlphaBetaMoveSelector extends Actor with ActorLogging {
  override def receive: Receive = {
    case Initialize(game, role) => sender ! Initialized(game.roles.filter(_ != role))
    case Play(game, state, role, metadata) =>
      val otherRoles = metadata.asInstanceOf[Seq[String]]
      val chosenAction = bestMove(state, role, otherRoles)
      log.info(s"Chosen action: $chosenAction")

      sender ! SelectedMove(chosenAction)
  }

  private def bestMove(state: GameState, role: String, otherRoles: Seq[String]) = {
    val legalActions = state.legalActions(role)
    if (legalActions.size == 1) legalActions.head else {
      val initialAction: Option[Action] = None
      val (_, bestAction) = legalActions.foldLeft((-1, initialAction))(tryNextMinScore(state, role, otherRoles, 101))
      bestAction.get
    }
  }

  private def minScore(state: GameState, role: String, otherRoles: Seq[String], alpha: Int, beta: Int)(action: Action): Int = {
    val otherActions: Seq[Map[String, Action]] = otherRoles.foldLeft(Seq(Map[String, Action]()))(addAction(state))

    otherActions.foldLeft(beta)(tryNextMaxScore(state, action, role, otherRoles, alpha))
  }

  private def maxScore(role: String, otherRoles: Seq[String])(state: GameState, alpha: Int, beta: Int): Int = {
    if (state.isTerminal) state.value(role) else {
      val initialAction: Option[Action] = None
      val (bestScore, _) = state.legalActions(role).foldLeft((alpha, initialAction))(tryNextMinScore(state, role, otherRoles, beta))
      bestScore
    }
  }

  private def addAction(state: GameState)(soFar: Seq[Map[String, Action]], role: String): Seq[Map[String, Action]] = {
    soFar.flatMap(actions => state.legalActions(role).map { action => actions + (role -> action) })
  }

  private def tryNextMinScore(state: GameState, role: String, otherRoles: Seq[String], beta: Int)(bestSoFar: (Int, Option[Action]), action: Action) = {
    val (alpha, _) = bestSoFar
    if (alpha == 100) bestSoFar
    else if (alpha >= beta) (beta, None)
    else {
      val score = minScore(state, role, otherRoles, alpha, beta)(action)
      if (score > alpha) (score, Some(action)) else bestSoFar
    }
  }

  private def tryNextMaxScore(state: GameState, action: Action, role: String, otherRoles: Seq[String], alpha: Int)(beta: Int, otherActions: Map[String, Action]): Int = {
    if (beta == 0) beta
    else if (beta <= alpha) alpha
    else {
      val actions = Map(role -> action) ++ otherActions
      val newState = state.update(actions)
      val score = maxScore(role, otherRoles)(newState, alpha, beta)
      if (score < beta) score else beta
    }
  }
}
