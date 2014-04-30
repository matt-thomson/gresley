package uk.co.mattthomson.coursera.ggp.gresley.moveselector.heuristic

import uk.co.mattthomson.coursera.ggp.gresley.gdl.{Action, GameState}
import akka.actor.{Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{SelectedMove, Play, Initialized, Initialize}
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.TimeoutChecker
import org.joda.time.DateTime

abstract class HeuristicSearchMoveSelector(depthLimit: Int) extends Actor with ActorLogging with TimeoutChecker {
  override def receive: Receive = {
    case Initialize(game, role, _) => sender ! Initialized(game.roles.filter(_ != role))
    case Play(_, state, role, endTime, metadata) =>
      val otherRoles = metadata.asInstanceOf[Seq[String]]
      val chosenAction = bestMove(state, role, otherRoles, endTime)
      log.info(s"Chosen action: $chosenAction")

      sender ! SelectedMove(chosenAction)
  }

  private def bestMove(state: GameState, role: String, otherRoles: Seq[String], endTime: DateTime) = {
    checkStillRunning(endTime)
    val legalActions = state.legalActions(role)
    if (legalActions.size == 1) legalActions.head else {
      val initialAction: Option[Action] = None
      val (_, bestAction) = legalActions.foldLeft((-1, initialAction))(tryNextMinScore(state, role, otherRoles, 101, 1, endTime))
      bestAction.get
    }
  }

  private def minScore(state: GameState, role: String, otherRoles: Seq[String], alpha: Int, beta: Int, level: Int, endTime: DateTime)(action: Action): Int = {
    checkStillRunning(endTime)

    val otherActions: Seq[Map[String, Action]] = otherRoles.foldLeft(Seq(Map[String, Action]()))(addAction(state))

    otherActions.foldLeft(beta)(tryNextMaxScore(state, action, role, otherRoles, alpha, level + 1, endTime))
  }

  private def maxScore(role: String, otherRoles: Seq[String], level: Int, endTime: DateTime)(state: GameState, alpha: Int, beta: Int): Int = {
    checkStillRunning(endTime)

    if (state.isTerminal) state.value(role)
    else if (level > depthLimit) intermediateStateValue(state, role)
    else {
      val initialAction: Option[Action] = None
      val (bestScore, _) = state.legalActions(role).foldLeft((alpha, initialAction))(tryNextMinScore(state, role, otherRoles, beta, level, endTime))
      bestScore
    }
  }

  private def tryNextMinScore(state: GameState, role: String, otherRoles: Seq[String], beta: Int, level: Int, endTime: DateTime)(bestSoFar: (Int, Option[Action]), action: Action) = {
    checkStillRunning(endTime)

    val (alpha, _) = bestSoFar
    if (alpha == 100) bestSoFar
    else if (alpha >= beta) (beta, None)
    else {
      val score = minScore(state, role, otherRoles, alpha, beta, level, endTime)(action)
      if (score > alpha) (score, Some(action)) else bestSoFar
    }
  }

  private def tryNextMaxScore(state: GameState, action: Action, role: String, otherRoles: Seq[String], alpha: Int, level: Int, endTime: DateTime)(beta: Int, otherActions: Map[String, Action]): Int = {
    if (beta == 0) beta
    else if (beta <= alpha) alpha
    else {
      val actions = Map(role -> action) ++ otherActions
      val newState = state.update(actions)
      val score = maxScore(role, otherRoles, level, endTime)(newState, alpha, beta)
      if (score < beta) score else beta
    }
  }

  private def addAction(state: GameState)(soFar: Seq[Map[String, Action]], role: String): Seq[Map[String, Action]] = {
    soFar.flatMap(actions => state.legalActions(role).map { action => actions + (role -> action) })
  }

  protected def intermediateStateValue(state: GameState, role: String): Int
}
