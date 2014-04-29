package uk.co.mattthomson.coursera.ggp.gresley.moveselector.heuristic

import akka.actor.{Props, ActorRef, Actor, ActorLogging}
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.TimeoutChecker
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.{SelectedMove, Play, Initialized, Initialize}

class IterativeDeepeningMoveSelector extends Actor with ActorLogging with TimeoutChecker {
  override def receive: Receive = {
    case Initialize(game, role) => sender ! Initialized(game.roles.filter(_ != role))
    case p: Play =>
      val depthLimit: Int = 1
      tryDepth(depthLimit, p, sender)
  }

  private def awaitResult(maxDepth: Int, play: Play, source: ActorRef): Receive = {
    case s: SelectedMove =>
      checkStillRunning(play.endTime)
      source ! s
      tryDepth(maxDepth + 1, play, source)
  }

  private def tryDepth(depthLimit: Int, play: Play, source: ActorRef) {
    val searcher = context.actorOf(Props(new GoalProximityHeuristicSearchMoveSelector(depthLimit)))
    searcher ! play
    context.become(awaitResult(depthLimit, play, source))
  }
}
