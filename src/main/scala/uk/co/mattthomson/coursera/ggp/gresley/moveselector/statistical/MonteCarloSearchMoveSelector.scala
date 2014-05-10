package uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical

import akka.actor._
import org.joda.time.DateTime
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical.MonteCarloSearchMoveSelector.{Explore, SelectResult, StopExploring, ExplorationResult}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Action
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Play
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialized
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialize
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.SelectedMove

class MonteCarloSearchMoveSelector extends Actor with ActorLogging {
  private val numExplorers = 3

  override def receive: Receive = {
    case Initialize(game, role, _) => sender ! Initialized(())

    case Play(_, state, role, endTime, _) =>
      import context.dispatcher

      val timeLeft = (endTime.getMillis - DateTime.now.getMillis).milliseconds
      context.system.scheduler.scheduleOnce(timeLeft - 2.seconds, self, StopExploring)
      context.system.scheduler.scheduleOnce(timeLeft - 1.seconds, self, SelectResult)

      val legalActions = state.legalActions(role)
      val explorers = (1 to numExplorers).map(_ => context.actorOf(Props(new MonteCarloTreeExplorer(state, role, endTime))))
      val actions = explorers.foldLeft(legalActions)(sendAction(legalActions))

      val results = legalActions.map { action => (action, MonteCarloCount()) }.toMap

      context.become(awaitResults(sender, explorers, actions, legalActions, results, running = true))
  }

  private def awaitResults(source: ActorRef,
                           explorers: Seq[ActorRef],
                           actions: List[Action],
                           legalActions: List[Action],
                           results: Map[Action, MonteCarloCount],
                           running: Boolean): Receive = {
    case ExplorationResult(action, value) =>
      results.get(action) match {
        case Some(count) =>
          val updatedCount = count.update(value)
          val updatedResults = results + (action -> updatedCount)
          val updatedActions = if (running) sendAction(legalActions)(actions, sender) else actions

          context.become(awaitResults(source, explorers, updatedActions, legalActions, updatedResults, running))

        case None =>
      }

    case StopExploring =>
      context.become(awaitResults(source, explorers, actions, legalActions, results, running = false))
    case SelectResult =>
      explorers.foreach(_ ! PoisonPill)
      log.info("Monte Carlo values:\n" + results.map { case (action, count) => s"$action -> $count" }.mkString("\n"))

      val (bestAction, _) = results.maxBy { case (_, count) => count.value }

      log.info(s"Chosen action: $bestAction")
      source ! SelectedMove(bestAction)

      context.become(receive)
  }

  private def sendAction(legalActions: List[Action])(actions: List[Action], explorer: ActorRef) = {
    val updatedActions = actions match {
      case Nil => legalActions
      case _ => actions
    }

    explorer ! Explore(updatedActions.head)
    updatedActions.tail
  }
}

object MonteCarloSearchMoveSelector {
  case class Explore(nextMove: Action)

  case class ExplorationResult(action: Action, value: Int)

  case object StopExploring

  case object SelectResult
}
