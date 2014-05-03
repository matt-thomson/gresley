package uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical

import akka.actor._
import akka.event.LoggingReceive
import org.joda.time.DateTime
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical.MonteCarloSearchMoveSelector.{StopExploring, Explore}
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Action
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Play
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialized
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.Initialize
import uk.co.mattthomson.coursera.ggp.gresley.player.Player.SelectedMove
import uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical.MonteCarloSearchMoveSelector.ExplorationResult

class MonteCarloSearchMoveSelector extends Actor with ActorLogging {
  override def receive: Receive = LoggingReceive {
    case Initialize(game, role, _) => sender ! Initialized(())

    case Play(_, state, role, endTime, _) =>
      import context.dispatcher

      val timeLeft = (endTime.getMillis - DateTime.now.getMillis).milliseconds
      context.system.scheduler.scheduleOnce(timeLeft - 1.second, self, StopExploring)

      val legalActions = state.legalActions(role)
      val explorers = legalActions.map { action =>
        val explorer = context.actorOf(Props(new MonteCarloTreeExplorer(state, role, action)))
        explorer ! Explore

        explorer
      }
      val results = legalActions.map { action => (action, (0, 0)) }.toMap

      context.become(awaitResults(sender, explorers, results))
  }

  private def awaitResults(source: ActorRef,
                           explorers: Set[ActorRef],
                           results: Map[Action, (Int, Int)]): Receive = LoggingReceive {
    case ExplorationResult(action, value) =>
      results.get(action) match {
        case Some((oldTotal, oldCount)) =>
          val (newTotal, newCount) = (oldTotal + value, oldCount + 1)
          val updatedResults = results + (action -> (newTotal, newCount))

          sender ! Explore
          context.become(awaitResults(source, explorers, updatedResults))

        case None =>
      }

    case StopExploring =>
      explorers.foreach { _ ! PoisonPill }

      val (bestAction, _) = results.maxBy { case (_, (total, count)) =>
        if (count == 0) 0 else total.toDouble / count.toDouble
      }

      log.info(s"Chosen action: $bestAction")
      source ! SelectedMove(bestAction)

      context.become(receive)
  }
}

object MonteCarloSearchMoveSelector {
  case object Explore

  case class ExplorationResult(action: Action, value: Int)

  case object StopExploring
}
