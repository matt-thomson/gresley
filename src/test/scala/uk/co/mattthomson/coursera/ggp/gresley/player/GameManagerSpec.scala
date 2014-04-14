package uk.co.mattthomson.coursera.ggp.gresley.player

import org.scalatest.{BeforeAndAfter, FlatSpec}
import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.{Actor, Props, ActorSystem}
import akka.pattern.ask
import uk.co.mattthomson.coursera.ggp.gresley.protocol._
import scala.concurrent.duration._
import uk.co.mattthomson.coursera.ggp.gresley.gdl._
import uk.co.mattthomson.coursera.ggp.gresley.player.GameManager.GamesInProgress
import akka.util.Timeout
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Action
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Stop
import uk.co.mattthomson.coursera.ggp.gresley.gdl.Start

class GameManagerSpec extends TestKit(ActorSystem("TestActorSystem")) with FlatSpec with ImplicitSender with BeforeAndAfter {
  val manager = system.actorOf(Props(new Actor {
    val child = context.actorOf(Props(new GameManager(Props[DummyPlayer])), "child")
    def receive = {
      case x if sender == child => testActor forward x
      case x => child forward x
    }
  }))

  after {
    import system.dispatcher
    implicit val timeout = Timeout(1)

    val games = manager ? GamesInProgress
    games.foreach(g => g.asInstanceOf[List[String]].foreach(abortGame))
  }

  "The manager" should "respond to an info message" in {
    manager ! Info
    expectMsg("ready")
  }

  it should "respond to a start message" in {
    val id = startGame

    manager ! GamesInProgress
    expectMsg(List(id))
  }

  it should "respond to a play message" in {
    val id = startGame

    manager ! Play(id, None)
    expectMsg(Action("left"))
  }

  it should "respond to a stop message" in {
    val id = startGame

    manager ! Stop(id, List("left", "right"))
    expectMsg("done")

    manager ! GamesInProgress
    expectMsg(Nil)
  }

  it should "respond to an abort message" in {
    val id = startGame

    abortGame(id)

    manager ! GamesInProgress
    expectMsg(Nil)
  }

  private def startGame: String = {
    val game = GameDescription("(role black)")
    val id = s"id-${System.nanoTime()}"
    manager ! Start(id, "black", game, 1.second, 2.seconds)
    expectMsg("ready")

    id
  }

  private def abortGame(id: String) {
    manager ! Abort(id)
    expectMsg("done")
  }
}

class DummyPlayer extends Actor {
  def receive = {
    case g: GameDescription =>
    case _ => sender ! Action("left")
  }
}
