package uk.co.mattthomson.coursera.ggp.gresley.servlet

import org.scalatra.test.scalatest.ScalatraFlatSpec
import org.scalatest.matchers.ShouldMatchers
import akka.testkit.{ImplicitSender, TestKit}
import akka.actor.ActorSystem

class GresleyServletSpec extends TestKit(ActorSystem("TestActorSystem")) with ScalatraFlatSpec with ShouldMatchers with ImplicitSender {
  addServlet(new GresleyServlet(system), "/")

  "The servlet" should "say hello" in {
    get("/") {
      status should equal (200)
      body should include ("Hello")
    }
  }

  it should "parse protocol messages and send to the player" in {
    post("/", body = "(info)") {
      status should equal (200)
      body should be ("ready")
    }
  }
}
