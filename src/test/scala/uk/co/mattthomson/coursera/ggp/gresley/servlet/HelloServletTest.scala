package uk.co.mattthomson.coursera.ggp.gresley.servlet

import org.scalatra.test.scalatest.ScalatraFlatSpec
import org.scalatest.matchers.ShouldMatchers

class HelloServletTest extends ScalatraFlatSpec with ShouldMatchers {
  addServlet(classOf[HelloServlet], "/")

  "The servlet" should "say hello" in {
    get("/") {
      status should equal (200)
      body should include ("Hello")
    }
  }
}
