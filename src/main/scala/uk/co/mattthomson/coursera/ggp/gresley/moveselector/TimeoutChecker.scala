package uk.co.mattthomson.coursera.ggp.gresley.moveselector

import org.joda.time.DateTime
import org.joda.time.DateTime._
import java.util.concurrent.TimeoutException

trait TimeoutChecker {
  protected def checkStillRunning(endTime: DateTime) = {
    if (now.isAfter(endTime)) throw new TimeoutException()
  }
}
