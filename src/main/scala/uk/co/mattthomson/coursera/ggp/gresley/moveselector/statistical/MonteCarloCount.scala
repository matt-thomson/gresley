package uk.co.mattthomson.coursera.ggp.gresley.moveselector.statistical

case class MonteCarloCount(total: Int, count: Int) {
  lazy val value = if (count == 0) 0 else total.toDouble / count.toDouble

  def update(score: Int) = MonteCarloCount(total + score, count + 1)

  override def toString = s"$value ($total / $count)"
}

object MonteCarloCount {
  def apply(): MonteCarloCount = MonteCarloCount(0, 0)
}
