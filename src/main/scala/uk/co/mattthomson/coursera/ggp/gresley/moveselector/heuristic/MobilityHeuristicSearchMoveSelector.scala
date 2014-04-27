package uk.co.mattthomson.coursera.ggp.gresley.moveselector.heuristic

import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameState

class MobilityHeuristicSearchMoveSelector(depthLimit: Int) extends HeuristicSearchMoveSelector(depthLimit) {
  override protected def intermediateStateValue(state: GameState, role: String): Int = state.legalActions(role).size
}
