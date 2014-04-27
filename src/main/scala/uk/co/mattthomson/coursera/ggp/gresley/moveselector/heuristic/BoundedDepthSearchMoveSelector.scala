package uk.co.mattthomson.coursera.ggp.gresley.moveselector.heuristic

import uk.co.mattthomson.coursera.ggp.gresley.gdl.GameState

class BoundedDepthSearchMoveSelector(depthLimit: Int) extends HeuristicSearchMoveSelector(depthLimit) {
  override protected def intermediateStateValue(state: GameState, role: String) = state.value(role)
}
