package lexical

class NFAVisualizer(nfa : TokenizedNFA) {

  def getStateTransitions(state : INFAState[CharCategory]) : List[INFATransition[CharCategory]] = state.transitions

  def exportAsImage(imgPath : String) = {

    val state2ID = nfa.states.zipWithIndex.toMap

    case class State(state : INFAState[CharCategory]) extends graphvisualize.IState {
      override def shape = if (nfa.acceptsAttr.exists(_._1 == state)) graphvisualize.Shape.DoubleCircle else super.shape

      def label : String = state2ID(state).toString + nfa.acceptsAttr.find(_._1 == state).map(p => s" (${p._2.toString.take(16)})").getOrElse("")

      def transitions : Seq[(String, Any)] = getStateTransitions(state).map { trans =>
        (nfa.charTable.toPrettyString(trans.symbol), State(trans.target))
      }
    }

    State(nfa.start).exportAsImage(imgPath)
  }

}

final class DFAVisualizer(dfa : TokenizedDFA) extends NFAVisualizer(dfa) {

  override def getStateTransitions(state : INFAState[CharCategory]) : List[INFATransition[CharCategory]] = {
    state.asInstanceOf[IDFAState[CharCategory]].transitions.filter(_.target != dfa.dead)
  }

}