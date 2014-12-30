package lexical

class NFAVisualizer(nfa: TokenizedNFA) {

  def exportAsImage(imgPath: String) = {

    val state2ID = nfa.states.zipWithIndex.toMap

    case class State(state: NFAState[CharCategory]) extends graphvisualize.State {
      override def shape = if (nfa.acceptsAttr.exists(_._1 == state)) graphvisualize.Shape.DoubleCircle else super.shape

      def label: String = state2ID(state).toString + nfa.acceptsAttr.find(_._1 == state).map(p => s" (${p._2.token.take(16)})").getOrElse("")

      def transitions: Seq[(String, Any)] = state.transitions.map { trans =>
        (nfa.charTable.toPrettyString(trans.symbol), State(trans.target))
      }
    }

    State(nfa.start).exportAsImage(imgPath)
  }

}

class DFAVisualizer(dfa: TokenizedDFA) {

  def exportAsImage(imgPath: String) = {

    val state2ID = dfa.states.zipWithIndex.toMap

    case class State(state: DFAState[CharCategory]) extends graphvisualize.State {
      override def shape = if (dfa.acceptsAttr.exists(_._1 == state)) graphvisualize.Shape.DoubleCircle else super.shape

      def label: String = state2ID(state).toString + dfa.acceptsAttr.find(_._1 == state).map(p => s" (${p._2.token.take(16)})").getOrElse("")

      def transitions: Seq[(String, Any)] = state.transitions.filter(_.target != dfa.dead).map { trans =>
        (dfa.charTable.toPrettyString(trans.symbol), State(trans.target))
      }
    }

    State(dfa.start).exportAsImage(imgPath)
  }

}