package lexical

import scala.collection.immutable
import scala.collection.mutable
import RegexAST._

object DFA2Regex {

  implicit class DFA2RegexExtension(dfa : TokenizedDFA) {

    // Kleene construction
    def toRegex2 : Tree = {
      type State = DFAState[CharCategory]

      assert(dfa.acceptsAttr.map(_._2).distinct.length == 1)

      val states = dfa.states.diff(List(dfa.dead))
      val charTable = dfa.charTable
      var pathMapPair = (mutable.Map[(State, State), Tree](), mutable.Map[(State, State), Tree]())
      for (
        s1 <- states;
        s2 <- states
      ) {
        val path = s1.transitions.filter(_.target == s2).foldLeft[Tree](if (s1 == s2) Empty else null) { (path, t) =>
          Chars(charTable.unapply(t.symbol).get) | path
        }
        pathMapPair._1((s1, s2)) = path
      }

      for (s0 <- states) {
        val prevMap = pathMapPair._1
        val curMap = pathMapPair._2

        for (
          s1 <- states;
          s2 <- states
        ) {
          curMap((s1, s2)) = prevMap(s1, s0) & (prevMap(s0, s0).kleeneStar & prevMap(s0, s2)) | prevMap(s1, s2)
        }

        pathMapPair = pathMapPair.swap
      }

      dfa.accepts.foldLeft[Tree](null) { (path, s) => path | pathMapPair._1((dfa.start, s))}
    }

    // States reducing
    def toRegex : Tree = {
      type State = DFAState[CharCategory]
      type EdgeMap = immutable.Map[(State, State), Tree]

      assert(dfa.acceptsAttr.map(_._2).distinct.length == 1)

      def lookup(edgeMap : EdgeMap, s1 : State, s2 : State) : Tree = {
        edgeMap.getOrElse((s1, s2), null)
      }

      def remove(edgeMap : EdgeMap, state : State) : EdgeMap = {
        var newMap = edgeMap.filter(kv => kv._1._1 != state && kv._1._2 != state)
        for (
          predecessor <- edgeMap.iterator.filter(_._1._2 == state).map(_._1._1);
          successor <- edgeMap.iterator.filter(_._1._1 == state).map(_._1._2)
        ) {
          val path = lookup(edgeMap, predecessor, state) & (lookup(edgeMap, state, state).kleeneStar & lookup(edgeMap, state, successor))
          newMap = newMap.updated((predecessor, successor), path | lookup(edgeMap, predecessor, successor))
        }
        newMap
      }

      def iterate(start : State, accepts : List[State], edgeMap : EdgeMap) : List[Tree] = {
        if (accepts.isEmpty) return Nil

        val state = accepts.head
        val privateEdgeMap = accepts.tail.foldLeft[EdgeMap](edgeMap)(remove)
        val path = if (start == state) {
          lookup(privateEdgeMap, start, state).kleeneStar
        } else {
          (lookup(privateEdgeMap, start, start) |
            (lookup(privateEdgeMap, start, state) &
              (lookup(privateEdgeMap, state, state).kleeneStar & lookup(privateEdgeMap, state, start)))).kleeneStar &
            (lookup(privateEdgeMap, start, state) &
              lookup(privateEdgeMap, state, state).kleeneStar)
        }

        path :: iterate(start, accepts.tail, remove(edgeMap, state))
      }

      val states = dfa.states.diff(List(dfa.dead))
      val charTable = dfa.charTable
      val edgeMap : EdgeMap = (for (s1 <- states; s2 <- states) yield ((s1, s2), s1.transitions.filter(_.target == s2).foldLeft[Tree](null) {
        (path, t) => Chars(charTable.unapply(t.symbol).get) | path
      })).filter(_._2 != null).toMap
      val edgeMapContainsStartAccepts = states.diff(dfa.start :: dfa.accepts).foldLeft(edgeMap)(remove)

      iterate(dfa.start, dfa.accepts, edgeMapContainsStartAccepts).foldRight[Tree](null)(_ | _)
    }

    def toRegexPattern2 : String = this.toRegex2.toPattern

    def toRegexPattern : String = this.toRegex.toPattern
  }

}