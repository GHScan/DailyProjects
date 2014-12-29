package lexical

import scala.collection.immutable
import scala.collection.mutable
import RegexAST._

object DFAConverter {

  // Kleene construction
  def toRegex2(dfa : TokenizedDFA) : Tree = {
    type State = DFAState[CharCategory]

    assert(dfa.acceptsAttr.map(_._2).distinct.length == 1)

    val states = dfa.states.diff(List(dfa.dead))
    val charMap = dfa.charMap
    var pathMapPair = (mutable.Map[(State, State), Tree](), mutable.Map[(State, State), Tree]())
    for (
      s1 <- states;
      s2 <- states
    ) {
      val path = s1.transitions.filter(_.target == s2).foldLeft[Tree](if (s1 == s2) Provider.empty else null) {
        case (path, t) =>
          val charMap(chars) = t.symbol
          Provider.alternation(Chars(chars), path)
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
        val path = Provider.concatenation(prevMap(s1, s0),
          Provider.concatenation(
            Provider.kleeneStar(prevMap(s0, s0)),
            prevMap(s0, s2)))
        curMap((s1, s2)) = Provider.alternation(path, prevMap(s1, s2))
      }

      pathMapPair = pathMapPair.swap
    }

    dfa.accepts.foldLeft[Tree](null) {
      case (path, s) => Provider.alternation(path, pathMapPair._1((dfa.start, s)))
    }
  }

  // States reducing
  def toRegex(dfa : TokenizedDFA) : Tree = {
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
        val path = Provider.concatenation(lookup(edgeMap, predecessor, state),
          Provider.concatenation(
            Provider.kleeneStar(lookup(edgeMap, state, state)),
            lookup(edgeMap, state, successor)))
        newMap = newMap.updated((predecessor, successor), Provider.alternation(path, lookup(edgeMap, predecessor, successor)))
      }
      newMap
    }

    def iterate(start : State, accepts : List[State], edgeMap : EdgeMap) : List[Tree] = {
      if (accepts.isEmpty) return Nil

      val state = accepts.head
      val privateEdgeMap = accepts.tail.foldLeft[EdgeMap](edgeMap)(remove)
      val path = (if (start == state) {
        Provider.kleeneStar(lookup(privateEdgeMap, start, state))
      } else {
        Provider.concatenation(
          Provider.kleeneStar(Provider.alternation(lookup(privateEdgeMap, start, start),
            Provider.concatenation(lookup(privateEdgeMap, start, state),
              Provider.concatenation(
                Provider.kleeneStar(lookup(privateEdgeMap, state, state)),
                lookup(privateEdgeMap, state, start))))),
          Provider.concatenation(lookup(privateEdgeMap, start, state),
            Provider.kleeneStar(lookup(privateEdgeMap, state, state))))
      })

      path :: iterate(start, accepts.tail, remove(edgeMap, state))
    }

    val states = dfa.states.diff(List(dfa.dead))
    val charMap = dfa.charMap
    val edgeMap : EdgeMap = (for (s1 <- states; s2 <- states) yield ((s1, s2), s1.transitions.filter(_.target == s2).foldLeft[Tree](null) {
      case (path, t) =>
        val charMap(chars) = t.symbol
        Provider.alternation(Chars(chars), path)
    })).filter(_._2 != null).toMap
    val edgeMapContainsStartAccepts = states.diff(dfa.start :: dfa.accepts).foldLeft(edgeMap)(remove)

    iterate(dfa.start, dfa.accepts, edgeMapContainsStartAccepts).foldRight[Tree](null)(Provider.alternation)
  }

  def toPattern(dfa : TokenizedDFA) : String = {
    RegexAST.toPattern(toRegex(dfa))
  }

  def toPattern2(dfa : TokenizedDFA) : String = {
    RegexAST.toPattern(toRegex2(dfa))
  }
}