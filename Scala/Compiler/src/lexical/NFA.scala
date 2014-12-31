package lexical

import scala.collection.mutable

trait NFATransition[T] extends FATransition[T] {
  def target : NFAState[T]
}

class SimpleNFATransition[T](val symbol : T, val target : NFAState[T]) extends NFATransition[T]

trait NFAState[T] extends FAState[T] {
  def transitions : List[NFATransition[T]]
}

class SimpleNFAState[T](var transitions : List[NFATransition[T]]) extends NFAState[T]

trait NFA[T, U] extends FA[T] {
  outer =>

  def start : NFAState[T]

  override def states : List[NFAState[T]] = super.states.asInstanceOf[List[NFAState[T]]]

  def accepts : List[NFAState[T]]

  def acceptsAttr : List[(NFAState[T], U)]

  def transitionsMapped[T2](f : T => Iterable[T2]) : NFA[T2, U] = {
    val state2NewState = states.map(s => (s, new SimpleNFAState[T2](Nil))).toMap
    for ((s, ns) <- state2NewState) {
      ns.transitions = s.transitions.flatMap(t => f(t.symbol).map(sym => new SimpleNFATransition[T2](sym, state2NewState(t.target))))
    }
    new SimpleNFA[T2, U](
      state2NewState(outer.start),
      outer.accepts.map(state2NewState),
      outer.acceptsAttr.map(p => (state2NewState(p._1), p._2))
    )
  }
}

class SimpleNFA[T, U](val start : NFAState[T], val accepts : List[NFAState[T]], val acceptsAttr : List[(NFAState[T], U)]) extends NFA[T, U]


case class TokenizedAcceptStateAttr(priority : Int, id : String)

class TokenizedNFA(
  val charTable : CharClassifyTable,
  val start : NFAState[CharCategory],
  val accepts : List[NFAState[CharCategory]],
  val acceptsAttr : List[(NFAState[CharCategory], TokenizedAcceptStateAttr)]) extends NFA[CharCategory, TokenizedAcceptStateAttr] {
  outer =>

  def toEmulator : TokenizedNFAEmulator = {
    val state2ID = states.zipWithIndex.toMap
    new TokenizedNFAEmulator(
      charTable,
      state2ID(start),
      state2ID.toList.sortBy(_._2).map { case ((state, _)) => acceptsAttr.find(_._1 == state).map(_._2)}.toArray,
      state2ID.toList.sortBy(_._2).map {
        case ((state, _)) => state.transitions.map { t => (t.symbol, state2ID(t.target))}
      }.toArray)
  }

  final private def toCharsNFA : NFA[Seq[Char], TokenizedAcceptStateAttr] = {
    transitionsMapped[Seq[Char]] { category =>
      List(if (category == CharCategory.Empty) Nil else charTable.rlookup(category))
    }
  }

  def |(other : TokenizedNFA) : TokenizedNFA = {
    val nfa1 = toCharsNFA
    val nfa2 = other.toCharsNFA
    val newStart = new SimpleNFAState[Seq[Char]](List(
      new SimpleNFATransition[Seq[Char]](Nil, nfa1.start),
      new SimpleNFATransition[Seq[Char]](Nil, nfa2.start)))
    TokenizedNFA.fromCharsNFA(new SimpleNFA(newStart, nfa1.accepts ++ nfa2.accepts, nfa1.acceptsAttr ++ nfa2.acceptsAttr))
  }

  def reversed : TokenizedNFA = {
    val attr = acceptsAttr.map(_._2).distinct.ensuring(_.length == 1).head

    val state2New = states.map(s => (s, new SimpleNFAState[CharCategory](Nil))).toMap
    for (s <- states; t <- s.transitions) {
      val newTarget = state2New(t.target)
      newTarget.transitions = new SimpleNFATransition[CharCategory](t.symbol, state2New(s)) :: newTarget.transitions
    }
    val newStart = new SimpleNFAState[CharCategory](accepts.map(s => new SimpleNFATransition[CharCategory](CharCategory.Empty, state2New(s))))

    new TokenizedNFA(charTable, newStart, List(state2New(start)), List((state2New(start), attr)))
  }

  def reachable : TokenizedNFA = {
    new TokenizedNFA(charTable, start, accepts.filter(states.contains(_)), acceptsAttr.filter(p => states.contains(p._1)))
  }

  def subset : TokenizedDFA = toEmulator.toDFAEmulator.toDFA
}

object TokenizedNFA {

  private def fromCharsNFA(nfa : NFA[Seq[Char], TokenizedAcceptStateAttr]) : TokenizedNFA = {
    val builder = new CharClassifyTableBuilder()
    for (s <- nfa.states; t <- s.transitions) {
      builder.addChars(t.symbol)
    }
    val newCharTable = builder.result
    val newNFA = nfa.transitionsMapped[CharCategory](chars => if (chars.isEmpty) List(CharCategory.Empty) else chars.groupBy(newCharTable).map(_._1))
    new TokenizedNFA(newCharTable, newNFA.start, newNFA.accepts, newNFA.acceptsAttr)
  }

  def fromPattern(pattern : String, priority : Int = 0, id : String = null) : TokenizedNFA = {
    import lexical.RegexAST._

    type State = SimpleNFAState[Seq[Char]]
    type Transition = SimpleNFATransition[Seq[Char]]

    def iterate(tree : Tree) : (State, State) = tree match {
      case Empty =>
        val accept = new State(Nil)
        (new State(List(new Transition(Nil, accept))), accept)
      case Chars(chars : Seq[_]) if chars.head.isInstanceOf[Char] =>
        val accept = new State(Nil)
        (new State(List(new Transition(chars.asInstanceOf[Seq[Char]], accept))), accept)
      case KleeneStar(content) =>
        val (start, accept) = iterate(content)
        val accept2 = new State(Nil)
        accept.transitions = new Transition(Nil, start) :: accept.transitions
        accept.transitions = new Transition(Nil, accept2) :: accept.transitions
        (new State(List(new Transition(Nil, start), new Transition(Nil, accept2))), accept2)
      case Concatenation(first, second) =>
        val (start1, accept1) = iterate(first)
        val (start2, accept2) = iterate(second)
        accept1.transitions = new Transition(Nil, start2) :: accept1.transitions
        (start1, accept2)
      case Alternation(first, second) =>
        val (start1, accept1) = iterate(first)
        val (start2, accept2) = iterate(second)
        val start = new State(List(start1, start2).map(new Transition(Nil, _)))
        val accept = new State(Nil)
        accept1.transitions = new Transition(Nil, accept) :: accept1.transitions
        accept2.transitions = new Transition(Nil, accept) :: accept2.transitions
        (start, accept)
    }

    val (start, accept) = iterate(new RegexParser().parse(pattern))
    fromCharsNFA(new SimpleNFA[Seq[Char], TokenizedAcceptStateAttr](
      start,
      List(accept),
      List((accept, TokenizedAcceptStateAttr(priority, if (id == null) pattern else id)))))
  }

}

final class TokenizedNFAEmulator(
  val charTable : CharClassifyTable,
  val start : Int,
  val acceptsAttr : Array[Option[TokenizedAcceptStateAttr]],
  val transitions : Array[List[(CharCategory, Int)]]) {

  override def equals(other : Any) : Boolean = {
    other.isInstanceOf[TokenizedNFAEmulator] && other.asInstanceOf[TokenizedNFAEmulator].equals(this)
  }

  def equals(other : TokenizedNFAEmulator) : Boolean = {
    (charTable == other.charTable
      && start == other.start
      && acceptsAttr.view == other.acceptsAttr.view
      && transitions.view == other.transitions.view)
  }

  private lazy val statesClosure : Array[mutable.BitSet] = {

    def traceClosure(state : Int) : mutable.BitSet = {
      val result = mutable.BitSet()

      def iterate(state : Int) {
        if (result(state)) return
        result += state

        for (
          trans <- transitions(state)
          if CharCategory.Empty == trans._1
        ) {
          iterate(trans._2)
        }
      }

      iterate(state)
      result
    }

    states.map(traceClosure).toArray
  }

  val acceptSet = mutable.BitSet(states.filter(acceptsAttr(_) != None) : _*)

  def states : Seq[Int] = 0 until transitions.length

  def closure(state : Int) : mutable.BitSet = statesClosure(state)

  def move(stateSet : mutable.BitSet, symbol : CharCategory) : mutable.BitSet = {
    val result = mutable.BitSet()

    for (
      state <- stateSet;
      trans <- transitions(state)
      if trans._1 == symbol
    ) {
      result |= closure(trans._2)
    }

    result
  }

  def toDFAEmulator : TokenizedDFAEmulator = TokenizedDFAEmulator.fromNFAEmulator(this)
}