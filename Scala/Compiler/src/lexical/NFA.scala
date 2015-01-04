package lexical

import scala.collection.mutable

trait IFAStateAttribute {
  def priority : Int
}

class FAStateAttribute(val priority : Int, val str : String) extends IFAStateAttribute {
  override def toString = str
}

object FAStateAttribute {
  val Default = new FAStateAttribute(0, "")
}

trait INFA[T] extends IFA[T] {
  outer =>

  def accepts : List[IFAState[T]]

  def acceptsAttr : List[(IFAState[T], IFAStateAttribute)]

  lazy val unaccepts = states.diff(accepts)

  def transitionsMapped[T2](f : T => Iterable[T2]) : INFA[T2] = {
    val state2NewState = states.map(s => (s, new FAState[T2](Nil))).toMap
    for ((s, ns) <- state2NewState) {
      ns.transitions = s.transitions.flatMap(t => f(t.symbol).map(sym => new FATransition[T2](sym, state2NewState(t.target))))
    }
    new NFA[T2](
      state2NewState(outer.start),
      outer.accepts.map(state2NewState),
      outer.acceptsAttr.map(p => (state2NewState(p._1), p._2))
    )
  }
}

class NFA[T](val start : IFAState[T], val accepts : List[IFAState[T]], val acceptsAttr : List[(IFAState[T], IFAStateAttribute)]) extends INFA[T]

class TokenizedNFA(
  val charTable : CharClassifyTable,
  val start : IFAState[CharCategory],
  val accepts : List[IFAState[CharCategory]],
  val acceptsAttr : List[(IFAState[CharCategory], IFAStateAttribute)]) extends INFA[CharCategory] {
  outer =>

  def toEmulator : TokenizedNFAEmulator = {
    val state2ID = states.zipWithIndex.toMap
    new TokenizedNFAEmulator(
      charTable,
      state2ID(start),
      state2ID.toList.sortBy(_._2).map { case ((state, _)) => acceptsAttr.find(_._1 == state).map(_._2).orNull}.toArray,
      state2ID.toList.sortBy(_._2).map {
        case ((state, _)) => state.transitions.map { t => (t.symbol, state2ID(t.target))}
      }.toArray)
  }

  def toDFAEmulator = toEmulator.toDFAEmulator

  final private def toCharsNFA : INFA[Seq[Char]] = {
    transitionsMapped[Seq[Char]] { category =>
      List(if (category == CharCategory.Empty) Nil else charTable.rlookup(category))
    }
  }

  def |(other : TokenizedNFA) : TokenizedNFA = {
    val nfa1 = toCharsNFA
    val nfa2 = other.toCharsNFA
    val newStart = new FAState[Seq[Char]](List(
      new FATransition[Seq[Char]](Nil, nfa1.start),
      new FATransition[Seq[Char]](Nil, nfa2.start)))
    TokenizedNFA.fromCharsNFA(new NFA(newStart, nfa1.accepts ++ nfa2.accepts, nfa1.acceptsAttr ++ nfa2.acceptsAttr))
  }

  def reversed : TokenizedNFA = {
    val attr = acceptsAttr.map(_._2).distinct.ensuring(_.length == 1).head

    val state2New = states.map(s => (s, new FAState[CharCategory](Nil))).toMap
    for (s <- states; t <- s.transitions) {
      val newTarget = state2New(t.target)
      newTarget.transitions = new FATransition[CharCategory](t.symbol, state2New(s)) :: newTarget.transitions
    }
    val newStart = new FAState[CharCategory](accepts.map(s => new FATransition[CharCategory](CharCategory.Empty, state2New(s))))

    new TokenizedNFA(charTable, newStart, List(state2New(start)), List((state2New(start), attr)))
  }

  def reachable : TokenizedNFA = {
    new TokenizedNFA(charTable, start, accepts.filter(states.contains(_)), acceptsAttr.filter(p => states.contains(p._1)))
  }

  def subset : TokenizedDFA = toDFAEmulator.toDFA
}

object TokenizedNFA {

  private def fromCharsNFA(nfa : INFA[Seq[Char]]) : TokenizedNFA = {
    val builder = new CharClassifyTableBuilder()
    for (s <- nfa.states; t <- s.transitions) {
      builder.addChars(t.symbol)
    }
    val newCharTable = builder.result
    val newNFA = nfa.transitionsMapped[CharCategory](chars => if (chars.isEmpty) List(CharCategory.Empty) else chars.groupBy(newCharTable).map(_._1))
    new TokenizedNFA(newCharTable, newNFA.start, newNFA.accepts, newNFA.acceptsAttr)
  }

  // Thompson construction
  def fromPattern(pattern : String, attr : IFAStateAttribute = null) : TokenizedNFA = {
    import lexical.RegexAST._

    type State = FAState[Seq[Char]]
    type Transition = FATransition[Seq[Char]]

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
    fromCharsNFA(new NFA[Seq[Char]](start, List(accept), List((accept, if (attr != null) attr else new FAStateAttribute(0, pattern)))))
  }

}

final class TokenizedNFAEmulator(
  val charTable : CharClassifyTable,
  val start : Int,
  val acceptsAttr : Array[IFAStateAttribute],
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

  val acceptSet = mutable.BitSet(states.filter(acceptsAttr(_) != null) : _*)

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