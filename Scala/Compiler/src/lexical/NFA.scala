package lexical

import scala.collection.mutable

trait NFATransition[T] extends FATransition[T] {
  def target : NFAState[T]
}

trait NFAState[T] extends FAState[T] {
  def transitions : List[NFATransition[T]]
}

trait NFA[T, U] extends FA[T] {
  def start : NFAState[T]

  override def states : List[NFAState[T]] = super.states.asInstanceOf[List[NFAState[T]]]

  def accepts : List[NFAState[T]]

  def acceptsAttr : List[(NFAState[T], U)]
}


case class TokenizedAcceptStateAttr(priority : Int, token : String)

trait TokenizedNFA extends NFA[CharCategory, TokenizedAcceptStateAttr] {

  def charTable : CharClassifyTable

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
}

class TokenizedNFATransition(val symbol : CharCategory, val target : TokenizedNFAState) extends NFATransition[CharCategory]

class TokenizedNFAState(var transitions : List[TokenizedNFATransition]) extends NFAState[CharCategory]

object TokenizedNFA {

  private def parseRegex(pattern : String) : (RegexAST.Tree, CharClassifyTable) = {
    import RegexAST._

    val tree = new RegexParser().parse(pattern)

    val builder = new CharClassifyTableBuilder(128)
    tree.transform {
      case t@Chars(chars : Seq[_]) if chars.head.isInstanceOf[Char] =>
        builder.addChars(chars.asInstanceOf[Seq[Char]])
        t
      case t => t
    }

    val charTable = builder.result
    val newTree = tree.transform {
      case t@Chars(chars : Seq[_]) if chars.head.isInstanceOf[Char] => Chars(chars.asInstanceOf[Seq[Char]].map(charTable).distinct)
      case t => t
    }

    (newTree, charTable)
  }

  def fromPattern(pattern : String, priority : Int = 0, token : String = null) : TokenizedNFA = {
    import RegexAST._

    type State = TokenizedNFAState
    type Transition = TokenizedNFATransition

    def iterate(tree : Tree) : (State, State) = tree match {
      case Empty =>
        val accept = new State(Nil)
        (new State(List(new Transition(CharCategory.Empty, accept))), accept)
      case Chars(chars : Seq[_]) if chars.head.isInstanceOf[CharCategory] =>
        val accept = new State(Nil)
        (new State(chars.asInstanceOf[Seq[CharCategory]].map(symbol => new Transition(symbol, accept)).toList), accept)
      case KleeneStar(content) =>
        val (start, accept) = iterate(content)
        val accept2 = new State(Nil)
        accept.transitions = new Transition(CharCategory.Empty, start) :: accept.transitions
        accept.transitions = new Transition(CharCategory.Empty, accept2) :: accept.transitions
        (new State(List(new Transition(CharCategory.Empty, start), new Transition(CharCategory.Empty, accept2))), accept2)
      case Concatenation(first, second) =>
        val (start1, accept1) = iterate(first)
        val (start2, accept2) = iterate(second)
        accept1.transitions = new Transition(CharCategory.Empty, start2) :: accept1.transitions
        (start1, accept2)
      case Alternation(first, second) =>
        val (start1, accept1) = iterate(first)
        val (start2, accept2) = iterate(second)
        val start = new State(List(start1, start2).map(new Transition(CharCategory.Empty, _)))
        val accept = new State(Nil)
        accept1.transitions = new Transition(CharCategory.Empty, accept) :: accept1.transitions
        accept2.transitions = new Transition(CharCategory.Empty, accept) :: accept2.transitions
        (start, accept)
    }

    val (tree, charTable1) = parseRegex(pattern)
    val (start1, accept1) = iterate(tree)
    new TokenizedNFA {
      val start : NFAState[CharCategory] = start1
      val accepts : List[NFAState[CharCategory]] = List(accept1)
      val acceptsAttr : List[(NFAState[CharCategory], TokenizedAcceptStateAttr)] = List((accept1, TokenizedAcceptStateAttr(priority, if (token == null) pattern else token)))
      val charTable : CharClassifyTable = charTable1
    }
  }

}

final class TokenizedNFAEmulator(
                                  val charTable : CharClassifyTable,
                                  val start : Int,
                                  val acceptsAttr : Array[Option[TokenizedAcceptStateAttr]],
                                  val transitions : Array[List[(CharCategory, Int)]])  {

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