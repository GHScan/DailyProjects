package lexical

import scala.collection.mutable

trait NFASymbolClass[T] {
  val Empty : T
}

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

case class TokenizedAcceptStateAttr(val priority : Int, val token : String)

trait TokenizedNFA extends NFA[CharCategory, TokenizedAcceptStateAttr] {
  def charMap : CharCategoryMap
}

object TokenizedNFA {

  private def parseRegex(pattern : String) : (RegexAST.Tree, CharCategoryMap) = {
    import RegexAST._

    val tree = new RegexParser().parse(pattern)

    val builder = new CharCategoryMapBuilder(128)
    transform(tree, {
      case t@Chars(chars : Seq[_]) if chars.head.isInstanceOf[Char] => {
        builder.addChars(chars.asInstanceOf[Seq[Char]])
        t
      }
      case t => t
    })

    val charMap = builder.result
    val newTree = transform(tree, {
      case t@Chars(chars : Seq[_]) if chars.head.isInstanceOf[Char] => {
        Chars(chars.asInstanceOf[Seq[Char]].map(charMap).distinct)
      }
      case t => t
    })

    (newTree, charMap)
  }

  def fromRegex(pattern : String, priority : Int = 0)(implicit symbolClass : NFASymbolClass[CharCategory]) : TokenizedNFA = {
    import RegexAST._

    class Transition(val symbol : CharCategory, val target : State) extends NFATransition[CharCategory]
    class State(var transitions : List[Transition]) extends NFAState[CharCategory]

    def iterate(tree : Tree) : (State, State) = tree match {
      case Empty => {
        val accept = new State(Nil)
        (new State(List(new Transition(symbolClass.Empty, accept))), accept)
      }
      case Chars(chars : Seq[_]) if chars.head.isInstanceOf[CharCategory] => {
        val accept = new State(Nil)
        (new State(chars.asInstanceOf[Seq[CharCategory]].map(symbol => new Transition(symbol, accept)).toList), accept)
      }
      case KleeneStar(content) => {
        val (start, accept) = iterate(content)
        val accept2 = new State(Nil)
        accept.transitions = new Transition(symbolClass.Empty, start) :: accept.transitions
        accept.transitions = new Transition(symbolClass.Empty, accept2) :: accept.transitions
        (new State(List(new Transition(symbolClass.Empty, start), new Transition(symbolClass.Empty, accept2))), accept2)
      }
      case Concatenation(first, second) => {
        val (start1, accept1) = iterate(first)
        val (start2, accept2) = iterate(second)
        accept1.transitions = new Transition(symbolClass.Empty, start2) :: accept1.transitions
        (start1, accept2)
      }
      case Alternation(first, second) => {
        val (start1, accept1) = iterate(first)
        val (start2, accept2) = iterate(second)
        val start = new State(List(start1, start2).map(new Transition(symbolClass.Empty, _)))
        val accept = new State(Nil)
        accept1.transitions = new Transition(symbolClass.Empty, accept) :: accept1.transitions
        accept2.transitions = new Transition(symbolClass.Empty, accept) :: accept2.transitions
        (start, accept)
      }
    }

    val (tree, charMap1) = parseRegex(pattern)
    val (start1, accept1) = iterate(tree)
    new TokenizedNFA {
      val start : NFAState[CharCategory] = start1
      val accepts : List[NFAState[CharCategory]] = List(accept1)
      val acceptsAttr : List[(NFAState[CharCategory], TokenizedAcceptStateAttr)] = List((accept1, new TokenizedAcceptStateAttr(priority, pattern)))
      val charMap : CharCategoryMap = charMap1
    }
  }

}

abstract class NFAEmulator[T, U](
  val start : Int,
  val acceptsAttr : Array[Option[U]],
  val transitions : Array[List[(T, Int)]])(implicit symbolClass : NFASymbolClass[T]) {

  override def equals(other : Any) : Boolean = {
    other.isInstanceOf[NFAEmulator[T, U]] && other.asInstanceOf[NFAEmulator[T, U]].equals(this)
  }
  def equals(other : NFAEmulator[T, U]) : Boolean = {
    (start == other.start
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
          trans <- transitions(state);
          if symbolClass.Empty == trans._1
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

  def move(stateSet : mutable.BitSet, symbol : T) : mutable.BitSet = {
    val result = mutable.BitSet()

    for (
      state <- stateSet;
      trans <- transitions(state);
      if trans._1 == symbol
    ) {
      result |= closure(trans._2)
    }

    result
  }

}

class TokenizedNFAEmulator(
  val charMap : CharCategoryMap,
  start : Int,
  acceptsAttr : Array[Option[TokenizedAcceptStateAttr]],
  transitions : Array[List[(CharCategory, Int)]]) extends NFAEmulator[CharCategory, TokenizedAcceptStateAttr](
  start, acceptsAttr, transitions) {

  override def equals(other : Any) : Boolean = {
    other.isInstanceOf[TokenizedNFAEmulator] && other.asInstanceOf[TokenizedNFAEmulator].equals(this)
  }
  def equals(other : TokenizedNFAEmulator) : Boolean = {
    charMap == other.charMap && super.equals(this)
  }
}

object TokenizedNFAEmulator {

  def apply(nfa : TokenizedNFA) : TokenizedNFAEmulator = {

    val state2ID = nfa.states.zipWithIndex.toMap

    new TokenizedNFAEmulator(
      nfa.charMap,
      state2ID(nfa.start),
      state2ID.toList.sortBy(_._2).map { case ((state, _)) => nfa.acceptsAttr.find(_._1 == state).map(_._2) }.toArray,
      state2ID.toList.sortBy(_._2).map {
        case ((state, _)) => state.transitions.map { t => (t.symbol, state2ID(t.target)) }
      }.toArray)
  }
}