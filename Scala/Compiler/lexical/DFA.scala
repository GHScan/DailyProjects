package lexical

import scala.collection.mutable

trait DFATransition[T] extends NFATransition[T] {
  def target : DFAState[T]
}

trait DFAState[T] extends NFAState[T] {
  def transitions : List[DFATransition[T]]
}

trait DFA[T, U] extends NFA[T, U] {
  def start : DFAState[T]
  override def states : List[DFAState[T]] = super.states.asInstanceOf[List[DFAState[T]]]
  def accepts : List[DFAState[T]]
  def acceptsAttr : List[(DFAState[T], U)]
  def dead : DFAState[T]
}

trait TokenizedDFA extends DFA[CharCategory, TokenizedAcceptStateAttr] with TokenizedNFA {

}

abstract class DFAEmulator[U](
  val start : Int,
  val acceptAttrs : Array[Option[U]],
  val transitions : Array[Array[Int]]) {

  val dead = (states.filter(i => transitions(i).forall(_ == i)) ++ List(-1)).head

  def states : Seq[Int] = 0 until transitions.length
}

class TokenizedDFAEmulator(
  val charMap : CharCategoryMap,
  start : Int,
  acceptAttrs : Array[Option[TokenizedAcceptStateAttr]],
  transitions : Array[Array[Int]]) extends DFAEmulator[TokenizedAcceptStateAttr](
  start, acceptAttrs, transitions) {
  outer =>

  def toDFA() : TokenizedDFA = {
    class Transition(val symbol : CharCategory, val target : State) extends DFATransition[CharCategory]
    case class State(id : Int) extends DFAState[CharCategory] {
      def transitions : List[Transition] = outer.transitions(id).toList.zipWithIndex.map {
        case ((target, ci)) => new Transition(new CharCategory(ci), State(target))
      }
    }
    new TokenizedDFA {
      def start = State(outer.start)
      val accepts = outer.acceptAttrs.zipWithIndex.filter(_._1 != None).map(p => State(p._2)).toList
      val acceptsAttr = acceptAttrs.toList.zipWithIndex.filter(_._1 != None).map(p => (State(p._2), p._1.get))
      def dead = State(outer.dead)
      def charMap = outer.charMap
    }
  }

}

object TokenizedDFAEmulator {

  def apply(nfa : TokenizedNFAEmulator) : TokenizedDFAEmulator = {

    val stateSet2ID = mutable.Map[mutable.BitSet, Int]()
    val transitions = mutable.ArrayBuffer[Array[Int]]()
    var workList = List(nfa.closure(nfa.start))
    stateSet2ID(workList.head) = 0
    transitions += new Array[Int](nfa.charMap.categories.length)

    while (!workList.isEmpty) {
      val stateSet = workList.head
      val id = stateSet2ID(stateSet)
      workList = workList.tail

      for (category <- nfa.charMap.categories) {
        val targetSet = nfa.move(stateSet, category)
        transitions(id)(category.value) = stateSet2ID.getOrElseUpdate(targetSet, {
          transitions += new Array[Int](nfa.charMap.categories.length)
          workList = targetSet :: workList
          stateSet2ID.size
        })
      }
    }

    val start = stateSet2ID.filter(p => p._1.contains(nfa.start)).map(_._2).ensuring(_.size == 1).head
    val accept2Attr = stateSet2ID.toList.map { case (set, id) => (set & nfa.acceptSet, id) }.filter(!_._1.isEmpty).map {
      case (set, id) =>
        (id, nfa.acceptsAttr(set.toList.maxBy { x => nfa.acceptsAttr(x).get.priority }).get)
    }.toMap

    new TokenizedDFAEmulator(
      nfa.charMap,
      start,
      (0 until transitions.length).map(accept2Attr.get(_)).toArray,
      transitions.toArray)

  }

}