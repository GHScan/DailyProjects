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

trait TokenizedDFA extends DFA[CharCategory, TokenizedAcceptStateAttr] with TokenizedNFA

class TokenizedDFATransition(val symbol : CharCategory, val target : TokenizedDFAState) extends DFATransition[CharCategory]

class TokenizedDFAState(var transitions : List[TokenizedDFATransition]) extends DFAState[CharCategory]

abstract class DFAEmulator[U](
                               val start : Int,
                               val acceptAttrs : Array[Option[U]],
                               val transitions : Array[Array[Int]]) {

  override def equals(other : Any) : Boolean = {
    other.isInstanceOf[DFAEmulator[U]] && other.asInstanceOf[DFAEmulator[U]].equals(this)
  }

  def equals(other : DFAEmulator[U]) : Boolean = {
    (start == other.start
      && acceptAttrs.view == other.acceptAttrs.view
      && transitions.length == other.transitions.length
      && transitions.iterator.zip(other.transitions.iterator).forall {
      case ((a1, a2)) => a1.view == a2.view
    })
  }

  val dead = (states.filter(i => transitions(i).forall(_ == i)) ++ List(-1)).head

  def states : Seq[Int] = 0 until transitions.length
}

final class TokenizedDFAEmulator(
                                  val charTable : CharClassifyTable,
                                  start : Int,
                                  acceptAttrs : Array[Option[TokenizedAcceptStateAttr]],
                                  transitions : Array[Array[Int]]) extends DFAEmulator[TokenizedAcceptStateAttr](
  start, acceptAttrs, transitions) {
  outer =>

  override def equals(other : Any) : Boolean = {
    other.isInstanceOf[TokenizedDFAEmulator] && other.asInstanceOf[TokenizedDFAEmulator].equals(this)
  }

  def equals(other : TokenizedDFAEmulator) : Boolean = {
    charTable == other.charTable && super.equals(other)
  }

  def toDFA : TokenizedDFA = {
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

      def charTable = outer.charTable
    }
  }

  def minimized : TokenizedDFAEmulator = {

    val state2Group = Array.fill(states.length)(0)
    var gid = 0
    states.groupBy(acceptAttrs(_)).foreach {
      case (_, l) =>
        l.foreach { c => state2Group(c) = gid}
        gid += 1
    }

    def iterate() {
      for (
        (_, group) <- states.groupBy(state2Group(_));
        category <- charTable.categories
      ) {
        val newGroups = group.groupBy(state => state2Group(transitions(state)(category.value)))
        if (newGroups.size > 1) {
          newGroups.foreach {
            case (_, l) =>
              l.foreach { c => state2Group(c) = gid}
              gid += 1
          }
          iterate()
        }
      }
    }
    iterate()

    gid = 0
    states.groupBy(state2Group(_)).foreach {
      case (_, l) =>
        l.foreach { c => state2Group(c) = gid}
        gid += 1
    }

    val newTransitions = Array.fill(state2Group.distinct.length, charTable.categories.length)(0)
    val newAcceptAttrs = Array.fill(newTransitions.length)(acceptAttrs(0))
    states.foreach { s => newAcceptAttrs(state2Group(s)) = acceptAttrs(s)}
    for (
      state <- states;
      category <- charTable.categories
    ) {
      newTransitions(state2Group(state))(category.value) = state2Group(transitions(state)(category.value))
    }
    new TokenizedDFAEmulator(charTable, state2Group(start), newAcceptAttrs, newTransitions)
  }

  def charTableCompacted : TokenizedDFAEmulator = {

    val oldCategory2New = charTable.categories.groupBy(c => transitions.toList.map(t => t(c.value))).toList.map(_._2).sortBy(_.head.value).zipWithIndex.
      flatMap {
      case ((l, i)) => l.map((_, new CharCategory(i)))
    }.toMap
    val newColumns = oldCategory2New.map(p => (p._2.value, p._1.value)).toList.sortBy(_._1).map(_._2).toArray

    val newCharMap = charTable.map(oldCategory2New)
    new TokenizedDFAEmulator(newCharMap, start, acceptAttrs, transitions.map(t => newColumns.map(t)))
  }

  def optimized : TokenizedDFAEmulator = minimized.charTableCompacted
}

object TokenizedDFAEmulator {

  def fromNFAEmulator(nfa : TokenizedNFAEmulator) : TokenizedDFAEmulator = {

    val stateSet2ID = mutable.Map[mutable.BitSet, Int]()
    val transitions = mutable.ArrayBuffer[Array[Int]]()
    var workList = List(nfa.closure(nfa.start))
    stateSet2ID(workList.head) = 0
    transitions += new Array[Int](nfa.charTable.categories.length)

    while (workList.nonEmpty) {
      val stateSet = workList.head
      val id = stateSet2ID(stateSet)
      workList = workList.tail

      for (category <- nfa.charTable.categories) {
        val targetSet = nfa.move(stateSet, category)
        transitions(id)(category.value) = stateSet2ID.getOrElseUpdate(targetSet, {
          transitions += new Array[Int](nfa.charTable.categories.length)
          workList = targetSet :: workList
          stateSet2ID.size
        })
      }
    }

    val start = stateSet2ID.filter(p => p._1.contains(nfa.start)).map(_._2).ensuring(_.size == 1).head
    val accept2Attr = stateSet2ID.toList.map { case (set, id) => (set & nfa.acceptSet, id)}.filter(_._1.nonEmpty).map {
      case (set, id) =>
        (id, nfa.acceptsAttr(set.toList.maxBy { x => nfa.acceptsAttr(x).get.priority}).get)
    }.toMap

    new TokenizedDFAEmulator(
      nfa.charTable,
      start,
      (0 until transitions.length).map(accept2Attr.get).toArray,
      transitions.toArray)

  }

}