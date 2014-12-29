package lexical

import scala.collection.mutable

final class IterativeDFABuilder[T] {
  outer =>

  private var charMap : CharCategoryMap = null
  private var init : T = _
  private var dead : T = _
  private var accepts : List[T] = Nil
  private var func : (T, Char) => T = null

  def charSet(chars : Seq[Char]) = {
    val builder = new CharCategoryMapBuilder()
    chars.foreach { c => builder.addChars(List(c)) }
    charMap = builder.result
    this
  }

  def initialValue(value : T) = {
    init = value
    this
  }

  def deadValue(value : T) = {
    dead = value
    this
  }

  def acceptValue(value : T) = {
    accepts = value :: accepts
    this
  }

  def iterateFunc(f : (T, Char) => T) = {
    func = f
    this
  }

  def result : TokenizedDFA = {
    class Transition(val symbol : CharCategory, val target : State) extends DFATransition[CharCategory]
    class State(var transitions : List[Transition]) extends DFAState[CharCategory]

    val value2State = mutable.Map[T, State](outer.dead -> new State(Nil))
    def getOrAddState(value : T) : State = {
      if (value2State.contains(value)) value2State(value)
      else {
        val state = new State(Nil)
        value2State(value) = state
        charMap.categories.foreach { category =>
          val target = if (category == charMap(0)) value2State(outer.dead) else getOrAddState(func(value, charMap.unapply(category).get.head))
          state.transitions = new Transition(category, target) :: state.transitions
        }
        state
      }
    }
    getOrAddState(init)

    new TokenizedDFA {
      def start = value2State(init)
      val accepts = value2State.iterator.filter(p => outer.accepts.contains(p._1)).map(_._2).toList
      val acceptsAttr = value2State.iterator.filter(p => outer.accepts.contains(p._1)).map(p => (p._2, new TokenizedAcceptStateAttr(0, p._1.toString))).toList
      def dead = value2State(outer.dead)
      def charMap = outer.charMap
    }
  }
}