package lexical

import scala.collection.mutable

trait IFATransition[T] {
  def symbol : T

  def target : IFAState[T]
}

trait IFAState[T] {
  def transitions : List[IFATransition[T]]
}

trait IFA[T] {

  def start : IFAState[T]

  def states : List[IFAState[T]] = _states

  private lazy final val _states : List[IFAState[T]] = {

    var result = List[IFAState[T]](start)
    val stateSet = mutable.Set[IFAState[T]](start)

    var workList = List(start)
    while (workList.nonEmpty) {
      val state = workList.head
      workList = workList.tail

      for (
        trans <- state.transitions;
        target = trans.target
        if !stateSet(target)
      ) {
        stateSet += target
        result = target :: result
        workList = target :: workList
      }
    }

    result.reverse
  }
}