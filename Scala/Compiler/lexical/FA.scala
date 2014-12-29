package lexical

import scala.collection.mutable

trait FATransition[T] {
  def symbol : T
  def target : FAState[T]
}

trait FAState[T] {
  def transitions : List[FATransition[T]]
}

trait FA[T] {

  def start : FAState[T]

  def states : List[FAState[T]] = _states

  private lazy final val _states : List[FAState[T]] = {

    var result = List[FAState[T]](start)
    val stateSet = mutable.Set[FAState[T]](start)

    var workList = List(start)
    while (!workList.isEmpty) {
      val state = workList.head
      workList = workList.tail

      for (
        trans <- state.transitions;
        target = trans.target;
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