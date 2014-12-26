package lexical

import scala.collection.mutable

object NFA {
  
  class Transition(val category : Int, val targetState : Int)

  class Machine(
      val transitionTable : mutable.ArrayBuffer[List[Transition]],
      val startState : Int,
      val acceptStates : List[Int]) {
    
  }
  
}