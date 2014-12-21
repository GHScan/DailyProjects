import scala.collection.mutable

object Visualizer {

  private def escape(s : String) : String = {
    import scala.reflect.runtime.universe._
    Literal(Constant(s)).toString
  }

  private def group2EdgeName(group : Int, charGroups : CharGroups) : String = {
    def makeRange(first : Int, last : Int) : String = {
      if (first == last) first.toChar.toString
      else s"${first.toChar}-${last.toChar}"
    }
    def chars2Segment(first : Int, prev : Int, chars : List[Int]) : List[String] = {
      chars match {
        case Nil => List(makeRange(first, prev))
        case head :: tail if head == prev + 1 => chars2Segment(first, head, tail)
        case head :: tail => makeRange(first, prev) :: chars2Segment(head, head, tail)
      }
    }

    if (group == NFA.EmptyGroup) "ε"
    else {
      val chars = charGroups.getGroupChars(group)
      chars2Segment(chars.head, chars.head, chars.tail).mkString(",")
    }
  }

  implicit def nfa2Visualizer(nfa : NFA.Machine) = new {
    def saveImage(imgPath : String) = {
      import NFA._

      val state2ID = mutable.Map[State, Int]()
      val state2String = mutable.Map[State, String]()

      def getStateString(state : State) : String = state2ID.getOrElseUpdate(state, state2ID.size).toString
      def generateEdgesString(state : State) {
        if (state2String.contains(state)) return
        val s = state.edges.map(e =>
          s"""${getStateString(state)}->${getStateString(e.state)}[label=${escape(group2EdgeName(e.group, nfa.charGroups))}];""").mkString("\n")
        state2String(state) = s
        state.edges.foreach { e => generateEdgesString(e.state) }
      }
      generateEdgesString(nfa.start)

      val script = s"""
      digraph graphname {  
        ${state2String.values.mkString("\n")}
        ${getStateString(nfa.end)}[shape=doublecircle];
      }  
      """

      import scala.sys.process._
      s"dot -Tpng -o $imgPath" #< new java.io.ByteArrayInputStream(script.getBytes("UTF-8")) !
    }
  }

  implicit def dfa2Visualizer(dfa : DFA.Machine) = new {
    def saveImage(imgPath : String) = {
      import DFA._

      val script = s"""
      digraph graphname {  
        ${
        (for (
          from <- (0 until dfa.transfer.length);
          group <- (0 until dfa.charGroups.groupCount);
          to = dfa.transfer(from)(group);
          if to != dfa.deadState
        ) yield s"""$from->$to[label=${escape(group2EdgeName(group, dfa.charGroups))}];\n""").mkString
      }
      ${
        (for (
          i <- (0 until dfa.ends.length);
          if dfa.ends(i)
        ) yield s"$i[shape=doublecircle];\n").mkString
      }
      }  
      """

      import scala.sys.process._
      s"dot -Tpng -o $imgPath" #< new java.io.ByteArrayInputStream(script.getBytes("UTF-8")) !
    }
  }
}
