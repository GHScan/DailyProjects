package graphviz

import scala.collection.mutable

object Shape {
  val Circle = "circle"
  val DoubleCircle = "doublecircle"
  val Record = "record"
}

trait IState {
  def shape = Shape.Circle
  def label : String
  def transitions : Seq[(String, Any)]

  def exportAsImage(imgPath : String) {
    val state2ID = mutable.Map[IState, Int]()
    var foundStates = Set[IState]()
    val edgeStrings = new StringBuilder()

    def getStateID(state : IState) = state2ID.getOrElseUpdate(state, state2ID.size)

    def traverse(state : IState) {
      if (foundStates.contains(state)) return
      foundStates += state
      state.transitions.foreach {
        case (edgeLabel, target : IState) =>
          edgeStrings ++= s"""n_${getStateID(state)}->n_${getStateID(target)}[label=${utils.Func.escape(edgeLabel)}];\n"""
          traverse(target)
        case _ =>
      }
    }

    traverse(this)

    val script = s"""
        digraph name {
          $edgeStrings
          ${(for (state <- foundStates) yield s"""n_${getStateID(state)}[label=${utils.Func.escape(state.label)} shape=${state.shape}];\n""").mkString}
        }
        """

    import scala.sys.process._
    s"""dot -T${imgPath.substring(imgPath.lastIndexOf(".") + 1)} -o $imgPath""" #< new java.io.ByteArrayInputStream(script.getBytes("UTF-8")) !
  }
}
