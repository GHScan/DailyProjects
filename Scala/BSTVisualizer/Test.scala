import scala.collection.mutable
import scala.collection.immutable

object Test extends App {
  sealed abstract class Node
  object Leaf extends Node
  case class Branch(value : Int, left : Node, right : Node) extends Node with GraphVisualizer.Node {
    def label = value.toString
    def edges = List(left, right).filter(_ != Leaf).map(s => ("", s.asInstanceOf[Branch]))
  }

  def insert(n : Node, value : Int) : Node = {
    n match {
      case Leaf => Branch(value, Leaf, Leaf)
      case Branch(i, _, _) if value == i => n
      case Branch(i, left, right) if value < i => Branch(i, insert(left, value), right)
      case Branch(i, left, right) => Branch(i, left, insert(right, value))
    }
  }

  def buildTree(values : Int*) = values.foldLeft(Leaf : Node)(insert)

  GraphVisualizer.saveImage("tree.png", buildTree(3, 1, 5, 2, 4, 9, 0, 8, 7).asInstanceOf[Branch])
}
