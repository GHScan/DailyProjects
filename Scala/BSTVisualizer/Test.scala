import scala.collection.mutable
import scala.collection.immutable

object Test extends App {
  sealed abstract class Node
  object Leaf extends Node
  case class Branch(value : Int, left : Node, right : Node) extends Node

  def insert(n : Node, value : Int) : Node = {
    n match {
      case Leaf => Branch(value, Leaf, Leaf)
      case Branch(i, _, _) if value == i => n
      case Branch(i, left, right) if value < i => Branch(i, insert(left, value), right)
      case Branch(i, left, right) => Branch(i, left, insert(right, value))
    }
  }

  def buildTree(values : Int*) = values.foldLeft(Leaf : Node)(insert)

  def saveImage(n : Node, imgPath : String) {
    def iterate(n : Node, parent : Branch) : List[String] = {
      n match {
        case Leaf => Nil
        case b@Branch(i, left, right) if parent == null => iterate(left, b) ::: iterate(right, b)
        case b@Branch(i, left, right) => s"${parent.value}->$i;" :: iterate(left, b) ::: iterate(right, b)
      }
    }

    val script = s"""
    digraph name  {
    ${iterate(n, null).mkString("\n")}
    }
      """

    import scala.sys.process._
    s"dot -Tpng -o $imgPath" #< new java.io.ByteArrayInputStream(script.getBytes("UTF-8")) !
  }

  saveImage(buildTree(3, 1, 5, 2, 4, 9, 0, 8, 7), "tree.png")
}
