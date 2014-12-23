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

  def State_exportBST(n : Node, imgPath : String) {
    case class State(b : Branch) extends GraphVisualize.StateMachine.State {
      def label = b.value.toString
      def transitions = List(b.left, b.right).map(s => ("", node2State(s)))
    }
    def node2State(n : Node) = if (n == Leaf) null else State(n.asInstanceOf[Branch])
    node2State(n).exportAsImage(imgPath)
  }
  def Record_exportBST(n : Node, imgPath : String) {
    case class Record(b : Branch) extends GraphVisualize.Structure.Record {
      def fields = List(Link("", node2Record(b.left)), Value("", b.value), Link("", node2Record(b.right)))
    }
    def node2Record(n : Node) = if (n == Leaf) null else Record(n.asInstanceOf[Branch])
    node2Record(n).exportAsImage(imgPath)
  }

  def insert(a : Array[List[String]], value : String) {
    val index = value.hashCode % a.length
    a(index) = value :: a(index)
  }
  def buildHash(values : String*) = {
    val a = Array.fill(16)(List[String]())
    values.foreach { i => insert(a, i) }
    a
  }
  def Record_exportHash(a : Array[List[String]], imgPath : String) {
    case class ArrayRecord(a : Array[List[String]]) extends GraphVisualize.Structure.Record {
      override def height = 0.1
      override def width = 8
      def fields = a.map(s => Link("", list2Record(s)))
    }
    case class ListRecord(l : List[String]) extends GraphVisualize.Structure.Record {
      override def height = 0.1
      override def width = 0.1
      def fields = List(Value("", l.head), Link("", list2Record(l.tail)))
    }
    def list2Record(l : List[String]) = if (l.isEmpty) null else ListRecord(l)

    ArrayRecord(a).exportAsImage(imgPath)
  }

  val bst = buildTree(3, 1, 9, 7, 6, 4, 5, 0, 8, 2)
  State_exportBST(bst, "bst_state.jpg")
  Record_exportBST(bst, "bst_record.jpg")

  val hash = buildHash((0 until 16).map(_.toString) : _*)
  Record_exportHash(hash, "hash_record.jpg")
}