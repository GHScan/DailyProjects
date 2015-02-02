import scala.collection.mutable.ArrayBuffer

object Test extends App {

  object AST {
    sealed abstract class Tree
    case object Empty extends Tree
    case class Chars(chars : List[Char]) extends Tree
    case class Concatenation(first : Tree, second : Tree) extends Tree
    case class Alternation(first : Tree, second : Tree) extends Tree
    case class KleeneStar(tree : Tree) extends Tree
  }

  object RegexImplicits {

    import AST._

    implicit class TreeExtension[T](tree : T)(implicit cvt : T => Tree) {
      def ~(other : Tree) = Concatenation(tree, other)
      def |(other : Tree) = Alternation(tree, other)
      def ? = Alternation(tree, Empty)
      def star = KleeneStar(tree)
      def plus = Concatenation(tree, KleeneStar(tree))
    }

    implicit def string2Tree(s : String) : Tree = s.map(c => Chars(List(c))).reduce(Concatenation)
  }

  object ISA {
    sealed abstract class Instruction
    case class Chars(chars : List[Char]) extends Instruction
    case object Match extends Instruction
    case class Jump(target : Int) extends Instruction
    case class Split(first : Int, second : Int) extends Instruction
  }

  object Compiler {
    def compile(tree : AST.Tree, buffer : ArrayBuffer[ISA.Instruction]) : Unit = tree match {
      case AST.Chars(chars) => buffer += ISA.Chars(chars)
      case AST.Concatenation(first, second) =>
        compile(first, buffer)
        compile(second, buffer)
      case AST.Alternation(first, second) =>
        val tempBuffer1, tempBuffer2 = ArrayBuffer[ISA.Instruction]()
        compile(first, tempBuffer1)
        compile(second, tempBuffer2)
        val start = buffer.length
        buffer += ISA.Split(start + 1, start + 2 + tempBuffer1.length)
        buffer ++= tempBuffer1
        buffer += ISA.Jump(start + 2 + tempBuffer1.length + tempBuffer2.length)
        buffer ++= tempBuffer2
      case AST.Empty =>
      case AST.KleeneStar(tree2) =>
        val start = buffer.length
        val tempBuffer = ArrayBuffer[ISA.Instruction]()
        compile(tree2, tempBuffer)
        buffer += ISA.Split(start + 1, start + 2 + tempBuffer.length)
        buffer ++= tempBuffer
        buffer += ISA.Jump(start)
    }
    def compile(tree : AST.Tree) : Array[ISA.Instruction] = {
      val buffer = ArrayBuffer[ISA.Instruction]()
      compile(tree, buffer)
      buffer += ISA.Match
      buffer.toArray
    }
  }

  abstract class VM {
    def execute(codes : Array[ISA.Instruction], input : String) : String
  }

  object RecursiveVM extends VM {
    def execute(codes : Array[ISA.Instruction], input : String) : String = {

      def iterate(pc : Int, pos : Int) : Option[Int] = {
        if (pc == codes.length) None
        else codes(pc) match {
          case ISA.Chars(chars) if pos < input.length && chars.contains(input.charAt(pos)) => iterate(pc + 1, pos + 1)
          case ISA.Chars(chars) => None
          case ISA.Jump(target) => iterate(target, pos)
          case ISA.Split(first, second) => iterate(first, pos) match {
            case None => iterate(second, pos)
            case v => v
          }
          case ISA.Match => Some(pos)
        }
      }

      iterate(0, 0) match {
        case Some(pos) if pos == input.length => input
        case _ => ""
      }
    }
  }

  object ConcurrentVM extends VM {
    sealed abstract class State
    case class Process(pc : Int, pos : Int) extends State
    case class Accept(pos : Int) extends State

    def execute(codes : Array[ISA.Instruction], input : String) : String = {
      def derive(pc : Int, pos : Int) : List[State] = {
        if (pc == codes.length) Nil
        else codes(pc) match {
          case ISA.Chars(chars) if pos < input.length && chars.contains(input.charAt(pos)) => List(Process(pc + 1, pos + 1))
          case ISA.Chars(chars) => Nil
          case ISA.Jump(target) => List(Process(target, pos))
          case ISA.Split(first, second) => List(Process(first, pos), Process(second, pos))
          case ISA.Match => List(Accept(pos))
        }
      }

      def iterate(states : List[State]) : String = states match {
        case Nil => ""
        case Process(pc, pos) :: tail => iterate(derive(pc, pos) ::: tail)
        case Accept(pos) :: tail if pos == input.length => input
        case Accept(pos) :: tail => iterate(tail)
      }

      iterate(List(Process(0, 0)))
    }
  }

  import RegexImplicits._

  for (vm <- List(RecursiveVM, ConcurrentVM)) {
    {
      println("### VM:", vm)
    }
    {
      val codes = Compiler.compile("abc" | "def")
      println(vm.execute(codes, "abc"))
      println(vm.execute(codes, "def"))
      println(vm.execute(codes, "abcdef"))
    }
    {
      val codes = Compiler.compile("abc".? ~ "def")
      println(vm.execute(codes, "abc"))
      println(vm.execute(codes, "def"))
      println(vm.execute(codes, "abcdef"))
    }
    {
      val codes = Compiler.compile("abc".plus ~ "abc")
      println(vm.execute(codes, "abc"))
      println(vm.execute(codes, "def"))
      println(vm.execute(codes, "abcabc"))
    }
  }
}
