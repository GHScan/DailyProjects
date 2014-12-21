import scala.collection.mutable
import scala.collection.immutable

object RegexParser {
  sealed abstract class Node
  case class EmptyChar() extends Node
  case class Literal(chars : Seq[Char]) extends Node
  case class Concatenation(first : Node, second : Node) extends Node
  case class Alternation(first : Node, second : Node) extends Node
  case class KleeneStar(node : Node) extends Node

  val MinChar = '\u0000'
  val MaxChar = '\u007f'
  val AllChars = Literal(MinChar to MaxChar)
  val Digits = Literal(AllChars.chars.filter(_.isDigit))
  val Lowers = Literal(AllChars.chars.filter(_.isLower))
  val Uppers = Literal(AllChars.chars.filter(_.isUpper))
  val Letters = Literal(AllChars.chars.filter(_.isLetter))
  val LetterDigits = Literal(AllChars.chars.filter(_.isLetterOrDigit))
  val WhiteSpaces = Literal(AllChars.chars.filter(_.isWhitespace))

  class Parser extends scala.util.parsing.combinator.RegexParsers {
    import scala.util.parsing.combinator._

    private val charParser : Parser[Node] = ("""[^\\\[\]\(\)\.\^\$\*\+\?\|]""".r ^^ (Literal(_))
      | "." ^^ (_ => AllChars)
      | """\\.""".r ^^ { s =>
        s.charAt(1) match {
          case 't' => Literal("\t")
          case 'n' => Literal("\n")
          case 'r' => Literal("\r")
          case 'd' => Digits
          case 'w' => LetterDigits
          case 'a' => Lowers
          case 'A' => Uppers
          case 's' => WhiteSpaces
        }
      }
      | "[" ~> opt("^") ~ """[^\]]+""".r <~ "]" ^^ {
        case None ~ s => Literal(s.distinct)
        case _ ~ s => Literal(AllChars.chars diff s)
      })
    private val factorParser : Parser[Node] = charParser | "(" ~> alternateParser <~ ")"
    private val kleeneStarParser : Parser[Node] = factorParser ~ opt("*" | "+" | "?") ^^ {
      case a ~ None => a
      case a ~ Some("*") => KleeneStar(a)
      case a ~ Some("+") => Concatenation(a, KleeneStar(a))
      case a ~ Some("?") => Alternation(a, EmptyChar())
    }
    private val concatenateParser : Parser[Node] = rep(kleeneStarParser) ^^ { x => x.tail.fold(x.head) { (a, b) => Concatenation(a, b) } }
    private val alternateParser : Parser[Node] = chainl1(concatenateParser, "|" ^^ { _ => (a, b) => Alternation(a, b) })

    def parse(s : String) : Node = parseAll(alternateParser, s).get
  }

  def parse(s : String) : Node = new Parser().parse(s)
}

class CharGroups(val groupCount : Int, table : Array[Int]) {
  def apply(c : Char) = table(c)

  def getGroupName(group : Int) : String = {
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

    if (group == NFA.EmptyGroup) "¦Å"
    else {
      val chars = (0 until table.length).filter(table(_) == group).toList
      chars2Segment(chars.head, chars.head, chars.tail).mkString(",")
    }
  }

  override def toString = s"CharGroups(count=$groupCount,${
    (0 until table.length).groupBy(table(_)).map(p => p._2.map(_.toChar)).toList.sortBy(_.length)
  })"
}

object CharGroups {
  def fromAST(node : RegexParser.Node) : CharGroups = {
    val table = Array.fill(RegexParser.MaxChar + 1)(0)
    var id = 0

    def processChars(chars : Seq[Char]) {
      chars.groupBy(table(_)).map(_._2).foreach { l =>
        id += 1
        l.foreach { c => table(c) = id }
      }
    }

    import RegexParser._
    def iterate(node : Node) {
      node match {
        case EmptyChar() =>
        case Literal(chars) => processChars(chars)
        case Concatenation(first, second) => { iterate(first); iterate(second) }
        case Alternation(first, second) => { iterate(first); iterate(second) }
        case KleeneStar(node) => { iterate(node) }
      }
    }
    iterate(node)

    id = -1
    processChars((0 until table.length).map(_.toChar))

    new CharGroups(id + 1, table)
  }
}

object NFA {
  val EmptyGroup = -1

  class Edge(val group : Int, val state : State)
  class State(var edges : Seq[Edge])

  def closure(states : Seq[State]) : List[State] = {
    var result = Set[State]()
    def iterate(state : State) {
      if (!result(state)) {
        result += state
        state.edges.filter(_.group == EmptyGroup).foreach(e => iterate(e.state))
      }
    }
    states.foreach(iterate)
    result.toList
  }

  def move(states : Seq[State], group : Int) : List[State] = {
    closure(states.flatMap { state => state.edges.filter(_.group == group).map(_.state) })
  }

  class Machine(val start : State, val end : State, val charGroups : CharGroups) {

    def matchPrefix(s : String) : String = {
      def iterate(states : Seq[State], i : Int, lastMatch : Int) : Int = {
        if (i == s.length) return lastMatch
        val newStates = move(states, charGroups(s(i)))
        if (newStates.isEmpty) lastMatch
        else iterate(newStates, i + 1, if (newStates.contains(end)) i else lastMatch)
      }
      s.substring(0, iterate(closure(List(start)), 0, -1) + 1)
    }

    def saveImage(imgPath : String) {
      val state2ID = mutable.Map[State, Int]()
      case class Node(state : State) extends GraphVisualizer.Node {
        override def shape = if (state == end) "doublecircle" else super.shape
        def label = state2ID.getOrElseUpdate(state, state2ID.size).toString
        def edges = state.edges.map(e => (charGroups.getGroupName(e.group), Node(e.state))).toList
      }
      GraphVisualizer.saveImage(imgPath, Node(start))
    }

    override def toString = s"NFAMachine(start=$start,end=$end)"
  }

  object Machine {

    def fromAST(node : RegexParser.Node, charGroups : CharGroups) : Machine = {

      import RegexParser._
      def iterate(node : Node) : (State, State) = {
        node match {
          case EmptyChar() => {
            val end = new State(Nil)
            (new State(List(new Edge(EmptyGroup, end))), end)
          }
          case Literal(chars) => {
            val end = new State(Nil)
            val start = new State(chars.map(charGroups(_)).distinct.map(new Edge(_, end)));
            (start, end)
          }
          case Concatenation(first, second) => {
            val (start1, end1) = iterate(first)
            val (start2, end2) = iterate(second)
            end1.edges :+= new Edge(EmptyGroup, start2)
            (start1, end2)
          }
          case Alternation(first, second) => {
            val (start1, end1) = iterate(first)
            val (start2, end2) = iterate(second)
            val end = new State(Nil)
            end1.edges :+= new Edge(EmptyGroup, end)
            end2.edges :+= new Edge(EmptyGroup, end);
            val start = new State(List(new Edge(EmptyGroup, start1), new Edge(EmptyGroup, start2)))
            (start, end)
          }
          case KleeneStar(node) => {
            val end = new State(Nil)
            val (start2, end2) = iterate(node)
            end2.edges :+= new Edge(EmptyGroup, start2)
            end2.edges :+= new Edge(EmptyGroup, end)
            val start = new State(List(new Edge(EmptyGroup, end), new Edge(EmptyGroup, start2)))
            (start, end)
          }
        }
      }

      val (start, end) = iterate(node)
      new Machine(start, end, charGroups)
    }
  }
}

object DFA {
  class Machine(
    val start : Int, val ends : Array[Boolean],
    val transfer : mutable.ArrayBuffer[Array[Int]],
    val charGroups : CharGroups) {
    val deadState = ((0 until transfer.length).filter(i => transfer(i).forall(_ == i)) ++ List(-1)).head

    def matchPrefix(s : String) : String = {
      def iterate(state : Int, i : Int, lastMatch : Int) : Int = {
        if (i == s.length) return lastMatch
        val newState = transfer(state)(charGroups(s(i)))
        if (newState == deadState) return lastMatch
        else iterate(newState, i + 1, if (ends(newState)) i else lastMatch)
      }
      s.substring(0, iterate(start, 0, -1) + 1)
    }

    def optimize() : Machine = {

      val toNewState = Array.tabulate(ends.length)(i => if (ends(i)) 0 else 1)
      var id = 1
      def iterate() {
        val newStates = (0 until ends.length).groupBy(toNewState(_)).map(_._2)
        for (
          newState <- newStates;
          group <- (0 until charGroups.groupCount)
        ) {
          val newerState = newState.groupBy(i => toNewState(transfer(i)(group))).map(_._2)
          if (newerState.size > 1) {
            newerState.foreach { l =>
              id += 1
              l.foreach { i => toNewState(i) = id }
            }
            return iterate()
          }
        }
      }
      iterate()

      id = 0
      (0 until ends.length).groupBy(toNewState(_)).map(_._2).foreach { l =>
        l.foreach { i => toNewState(i) = id }
        id += 1
      }

      val newTransfer = mutable.ArrayBuffer.fill(id)(Array.fill(charGroups.groupCount)(-1))
      val newEnds = Array.fill(id)(false)
      (0 until ends.length).foreach { i => newEnds(toNewState(i)) = ends(i) }
      for (
        i <- (0 until transfer.length);
        group <- (0 until charGroups.groupCount)
      ) {
        newTransfer(toNewState(i))(group) = toNewState(transfer(i)(group))
      }

      new Machine(toNewState(start), newEnds, newTransfer, charGroups)
    }

    def saveImage(imgPath : String) {
      case class Node(i : Int) extends GraphVisualizer.Node {
        override def shape = if (ends(i)) "doublecircle" else super.shape
        def label = i.toString
        def edges = transfer(i).zipWithIndex.filter(_._1 != deadState).map { case (j, group) => (charGroups.getGroupName(group), Node(j)) }.toList
      }
      GraphVisualizer.saveImage(imgPath, Node(start))
    }

    override def toString = s"DFAMachine(count=${ends.length},start=$start,ends=${ends.toList.zipWithIndex.filter(_._1).map(_._2)})"
  }

  object Machine {

    def fromNFA(nfa : NFA.Machine, charGroups : CharGroups) : Machine = {
      var stateMap = Map[List[NFA.State], Int]()
      val transfer = new mutable.ArrayBuffer[Array[Int]]

      def iterate(states : List[NFA.State], id : Int) {
        (0 until charGroups.groupCount).foreach { c =>
          val newStates = NFA.move(states, c)
          val newid = {
            if (!stateMap.contains(newStates)) {
              val newid = transfer.length
              transfer += Array.fill(charGroups.groupCount)(-1)
              stateMap += ((newStates, newid))
              iterate(newStates, newid)
              newid
            } else {
              stateMap.get(newStates).get
            }
          }
          transfer(id)(c) = newid
        }
      }

      val states = NFA.closure(List(nfa.start))
      stateMap += ((states, 0))
      transfer += Array.fill(charGroups.groupCount)(-1)
      iterate(states, 0)

      val starts = stateMap.filter(_._1.contains(nfa.start)).map(_._2)
      val ends = stateMap.filter(_._1.contains(nfa.end)).map(_._2).toList
      assert(starts.size == 1)
      assert(ends.size >= 1)
      new Machine(starts.head, Array.tabulate(transfer.length)(ends.contains(_)), transfer, charGroups)
    }
  }
}

object Test extends App {
  val ast = RegexParser.parse("""if|else|for|struct|\d+|\A+""")
  val charGroups = CharGroups.fromAST(ast)
  val nfa = NFA.Machine.fromAST(ast, charGroups)
  val dfa = DFA.Machine.fromNFA(nfa, charGroups)
  val odfa = dfa.optimize()
  println(ast)
  println(charGroups)
  println(nfa)
  println(dfa)
  println(odfa)
  Utils.timeit("saveImages", 1) {
    nfa.saveImage("nfa.png")
    dfa.saveImage("dfa.png")
    odfa.saveImage("odfa.png")
  }
  val s = """sjdkl"""
  println(nfa.matchPrefix(s))
  println(dfa.matchPrefix(s))
  println(odfa.matchPrefix(s))
}
