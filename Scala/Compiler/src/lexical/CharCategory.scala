package lexical

class CharCategory(val value : Int) extends AnyVal

object CharCategory {

  val Empty = new CharCategory(-1)
}

final class CharClassifyTable(val table : Array[CharCategory]) extends (Char => CharCategory) {

  override def equals(other : Any) : Boolean = {
    other.isInstanceOf[CharClassifyTable] && other.asInstanceOf[CharClassifyTable].equals(this)
  }

  def equals(other : CharClassifyTable) : Boolean = {
    table.view == other.table.view
  }

  def apply(c : Char) : CharCategory = table(c)

  def lookup(c : Char) : CharCategory = table(c)

  def rlookup(category : CharCategory) : List[Char] = chars.filter(table(_) == category)

  lazy val chars : List[Char] = (0 until table.length).map(_.toChar).toList
  lazy val categories : List[CharCategory] = table.distinct.toList

  def toPrettyString(category : CharCategory) : String = {
    if (category == CharCategory.Empty) "ε"
    else {
      utils.Func.splitToContinuesSegments(chars.filter(table(_) == category)).map {
        case List(x) => x.toString
        case List(x, y) => s"$x,$y"
        case l => s"${l.head}-${l.last}"
      }.mkString(",")
    }
  }

  def map(f : CharCategory => CharCategory) : CharClassifyTable = {
    new CharClassifyTable(table.map(f))
  }

  def union(other : CharClassifyTable) : CharClassifyTable = {
    val builder = new CharClassifyTableBuilder()
    for (c <- categories) builder.addChars(rlookup(c))
    for (c <- other.categories) builder.addChars(other.rlookup(c))
    builder.result
  }
}

class CharClassifyTableBuilder(size : Int = 128) {

  private val table : Array[CharCategory] = Array.fill(size)(new CharCategory(0))
  private var nextCategory : Int = 0

  def addChars(chars : Seq[Char]) : CharClassifyTableBuilder = {
    chars.groupBy(table(_)).foreach {
      case (_, l) =>
        nextCategory += 1
        l.foreach { c =>
          table(c) = new CharCategory(nextCategory)
        }
    }

    this
  }

  def result : CharClassifyTable = {
    nextCategory = 0
    (0 until table.length).groupBy(table(_)).toList.map(_._2).sortBy(_.head).foreach { l =>
      l.foreach { c =>
        table(c) = new CharCategory(nextCategory)
      }
      nextCategory += 1
    }

    new CharClassifyTable(table)
  }
}