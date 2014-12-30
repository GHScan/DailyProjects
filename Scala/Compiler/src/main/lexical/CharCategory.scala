package lexical

class CharCategory(val value: Int) extends AnyVal

object CharCategory {

  val Empty = new CharCategory(-1)

  implicit val nfaSymbolClass = new NFASymbolClass[CharCategory] {
    val Empty = CharCategory.Empty
  }
}

final class CharClassifyTable(val table: Array[CharCategory]) extends (Char => CharCategory) {

  def apply(c: Char): CharCategory = table(c)

  def unapply(category: CharCategory): Option[List[Char]] = Some(chars.filter(table(_) == category))

  lazy val chars: List[Char] = (0 until table.length).map(_.toChar).toList
  lazy val categories: List[CharCategory] = table.distinct.toList

  def toPrettyString(category: CharCategory): String = {
    if (category == CharCategory.Empty) "ε"
    else {
      utils.Func.splitToContinuesSegments(chars.filter(table(_) == category)).map {
        case List(x) => x.toString
        case List(x, y) => s"$x,$y"
        case l => s"${l.head}-${l.last}"
      }.mkString(",")
    }
  }

  def map(f: CharCategory => CharCategory): CharClassifyTable = {
    new CharClassifyTable(table.map(f))
  }

  override def equals(other: Any): Boolean = {
    other.isInstanceOf[CharClassifyTable] && other.asInstanceOf[CharClassifyTable].equals(this)
  }

  def equals(other: CharClassifyTable): Boolean = {
    table.view == other.table.view
  }
}

class CharClassifyTableBuilder(size: Int = 128) {

  private val mTable: Array[CharCategory] = Array.fill(size)(new CharCategory(0))
  private var mNextCategory: Int = 0

  def addChars(chars: Seq[Char]): CharClassifyTableBuilder = {
    chars.groupBy(mTable(_)).foreach {
      case (_, l) =>
        mNextCategory += 1
        l.foreach { c =>
          mTable(c) = new CharCategory(mNextCategory)
        }
    }

    this
  }

  def result: CharClassifyTable = {
    mNextCategory = 0
    (0 until mTable.length).groupBy(mTable(_)).toList.map(_._2).sortBy(_.head).foreach { l =>
      l.foreach { c =>
        mTable(c) = new CharCategory(mNextCategory)
      }
      mNextCategory += 1
    }

    new CharClassifyTable(mTable)
  }
}