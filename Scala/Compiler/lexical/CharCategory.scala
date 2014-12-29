package lexical

class CharCategory(val value : Int) extends AnyVal

object CharCategory {

  val Empty = new CharCategory(-1)

  implicit val nfaSymbolClass = new NFASymbolClass[CharCategory] {
    val Empty = CharCategory.Empty
  }
}

final class CharCategoryMap(val table : Array[CharCategory]) extends Function1[Char, CharCategory] {

  def apply(c : Char) : CharCategory = table(c)
  def unapply(category : CharCategory) : Option[List[Char]] = Some(chars.filter(table(_) == category))

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

  def map(f : CharCategory => CharCategory) : CharCategoryMap = {
    new CharCategoryMap(table.map(f))
  }

  override def equals(other : Any) : Boolean = {
    other.isInstanceOf[CharCategoryMap] && other.asInstanceOf[CharCategoryMap].equals(this)
  }

  def equals(other : CharCategoryMap) : Boolean = {
    table.view == other.table.view
  }
}

class CharCategoryMapBuilder(size : Int = 128) {

  private val mTable : Array[CharCategory] = Array.fill(size)(new CharCategory(0))
  private var mNextCategory : Int = 0

  def addChars(chars : Seq[Char]) : CharCategoryMapBuilder = {
    chars.groupBy(mTable(_)).foreach {
      case (_, l) =>
        mNextCategory += 1
        l.foreach { c =>
          mTable(c) = new CharCategory(mNextCategory)
        }
    }

    this
  }

  def result : CharCategoryMap = {
    mNextCategory = 0
    (0 until mTable.length).groupBy(mTable(_)).toList.map(_._2).sortBy(_.head).foreach { l =>
      l.foreach { c =>
        mTable(c) = new CharCategory(mNextCategory)
      }
      mNextCategory += 1
    }

    new CharCategoryMap(mTable)
  }
}