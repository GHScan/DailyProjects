package lexical

class CharCategory(val value : Int) extends AnyVal

object CharCategory {

  val Empty = new CharCategory(-1)

  implicit val nfaSymbolClass = new NFASymbolClass[CharCategory] {
    val Empty = new CharCategory(-1)
  }
}

class CharCategoryMap(table : Array[CharCategory]) extends Function1[Char, CharCategory] {

  def apply(c : Char) : CharCategory = table(c)

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
}

class CharCategoryMapBuilder(size : Int = 128) {

  private val mTable : Array[CharCategory] = Array.fill(size)(new CharCategory(0))
  private var mNextCategroy : Int = 0

  def addChars(chars : Seq[Char]) : CharCategoryMapBuilder = {
    chars.groupBy(mTable(_)).foreach {
      case (_, l) =>
        mNextCategroy += 1
        l.foreach { c =>
          mTable(c) = new CharCategory(mNextCategroy)
        }
    }

    this
  }

  def result : CharCategoryMap = {
    mNextCategroy = -1
    (0 until mTable.length).groupBy(mTable(_)).foreach {
      case (_, l) =>
        mNextCategroy += 1
        l.foreach { c =>
          mTable(c) = new CharCategory(mNextCategroy)
        }
    }

    new CharCategoryMap(mTable)
  }
}