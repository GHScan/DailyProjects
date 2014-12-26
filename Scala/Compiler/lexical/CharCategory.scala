package lexical

class CharCategory(table : Array[Int]) {

  def apply(c : Char) : Int = 0

  def map(f : Int => Int) : CharCategory = null
}

object CharCategory {
  val EmptyCategory : Int = -1
}

class CharCategoryBuilder {

  def addChars(chars : String) {}

  def result : CharCategory = null
}