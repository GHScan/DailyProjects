package test.lexical

import org.scalatest._
import lexical.CharClassifyTableBuilder

class CharCategoryTest extends FlatSpec with Matchers {

  behavior of "Empty CharCategory"
  it should "map all to 0" in {
    val cc = new CharClassifyTableBuilder(4)
      .result

    cc.categories.length should equal(1)
  }

  behavior of "CharCategory with 3 distinct category"
  it should "map all to 0,1,2" in {
    val cc = new CharClassifyTableBuilder(128)
      .addChars("abc")
      .addChars("def")
      .result

    val category1 = "abc".map(cc(_)).toSet
    val category2 = "def".map(cc(_)).toSet
    val category3 = "hijgk".map(cc(_)).toSet
    category1.size should equal(1)
    category2.size should equal(1)
    category3.size should equal(1)
    cc.categories.toSet should equal(category1 | category2 | category3)
  }

  behavior of "CharCategory with 4 intersect category"
  it should "map all to 0,1,2,3" in {
    val cc = new CharClassifyTableBuilder(128)
      .addChars("abc")
      .addChars("bcd")
      .result

    val category1 = "a".map(cc(_)).toSet
    val category2 = "bc".map(cc(_)).toSet
    val category3 = "d".map(cc(_)).toSet
    val category4 = "ef".map(cc(_)).toSet
    category1.size should equal(1)
    category2.size should equal(1)
    category3.size should equal(1)
    category4.size should equal(1)
    cc.categories.toSet should equal(category1 | category2 | category3 | category4)
  }

  behavior of "Complex CharCategory"
  it should "map all to smaller space" in {
    val cc = new CharClassifyTableBuilder(8)
      .addChars("\u0000\u0001\u0002")
      .addChars("\u0001\u0002\u0003")
      .addChars("\u0002\u0003\u0004")
      .result

    cc.chars.length should equal(8)
    cc.categories.length should equal(6)
  }
}