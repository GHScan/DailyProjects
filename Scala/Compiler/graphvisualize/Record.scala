package graphvisualize

import scala.collection.mutable

trait Record {
  def width = 1.0
  def height = 0.5
  def fields : Seq[Record#Field]

  sealed abstract class Field
  case class Value(name : String, value : Any) extends Field
  case class Link(name : String, value : Any) extends Field

  def exportAsImage(imgPath : String) {
    var foundRecord = Set[Record]()
    val record2ID = mutable.Map[Record, Int]()
    val edgeStrings = new StringBuilder()

    def getRecordID(record : Record) = record2ID.getOrElseUpdate(record, record2ID.size)
    def getRecordLabel(record : Record) =
      record.fields.zipWithIndex.map {
        case (Link(name, _), index) => s"<f_$index>$name"
        case (Value("", v), index) => s"<f_$index>$v"
        case (Value(name, v), index) => s"<f_$index>$name=$v"
        case _ => assert(false, "It's impossible!")
      }.mkString("|")
    def traverse(record : Record) {
      if (foundRecord(record)) return
      foundRecord += record
      record.fields.zipWithIndex.foreach {
        case (Link(_, target : Record), index) =>
          edgeStrings ++=
            s"""n_${getRecordID(record)}:f_$index->n_${getRecordID(target)}:f_0;\n"""
          traverse(target)
        case _ =>
      }
    }
    traverse(this)

    val script = s"""
        digraph name {
          $edgeStrings
          ${
      (for (record <- foundRecord)
        yield s"""n_${getRecordID(record)}[width=${record.width} height=${record.height} shape=${Shape.Record} label=${utils.Func.escape(getRecordLabel(record))}];\n""").mkString
    }
        }
        """

    import scala.sys.process._
    s"""dot -T${imgPath.substring(imgPath.lastIndexOf(".") + 1)} -o $imgPath""" #< new java.io.ByteArrayInputStream(script.getBytes("UTF-8")) !
  }
}
