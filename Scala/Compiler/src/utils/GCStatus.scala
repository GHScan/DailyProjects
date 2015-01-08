package utils

class GCStatus(_status : Seq[(String, Long, Float)]) {

  val status = _status.filter(_._2 > 0)

  def -(o : GCStatus) =
    new GCStatus(
      status.map(s =>
        o.status.find(s._1 == _._1) match {
          case None => s
          case Some((name, count, time)) => (name, s._2 - count, s._3 - time)
        }))

  override def toString = "GC:" +
    status
    .map(v => f"(${v._1}%s,count=${v._2}%d,time=${v._3}%.3f)")
    .mkString
}

object GCStatus {

  def apply() =
    new GCStatus(
      java.lang.management.ManagementFactory.getGarbageCollectorMXBeans
        .toArray
        .map { o =>
          val a = o.asInstanceOf[java.lang.management.GarbageCollectorMXBean]
          (a.getName, a.getCollectionCount, a.getCollectionTime / 1000.0f)
        })
}
