package utils

class GCStatus(_infos : Seq[(String, Long, Float)]) {

  val infos = _infos.filter(_._2 > 0)

  def -(o : GCStatus) =
    new GCStatus(
      infos.map(s =>
        o.infos.find(s._1 == _._1) match {
          case None => s
          case Some((oname, ocount, otime)) => (oname, s._2 - ocount, s._3 - otime)
        }))

  override def toString = "GC:" +
    infos
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
