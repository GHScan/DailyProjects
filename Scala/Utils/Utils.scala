class GCStatus(_status : List[(String, Long, Float)]) {
    val status = _status.filter(_._2 > 0)
    def -(o : GCStatus) =
        new GCStatus(
            status.map(s =>
                o.status.find(s._1 == _._1) match {
                    case None => s
                    case Some((oname, ocount, otime)) => (oname, s._2 - ocount, s._3 - otime)
                }))
    override def toString = "GC:" +
        status
        .map(v => f"(${v._1}%s,count=${v._2}%d,time=${v._3}%.3f)")
        .mkString
}

object GC {
    def status =
        new GCStatus(
            java.lang.management.ManagementFactory.getGarbageCollectorMXBeans
                .toArray
                .map { o =>
                    val g = o.asInstanceOf[java.lang.management.GarbageCollectorMXBean]
                    (g.getName, g.getCollectionCount, g.getCollectionTime / 1000.0f)
                }.toList)
}

object Utils {
    def timeit(times : Int)(f : => Unit) {
        if (times > 1) f
        System.gc
        val start = System.nanoTime
        val gcStatus = GC.status
        for (_ <- 0 until times) f
        println((System.nanoTime - start) / 1000000000.0 / times, GC.status - gcStatus)
    }
}
