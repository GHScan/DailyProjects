package utils

object Profiler {

  def measure(name : String, times : Int)(f : => Unit) {
    if (times > 1) f
    System.gc()
    val start = System.nanoTime
    val gcStatus = GCStatus()
    for (_ <- 0 until times) f
    println(f"$name%-32s", (System.nanoTime - start) / times.toDouble / 1000000000, GCStatus() - gcStatus)
  }

}