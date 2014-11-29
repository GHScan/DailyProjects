import scala.util.Random

object Test extends App {
		override def main(args : Array[String]) {
		val len = 1024 * 1024

		val randList = List.fill(len)(Random.nextInt)
		timeit(1, () => {
			randList.sorted
		})
		timeit(1, () => {
			qsort[Int](randList, (i, j) => i - j)
		})

		val randArray = Array.fill(len)(Random.nextInt)
		var tempArray = randArray.clone
		timeit(1, () => {
			java.util.Arrays.sort(tempArray)
		})
		tempArray = randArray.clone
		timeit(1, () => {
			qsort1(tempArray, 0, tempArray.length)
		})
		tempArray = randArray.clone
		timeit(1, () => {
			qsort2[Int](tempArray, 0, tempArray.length, (i, j) => i < j)
		})
		tempArray = randArray.clone
		timeit(1, () => {
			qsort3[Int](tempArray, 0, tempArray.length, (i, j) => i < j)
		})
	}
	def qsort[@specialized(Int) T](a : List[T], cmp : (T, T) => Int) : List[T] = {
		a match {
			case Nil => Nil
			case _ => qsort(a.filter(v => cmp(v, a.head) < 0), cmp) ++ a.filter(v => cmp(v, a.head) == 0) ++ qsort(a.filter(v => cmp(v, a.head) > 0), cmp)
		}
	}
	def qsort1(a : Array[Int], begin : Int, end : Int) {
		if (end - begin <= 1) return

		val head = a(begin)
		var p = begin
		var i = begin + 1
		while (i < end) {
			if (a(i) < head) {
				p += 1
				val t = a(i)
				a(i) = a(p)
				a(p) = t
			}
			i += 1
		}
		a(begin) = a(p)
		a(p) = head

		qsort1(a, begin, p)
		qsort1(a, p + 1, end)
	}
	def qsort2[@specialized(Int) T](a : Array[T], begin : Int, end : Int, cmp : (T, T) => Boolean) {
		if (end - begin <= 1) return

		val head = a(begin)
		var p = begin
		var i = begin + 1
		while (i < end) {
			if (cmp(a(i), head)) {
				p += 1
				swap(a, i, p)
			}
			i += 1
		}
		swap(a, begin, p)

		qsort2(a, begin, p, cmp)
		qsort2(a, p + 1, end, cmp)
	}
	@inline def swap[@specialized(Int) T](a : Array[T], i : Int, j : Int) {
		val t = a(i)
		a(i) = a(j)
		a(j) = t
	}

	def qsort3[@specialized(Int) T](a : Array[T], begin : Int, end : Int, cmp : (T, T) => Boolean) {
		(end - begin) match {
			case 0 => return
			case 1 => return
			case 2 => {
				swapIfLess(a, begin + 1, begin, cmp)
				return
			}
			case 3 => {
				swapIfLess(a, begin + 1, begin, cmp)
				swapIfLess(a, begin + 2, begin, cmp)
				swapIfLess(a, begin + 2, begin + 1, cmp)
				return
			}
			case _ =>
		}

		swapIfLess(a, end - 1, begin, cmp)

		val head = a(begin)
		var lo = begin + 1
		var hi = end - 2
		while (lo <= hi) {
			while (cmp(head, a(hi))) hi -= 1
			while (cmp(a(lo), head)) lo += 1
			if (lo <= hi) {
				swap(a, lo, hi)
				lo += 1
				hi -= 1
			}
		}

		lo -= 1
		swap(a, begin, lo)
		qsort3(a, begin, lo, cmp)
		qsort3(a, lo + 1, end, cmp)
	}
	@inline def swapIfLess[@specialized(Int) T](a : Array[T], i : Int, j : Int, cmp : (T, T) => Boolean) {
		val iv = a(i)
		val jv = a(j)
		if (cmp(iv, jv)) {
			a(j) = iv
			a(i) = jv
		}
	}

	def timeit(times : Int, f : () => Unit) {
		if (times > 1) f()
		val start = System.nanoTime
		val gcStatus = GC.status
		for (_ <- 0 until times) f()
		println((System.nanoTime - start) / 1000000000.0 / times, GC.status - gcStatus)
	}

	class GCStatus(_status : List[(String, Long, Float)]) {
		val status = _status.filter(s=>s._2 > 0)
		def -(o : GCStatus) =
			new GCStatus(
				status.map(s =>
					o.status.find(v => v._1 == s._1) match {
						case None => s
						case Some((oname, ocount, otime)) => (oname, s._2 - ocount, s._3 - otime)
					}))
		override def toString = "GC:" + 
				status
				.map(v => f"(${v._1}%s,count=${v._2}%d,time=${v._3}%.3f)")
				.fold("")((a, b) => a + b) 
	}
	object GC {
		def status =
			new GCStatus(
				java.lang.management.ManagementFactory.getGarbageCollectorMXBeans
					.toArray
					.map(o => o.asInstanceOf[java.lang.management.GarbageCollectorMXBean])
					.map(g => (g.getName, g.getCollectionCount, g.getCollectionTime / 1000.0f))
					.toList)
	}
}
