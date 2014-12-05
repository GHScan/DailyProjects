import scala.util.Random

object Test extends App {
    def main() {
        val kLen = 1024 * 1024
        val kTimes = 3

        val randList = List.fill(kLen) { Random.nextInt }
        Utils.timeit(kTimes) {
            randList.sorted
        }
        Utils.timeit(kTimes) {
            qsort_g(randList)
        }
        Utils.timeit(kTimes) {
            qsort(randList)(_ < _)
        }

        val randArrays = List.fill(kTimes + 1) { Array.fill(kLen)(Random.nextInt) }
        var tempArrays = randArrays.map(a => a.clone).iterator
        Utils.timeit(kTimes) {
            java.util.Arrays.sort(tempArrays.next)
        }
        tempArrays = randArrays.map(a => a.clone).iterator
        Utils.timeit(kTimes) {
            val a = tempArrays.next
            qsort1(a, 0, a.length)
        }
        tempArrays = randArrays.map(a => a.clone).iterator
        Utils.timeit(kTimes) {
            val a = tempArrays.next
            qsort2[Int](a, 0, a.length, _ < _)
        }
        tempArrays = randArrays.map(a => a.clone).iterator
        Utils.timeit(kTimes) {
            val a = tempArrays.next
            qsort3[Int](a, 0, a.length, _ < _)
        }
    }

    main()

    def qsort_g[T](a : Traversable[T])(implicit ord : Ordering[T]) : Traversable[T] = {
        import ord._
        a match {
            case Nil => Nil
            case head :: tail => qsort_g(tail.filter(_ < head)) ++: List(head) ++: qsort_g(tail.filter(_ > head))
        }
    }
    def qsort[T](a : List[T])(implicit f : (T, T) => Boolean) : List[T] = {
        a match {
            case Nil => Nil
            case head :: tail => qsort(tail.filter(f(_, head))) ::: head :: qsort(tail.filter(f(head, _)))
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
}
