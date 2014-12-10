import scala.util.Random
import scala.collection.mutable

object Test extends App {
  def main() {
    val kLen = 1024 * 1024
    val kTimes = 3

    val randList = List.fill(kLen) { Random.nextInt }
    Utils.timeit("List.sorted", kTimes) {
      randList.sorted
    }
    Utils.timeit("qsort_g", kTimes) {
      qsort_g(randList)
    }
    Utils.timeit("qsort", kTimes) {
      qsort[Int](randList, _ < _)
    }
    Utils.timeit("mergeSort", kTimes) {
      mergeSort[Int](randList, _ < _)
    }

    val randArrays = List.fill(kTimes + 1) { Array.fill(kLen)(Random.nextInt) }
    var tempArrays = randArrays.map(a => a.clone).iterator
    Utils.timeit("java.util.Arrays.sort", kTimes) {
      java.util.Arrays.sort(tempArrays.next)
    }
    tempArrays = randArrays.map(a => a.clone).iterator
    Utils.timeit("qsort1", kTimes) {
      val a = tempArrays.next
      qsort1(a, 0, a.length)
    }
    tempArrays = randArrays.map(a => a.clone).iterator
    Utils.timeit("qsort2", kTimes) {
      val a = tempArrays.next
      qsort2[Int](a, 0, a.length, _ < _)
    }
    tempArrays = randArrays.map(a => a.clone).iterator
    Utils.timeit("qsort2_g", kTimes) {
      val a = tempArrays.next
      qsort2_g(a, 0, a.length)
    }
    tempArrays = randArrays.map(a => a.clone).iterator
    Utils.timeit("qsort3", kTimes) {
      val a = tempArrays.next
      qsort3[Int](a, 0, a.length, _ < _)
    }
    tempArrays = randArrays.map(a => a.clone).iterator
    Utils.timeit("qsort3_g", kTimes) {
      val a = tempArrays.next
      qsort3_g(a, 0, a.length)
    }
    tempArrays = randArrays.map(a => a.clone).iterator
    Utils.timeit("mergeSort_2", kTimes) {
      val a = tempArrays.next
      mergeSort_2[Int](a, _ < _)
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
  def qsort[T](a : List[T], f : (T, T) => Boolean) : List[T] = {
    a match {
      case Nil => Nil
      case head :: tail => qsort(tail.filter(f(_, head)), f) ::: head :: qsort(tail.filter(f(head, _)), f)
    }
  }
  def mergeSort[T](a : List[T], f : (T, T) => Boolean) : List[T] = {
    def merge(a : List[T], b : List[T], result : List[T]) : List[T] = {
      if (a.isEmpty) result.reverse ::: b
      else if (b.isEmpty) result.reverse ::: a
      else if (f(a.head, b.head)) merge(a.tail, b, a.head :: result)
      else merge(a, b.tail, b.head :: result)
    }
    def sort(a : List[T], len : Int) : List[T] = {
      if (len < 2) return a
      val half = len / 2
      val (left, right) = a.splitAt(half)
      merge(sort(left, half), sort(right, len - half), Nil)
    }
    sort(a, a.length)
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
  def qsort2_g[T](a : mutable.Seq[T], begin : Int, end : Int)(implicit ord : Ordering[T]) {
    import ord._

    if (end - begin <= 1) return

    val head = a(begin)
    var p = begin
    var i = begin + 1
    while (i < end) {
      if (a(i) < head) {
        p += 1
        swap_g(a, i, p)
      }
      i += 1
    }
    swap_g(a, begin, p)

    qsort2_g(a, begin, p)
    qsort2_g(a, p + 1, end)
  }
  @inline def swap[@specialized(Int) T](a : Array[T], i : Int, j : Int) {
    val t = a(i)
    a(i) = a(j)
    a(j) = t
  }
  @inline def swap_g[T](a : mutable.Seq[T], i : Int, j : Int) {
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
  def qsort3_g[T](a : mutable.Seq[T], begin : Int, end : Int)(implicit ord : Ordering[T]) {
    import ord._

    (end - begin) match {
      case 0 => return
      case 1 => return
      case 2 => {
        swapIfLess_g(a, begin + 1, begin)
        return
      }
      case 3 => {
        swapIfLess_g(a, begin + 1, begin)
        swapIfLess_g(a, begin + 2, begin)
        swapIfLess_g(a, begin + 2, begin + 1)
        return
      }
      case _ =>
    }

    swapIfLess_g(a, end - 1, begin)

    val head = a(begin)
    var lo = begin + 1
    var hi = end - 2
    while (lo <= hi) {
      while (head < a(hi)) hi -= 1
      while (a(lo) < head) lo += 1
      if (lo <= hi) {
        swap_g(a, lo, hi)
        lo += 1
        hi -= 1
      }
    }

    lo -= 1
    swap_g(a, begin, lo)
    qsort3_g(a, begin, lo)
    qsort3_g(a, lo + 1, end)
  }
  @inline def swapIfLess[@specialized(Int) T](a : Array[T], i : Int, j : Int, cmp : (T, T) => Boolean) {
    val iv = a(i)
    val jv = a(j)
    if (cmp(iv, jv)) {
      a(j) = iv
      a(i) = jv
    }
  }
  @inline def swapIfLess_g[T](a : mutable.Seq[T], i : Int, j : Int)(implicit ord : Ordering[T]) {
    import ord._
    val iv = a(i)
    val jv = a(j)
    if (iv < jv) {
      a(j) = iv
      a(i) = jv
    }
  }

  def mergeSort_2[@specialized(Int) T : Manifest](a : Array[T], f : (T, T) => Boolean) {
    def merge(
      to : Array[T], toBegin : Int,
      from1 : Array[T], from1Begin : Int, len1 : Int,
      from2 : Array[T], from2Begin : Int, len2 : Int) {
      if (len1 == 0) {
        var i = 0
        while (i < len2) {
          to(toBegin + i) = from2(from2Begin + i)
          i += 1
        }
      } else if (len2 == 0) {
        var i = 0
        while (i < len1) {
          to(toBegin + i) = from1(from1Begin + i)
          i += 1
        }
      } else {
        if (f(from1(from1Begin), from2(from2Begin))) {
          to(toBegin) = from1(from1Begin)
          merge(to, toBegin + 1, from1, from1Begin + 1, len1 - 1, from2, from2Begin, len2)
        } else {
          to(toBegin) = from2(from2Begin)
          merge(to, toBegin + 1, from1, from1Begin, len1, from2, from2Begin + 1, len2 - 1)
        }
      }
    }

    def sort(a : Array[T], begin : Int, len : Int, temp : Array[T], tempBegin : Int) {
      if (len < 2) return

      val half = len / 2
      val mid = begin + half
      sortTo(a, begin, half, temp, tempBegin)
      sortTo(a, mid, len - half, temp, tempBegin + half)
      merge(a, begin, temp, tempBegin, half, temp, tempBegin + half, len - half)
    }
    def sortTo(a : Array[T], begin : Int, len : Int, temp : Array[T], tempBegin : Int) {
      if (len < 2) {
        if (len == 1) {
          temp(tempBegin) = a(begin)
        }
        return
      }

      val half = len / 2
      val mid = begin + half
      sort(a, begin, half, temp, tempBegin)
      sort(a, mid, len - half, temp, tempBegin)
      merge(temp, tempBegin, a, begin, half, a, mid, len - half)
    }

    val temp = new Array[T](a.length)
    sort(a, 0, a.length, temp, 0)
  }
}
