import scala.util.Random
import scala.collection.mutable
import scala.collection.immutable

object Helper {
  val kMaxPrimeValue = 1024 * 1024 * 2
  private val mPrimeBuffer : Array[Byte] = {
    val a = Array.fill(kMaxPrimeValue)(1 : Byte)
    a(0) = 0
    a(1) = 0
    var i = 2
    while (i < kMaxPrimeValue) {
      if (a(i) != 0) {
        var j = i + i
        while (j < kMaxPrimeValue) {
          a(j) = 0
          j += i
        }
      }
      i += 1
    }
    a
  }
  def isPrime(n : Int) : Boolean = {
    if (n < kMaxPrimeValue) mPrimeBuffer(n) == 1
    else BigInt(n).isProbablePrime(20)
  }
  val primes : Stream[Int] = Stream.from(2) filter isPrime

  def group[T](a : List[T]) : List[(T, Int)] = {
    def iter(x : T, count : Int, rest : List[T]) : List[(T, Int)] = {
      rest match {
        case Nil => List((x, count))
        case head :: tail if head == x => iter(x, count + 1, tail)
        case head :: tail => (x, count) :: iter(head, 1, tail)
      }
    }
    if (a.isEmpty) Nil
    else iter(a.head, 1, a.tail)
  }

  def toPrimeFactors(a : Long) : List[Int] = {
    def iter(primes : Stream[Int], rest : Long) : List[Int] = {
      val prime = primes.head
      if (rest == 1) Nil
      else if (prime >= rest) List(rest.toInt)
      else if (rest % prime == 0) prime :: iter(primes, rest / prime) else iter(primes.tail, rest)
    }
    iter(primes, a)
  }
  def toGroupedPrimeFactors(a : Long) = group(toPrimeFactors(a))

  def countDivisor(n : Long) : Int = {
    val g = toGroupedPrimeFactors(n)
    return g.map(_._2 + 1).product
  }
  def divisors(n : Long) : List[Int] = {
    def gen(primeFactors : List[(Int, Int)]) : List[Int] = {
      primeFactors match {
        case Nil => List(1)
        case (prime, count) :: tail => {
          val rest = gen(tail)
          for (
            v <- List.iterate(1, count + 1)(_ * prime);
            r <- rest
          ) yield v * r
        }
      }
    }
    gen(toGroupedPrimeFactors(n))
  }

  def isPalindrome(a : String) : Boolean = a == a.reverse

  def mergeStream[T](a : Stream[T], b : Stream[T], cmp : ((T, T), (T, T)) => Boolean) : Stream[(T, T)] = {
    def merge(a : Stream[(T, T)], b : Stream[(T, T)]) : Stream[(T, T)] = {
      if (a.isEmpty) b
      else if (b.isEmpty) a
      else {
        if (cmp(a.head, b.head)) a.head #:: merge(a.tail, b)
        else b.head #:: merge(a, b.tail)
      }
    }
    (a.head, b.head) #:: merge(
      mergeStream(a.tail, b.tail, cmp),
      merge(b.tail.map((a.head, _)), a.tail.map((_, b.head))))
  }

  def gcd(a : Int, b : Int) : Int = {
    if (a == 0) b
    else gcd(b % a, a)
  }
  def lcm(a : Int, b : Int) : Int = a / gcd(a, b) * b

  def rotateLeft[T](a : Seq[T], n : Int) : Seq[T] = {
    val (left, right) = a.splitAt(n)
    right ++ left
  }
  def rotateRight[T](a : Seq[T], n : Int) : Seq[T] = {
    val (left, right) = a.splitAt(a.length - n)
    right ++ left
  }

  def transpose[T](m : Seq[Seq[T]]) : Seq[Seq[T]] = {
    m match {
      case Nil +: _ => Nil
      case _ => m.map(_.head) +: transpose(m.map(_.tail))
    }
  }

  def memoize[In, Out](f : In => Out) : In => Out = {
    val m = mutable.Map[In, Out]()
    v => m.getOrElseUpdate(v, f(v))
  }

  def factorial(n : Int) : BigInt = n match {
    case x if x < 2 => 1
    case _ => n * factorial(n - 1)
  }

  lazy val fibs : Stream[Int] = 1 #:: 1 #:: fibs.zip(fibs.drop(1)).map { case (i, j) => i + j }
  lazy val fibs2 : Stream[BigInt] = 1 #:: 1 #:: fibs2.zip(fibs2.drop(1)).map { case (i, j) => i + j }

  def reverse[T](a : Array[T], begin : Int, end : Int) {
    var i = begin
    var j = end - 1
    while (i < j) {
      { val t = a(i); a(i) = a(j); a(j) = t }
      i += 1
      j -= 1
    }
  }

  def longToArray(a : Array[Int], n : Long) : Int = {
    var len = 0
    var tn = n
    while (tn > 0) {
      a(len) = (tn % 10).toInt
      len += 1
      tn /= 10
    }
    reverse(a, 0, len)
    len
  }

  def arrayToLong(a : Array[Int], begin : Int, end : Int, step : Int = 1) : Long = {
    var i = begin
    var sum = 0L
    while (i != end) {
      sum = sum * 10 + a(i)
      i += step
    }
    sum
  }

  def permutation[T](a : Array[T], f : Array[T] => Unit) {
    def iterate(i : Int) {
      if (i == a.length) f(a)
      else {
        for (j <- (i until a.length)) {
          { val t = a(i); a(i) = a(j); a(j) = t }
          iterate(i + 1);
          { val t = a(i); a(i) = a(j); a(j) = t }
        }
      }
    }
    iterate(0)
  }

  def combination[T](a : Array[T], n : Int, f : Array[T] => Unit) {
    val tempArray = a.clone
    def iterate(ia : Int, itemp : Int) {
      if (itemp == n) f(tempArray)
      else if (ia == a.length) {}
      else {
        tempArray(itemp) = a(ia)
        iterate(ia + 1, itemp + 1)
        iterate(ia + 1, itemp)
      }
    }
    iterate(0, 0)
  }
}

object Test extends App {
  def puzzle_1() : Int = {
    (1 until 1000) filter (v => v % 3 == 0 || v % 5 == 0) sum
  }
  def puzzle_2() : Int = {
    Helper.fibs takeWhile (_ < 4000000) filter (_ % 2 == 0) sum
  }
  def puzzle_3() : Int = {
    Helper.toPrimeFactors(600851475143L).last
  }
  def puzzle_4() : Int = {
    val s = Stream.range(999, 100, -1)
    (Helper.mergeStream[Int](s, s, (a, b) => a._1 * a._2 > b._1 * b._2) map { case (i, j) => (i * j).toString } filter Helper.isPalindrome head) toInt
  }
  def puzzle_5() : Int = {
    (1 to 20).fold(1)(Helper.lcm(_, _))
  }
  def puzzle_6() : Int = {
    val a = (0 to 100) sum
    val b = (0 to 100) map (v => v * v) sum;
    a * a - b
  }
  def puzzle_7() : Int = {
    Helper.primes(10000)
  }
  def puzzle_8() : Long = {
    val s = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    s.sliding(13) map (s => s.map(c => (c - '0').toLong).product) max
  }
  def puzzle_9() : Int = {
    (for (
      c <- Stream.range(500, 333, -1);
      a <- Stream.range(333, 1, -1);
      b = 1000 - a - c;
      if b > a && b < c;
      if a * a + b * b == c * c
    ) yield a * b * c).head
  }
  def puzzle_10() : Long = {
    Helper.primes.takeWhile(_ < 2000000).foldLeft(0L)(_ + _)
  }
  def puzzle_11() : Int = {
    val s = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48"
    val m = s.split(" ").map(_.toInt).toList.grouped(20).toList
    val m2 = Helper.transpose(m)
    val m3 = Helper.transpose(m.zipWithIndex.map { case (r, i) => Helper.rotateLeft(r, i) })
    val m4 = Helper.transpose(m.zipWithIndex.map { case (r, i) => Helper.rotateRight(r, i) })
    List(m.map(r => r.sliding(4).map(_.product).max).max,
      m2.map(r => r.sliding(4).map(_.product).max).max,
      m3.map(r => r.sliding(4).map(_.product).max).max,
      m4.map(r => r.sliding(4).map(_.product).max).max).max
  }
  def puzzle_12() : Int = {
    lazy val triNums : Stream[Int] = 1 #:: Stream.from(2).zip(triNums).map { case (i, j) => i + j }
    (triNums map (v => (v, Helper.countDivisor(v))) dropWhile (_._2 < 500) head)._1
  }
  def puzzle_13() : String = {
    val s = "37107287533902102798797998220837590246510135740250 46376937677490009712648124896970078050417018260538 74324986199524741059474233309513058123726617309629 91942213363574161572522430563301811072406154908250 23067588207539346171171980310421047513778063246676 89261670696623633820136378418383684178734361726757 28112879812849979408065481931592621691275889832738 44274228917432520321923589422876796487670272189318 47451445736001306439091167216856844588711603153276 70386486105843025439939619828917593665686757934951 62176457141856560629502157223196586755079324193331 64906352462741904929101432445813822663347944758178 92575867718337217661963751590579239728245598838407 58203565325359399008402633568948830189458628227828 80181199384826282014278194139940567587151170094390 35398664372827112653829987240784473053190104293586 86515506006295864861532075273371959191420517255829 71693888707715466499115593487603532921714970056938 54370070576826684624621495650076471787294438377604 53282654108756828443191190634694037855217779295145 36123272525000296071075082563815656710885258350721 45876576172410976447339110607218265236877223636045 17423706905851860660448207621209813287860733969412 81142660418086830619328460811191061556940512689692 51934325451728388641918047049293215058642563049483 62467221648435076201727918039944693004732956340691 15732444386908125794514089057706229429197107928209 55037687525678773091862540744969844508330393682126 18336384825330154686196124348767681297534375946515 80386287592878490201521685554828717201219257766954 78182833757993103614740356856449095527097864797581 16726320100436897842553539920931837441497806860984 48403098129077791799088218795327364475675590848030 87086987551392711854517078544161852424320693150332 59959406895756536782107074926966537676326235447210 69793950679652694742597709739166693763042633987085 41052684708299085211399427365734116182760315001271 65378607361501080857009149939512557028198746004375 35829035317434717326932123578154982629742552737307 94953759765105305946966067683156574377167401875275 88902802571733229619176668713819931811048770190271 25267680276078003013678680992525463401061632866526 36270218540497705585629946580636237993140746255962 24074486908231174977792365466257246923322810917141 91430288197103288597806669760892938638285025333403 34413065578016127815921815005561868836468420090470 23053081172816430487623791969842487255036638784583 11487696932154902810424020138335124462181441773470 63783299490636259666498587618221225225512486764533 67720186971698544312419572409913959008952310058822 95548255300263520781532296796249481641953868218774 76085327132285723110424803456124867697064507995236 37774242535411291684276865538926205024910326572967 23701913275725675285653248258265463092207058596522 29798860272258331913126375147341994889534765745501 18495701454879288984856827726077713721403798879715 38298203783031473527721580348144513491373226651381 34829543829199918180278916522431027392251122869539 40957953066405232632538044100059654939159879593635 29746152185502371307642255121183693803580388584903 41698116222072977186158236678424689157993532961922 62467957194401269043877107275048102390895523597457 23189706772547915061505504953922979530901129967519 86188088225875314529584099251203829009407770775672 11306739708304724483816533873502340845647058077308 82959174767140363198008187129011875491310547126581 97623331044818386269515456334926366572897563400500 42846280183517070527831839425882145521227251250327 55121603546981200581762165212827652751691296897789 32238195734329339946437501907836945765883352399886 75506164965184775180738168837861091527357929701337 62177842752192623401942399639168044983993173312731 32924185707147349566916674687634660915035914677504 99518671430235219628894890102423325116913619626622 73267460800591547471830798392868535206946944540724 76841822524674417161514036427982273348055556214818 97142617910342598647204516893989422179826088076852 87783646182799346313767754307809363333018982642090 10848802521674670883215120185883543223812876952786 71329612474782464538636993009049310363619763878039 62184073572399794223406235393808339651327408011116 66627891981488087797941876876144230030984490851411 60661826293682836764744779239180335110989069790714 85786944089552990653640447425576083659976645795096 66024396409905389607120198219976047599490197230297 64913982680032973156037120041377903785566085089252 16730939319872750275468906903707539413042652315011 94809377245048795150954100921645863754710598436791 78639167021187492431995700641917969777599028300699 15368713711936614952811305876380278410754449733078 40789923115535562561142322423255033685442488917353 44889911501440648020369068063960672322193204149535 41503128880339536053299340368006977710650566631954 81234880673210146739058568557934581403627822703280 82616570773948327592232845941706525094512325230608 22918802058777319719839450180888072429661980811197 77158542502016545090413245809786882778948721859617 72107838435069186155435662884062257473692284509516 20849603980134001723930671666823555245252804609722 53503534226472524250874054075591789781264330331690"
    s.split(" ").map(BigInt(_)).fold(0 : BigInt)(_ + _).toString.take(10)
  }
  def puzzle_14() : Int = {
    def collatzLen(n : Long) : Int = {
      n match {
        case x if x == 1 => 1
        case x if x % 2 == 1 => collatzLen(n * 3 + 1) + 1
        case _ => collatzLen(n / 2) + 1
      }
    }
    Stream.range(1, 1000000).maxBy(v => collatzLen(v))
  }
  def puzzle_15() : Long = {
    lazy val countPath : ((Int, Int)) => Long = Helper.memoize[(Int, Int), Long](xy => {
      xy match {
        case (0, _) => 1
        case (_, 0) => 1
        case (x, y) => countPath(x - 1, y) + countPath(x, y - 1)
      }
    })
    countPath((20, 20))
  }
  def puzzle_16() : Int = {
    BigInt(2).pow(1000).toString.map(_ - '0').sum
  }
  def puzzle_18() : Int = {
    val s = "75,95 64,17 47 82,18 35 87 10,20 04 82 47 65,19 01 23 75 03 34,88 02 77 73 07 63 67,99 65 04 28 06 16 70 92,41 41 26 56 83 40 80 70 33,41 48 72 33 47 32 37 16 94 29,53 71 44 65 25 43 91 52 97 51 14,70 11 33 28 77 73 17 78 39 68 17 57,91 71 52 38 17 14 91 43 58 50 27 29 48,63 66 04 68 89 53 67 30 73 16 69 87 40 31,04 62 98 27 23 09 70 98 73 93 38 53 60 04 23,"
    val triangle = s.split(",").toList.map(_.split(" ").toList.map(_.toInt));

    def genMaxLine(triangle : List[List[Int]], maxLine : List[Int]) : List[Int] = {
      triangle match {
        case Nil => maxLine
        case triLine :: triTail => {
          val maxContent = maxLine.drop(1).zip(maxLine.dropRight(1)).map { case (i, j) => if (i < j) j else i }
          val newMaxLine = triLine.zip((maxLine.head :: maxContent) :+ maxLine.last).map { case (i, j) => i + j }
          genMaxLine(triTail, newMaxLine)
        }
      }
    }

    genMaxLine(triangle.tail, triangle.head).max
  }
  def puzzle_19() : Int = {
    import java.text.SimpleDateFormat
    import java.util.Calendar

    val c1 = Calendar.getInstance;
    c1.setTime(new SimpleDateFormat("yyyy MM dd").parse("1901 1 1"))
    val c2 = Calendar.getInstance;
    c2.setTime(new SimpleDateFormat("yyyy MM dd").parse("2000 12 31"))

    var count = 0
    while (c1.before(c2)) {
      if (c1.get(Calendar.DAY_OF_WEEK) == Calendar.SUNDAY) {
        count += 1
      }
      c1.add(Calendar.MONTH, 1)
    }
    count
  }
  def puzzle_20() : Int = {
    ((1 : BigInt) to 100).product.toString.map(_ - '0').sum
  }
  def puzzle_21() : Int = {
    def d(n : Int) = Helper.divisors(n).dropRight(1).sum
    val a = Array.tabulate(10000)(d)
    ((2 until 10000) filter (i => i != a(i) && a(i) < 10000 && a(a(i)) == i)).sum
  }
  def puzzle_22() : Long = {
    def score(word : String, weight : Int) : Int = word.map(_ - 'A' + 1).sum * weight

    val words = """\"(\w+)\"""".r.findAllMatchIn(scala.io.Source.fromFile("name.txt").mkString).map(_.group(1)).toList.sorted
    words.zipWithIndex.map { case (word, i) => score(word, i + 1) }.sum
  }
  def puzzle_23() : Int = {
    val kMax = 28123

    val a = Array.fill(kMax)(1);
    {
      var i = 2
      while (i < kMax) {
        var j = i + i
        while (j < kMax) {
          a(j) += i
          j += i
        }
        i += 1
      }
    }

    val abundantNums = ((1 until kMax) filter (i => a(i) > i)).toArray

    val validNums : mutable.Set[Int] = (for (
      i <- (0 until abundantNums.length);
      j <- (i until abundantNums.length);
      v = abundantNums(i) + abundantNums(j) if v < kMax
    ) yield v)(scala.collection.breakOut)
    val invalidNums = (1 until kMax) filter (validNums(_) == false)

    invalidNums.sum
  }
  def puzzle_24() : Long = {
    def index2Perm[T](a : List[T], index : Int) : List[T] = {
      a match {
        case List(_) => a
        case _ => {
          val base = Helper.factorial(a.length - 1).toInt
          assert(index < base * a.length)
          val i = index / base
          val (left, right) = a.splitAt(i)
          right.head :: index2Perm(left ::: right.tail, index % base)
        }
      }
    }

    new String(index2Perm(('0' to '9').toList, 1000000 - 1).toArray).toLong
  }
  def puzzle_25() : Int = {
    val num = BigInt(10).pow(1000 - 1)
    Helper.fibs2.zipWithIndex.dropWhile { case (v, i) => v < num }.head._2 + 1
  }
  def puzzle_26() : Int = {
    def loopLen(n : Int) : Option[Int] = {
      def iterate(rest : Int, n : Int, len : Int, history : Map[Int, Int]) : Option[Int] = {
        rest match {
          case _ if n > rest => iterate(rest * 10, n, len, history)
          case _ if history.contains(rest) => Some(len - history(rest))
          case _ if rest % n == 0 => None
          case _ => iterate(rest % n, n, len + 1, history + ((rest, len)))
        }
      }
      iterate(1, n, 0, Map[Int, Int]())
    }

    (2 until 1000) maxBy loopLen
  }
  def puzzle_27() : Int = {
    def primeCount(a : Int, b : Int) : Int = {
      def iterate(n : Int) : Int = {
        val v = n * n + a * n + b
        if (v > 0 && Helper.isPrime(v)) iterate(n + 1)
        else n
      }
      iterate(0)
    }

    var max = (1, 41, 40)
    for (
      b <- (2 to 999) filter Helper.isPrime;
      a <- (-999 to 999);
      count = primeCount(a, b);
      if count > max._3
    ) {
      max = (a, b, count)
    }

    max._1 * max._2
  }
  def puzzle_28() : Int = {
    var sum = 1
    var step = 2
    var i = 1
    while (i < 1001 * 1001) {
      for (j <- 0 until 4) {
        i += step
        sum += i
      }
      step += 2
    }
    sum
  }
  def puzzle_29() : Int = {
    val s : Set[BigInt] = (for (
      a <- 2 to 100;
      b <- 2 to 100
    ) yield BigInt(a).pow(b))(scala.collection.breakOut)
    s.size
  }
  def puzzle_30() : Int = {
    def valid(n : Int, e : Int) : Boolean = {
      var sumE = 0.0
      var a = n
      while (a > 0) {
        sumE += math.pow(a % 10, e)
        a /= 10
      }
      n == sumE.toInt
    }
    def maxLen(e : Int, len : Int = 1) : Int = {
      if (math.pow(9, e) * len >= math.pow(10, len - 1)) maxLen(e, len + 1)
      else len - 1
    }

    val e = 5
    val max = (math.pow(9, e) * maxLen(e)).toInt
    (2 to max) filter (n => valid(n, e)) sum
  }
  def puzzle_31() : Int = {
    def solve(coins : List[Int], target : Int) : Int = {
      if (target == 0) 1
      else if (coins.isEmpty) 0
      else if (coins.head > target) solve(coins.tail, target)
      else solve(coins, target - coins.head) + solve(coins.tail, target)
    }
    solve(List(200, 100, 50, 20, 10, 5, 2, 1), 200)
  }
  def puzzle_32() : Int = {
    def toInt(a : Array[Int], begin : Int, end : Int) : Int = {
      if (begin == end) 0
      else toInt(a, begin, end - 1) * 10 + a(end - 1)
    }

    var result = Set[Int]()
    Helper.permutation[Int]((1 to 9).toArray, a => {
      val n = toInt(a, 0, 4)
      val n2 = toInt(a, 4, 5)
      val n3 = toInt(a, 5, 9)
      if (n2 * n3 == n) result += n
      else {
        val n2 = toInt(a, 4, 6)
        val n3 = toInt(a, 6, 9)
        if (n2 * n3 == n) result += n
      }
    })
    result.sum
  }
  def puzzle_33() : Int = {
    val nums = for (
      a <- (10 to 99);
      b <- (a + 1 to 99);
      if b / 10 == a % 10;
      if b % 10 * a == a / 10 * b
    ) yield (a, b)
    val as = nums.map(_._1).product
    val bs = nums.map(_._2).product
    bs / Helper.gcd(as, bs)
  }
  def puzzle_34() : Int = {
    def maxLen(len : Int = 1) : Int = {
      if (Helper.factorial(9).toInt * len > math.pow(10, len - 1)) maxLen(len + 1)
      else len - 1
    }
    val facTable = Array.tabulate(10)(i => Helper.factorial(i).toInt)
    def valid(n : Int) : Boolean = {
      var sum = 0
      var tn = n
      while (tn > 0) {
        sum += facTable(tn % 10)
        tn /= 10
      }
      sum == n
    }

    val max = Helper.factorial(9).toInt * maxLen()
    (3 until max) filter valid sum
  }
  def puzzle_35() : Int = {
    def loopArrayToInt(a : Array[Int], off : Int, len : Int) : Int = {
      var sum = 0
      var i = 0
      while (i < len) {
        val j = (off + i) % len
        sum = sum * 10 + a(j)
        i += 1
      }
      sum
    }

    var tempArray = Array.fill(10)(0)
    def valid(n : Int) : Boolean = {
      if (!Helper.isPrime(n)) false
      else {
        val len = Helper.longToArray(tempArray, n)
        var off = 0
        while (off < len) {
          val tn = loopArrayToInt(tempArray, off, len)
          if (!Helper.isPrime(tn)) return false
          off += 1
        }
        true
      }
    }

    (2 until 1000000) filter valid length
  }
  def puzzle_36() : Int = {
    def valid(n : Int) : Boolean = {
      val os = n.toString
      if (os != os.reverse) return false
      val bs = java.lang.Integer.toBinaryString(n)
      return bs == bs.reverse
    }
    (1 until 1000000) filter valid sum
  }
  def puzzle_37() : Int = {
    var tempArray = Array.fill(10)(0)
    def valid(n : Int) : Boolean = {
      if (!Helper.isPrime(n)) return false

      val len = Helper.longToArray(tempArray, n)
      for (begin <- (0 until len)) {
        val tn = Helper.arrayToLong(tempArray, begin, len, 1).toInt
        if (!Helper.isPrime(tn)) return false
      }
      for (end <- (1 to len)) {
        val tn = Helper.arrayToLong(tempArray, 0, end, 1).toInt
        if (!Helper.isPrime(tn)) return false
      }
      true
    }

    Helper.primes.dropWhile(_ < 10).filter(valid).take(11).sum
  }
  def puzzle_38() : Int = {
    def valid(s : String) : Boolean = {
      val set = mutable.BitSet(s.map(_ - '0') : _*)
      set.size == 9 && !set(0)
    }
    (9000 until 10000) map (n => n.toString + n * 2) filter valid map (_.toInt) max
  }
  def puzzle_39() : Int = {
    val m = mutable.Map[Int, Int]()
    for (
      c <- (2 until 500);
      b <- (2 until c - 1);
      aa = c * c - b * b;
      if aa < b * b;
      a = math.sqrt(aa).toInt;
      if a * a == aa;
      p = a + b + c if p < 1000
    ) {
      m(p) = 1 + m.getOrElse(p, 0)
    }
    m.maxBy(_._2)._1
  }
  def puzzle_40() : Int = {
    def d(n : Int) : Char = {
      def getIndexAndLen(i : Int, len : Int = 1) : (Int, Int) = {
        val totalDigit = (math.pow(10, len) - math.pow(10, len - 1)).toInt * len
        if (i >= totalDigit) getIndexAndLen(i - totalDigit, len + 1) else (i, len)
      }
      val (i, len) = getIndexAndLen(n)
      val numIdx = i / len
      val digitIdx = i % len
      (math.pow(10, len - 1) + numIdx).toString()(digitIdx)
    }

    (0 to 6) map (v => d(math.pow(10, v).toInt - 1) - '0') product
  }
  def puzzle_41() : Int = {
    var max = 0

    import scala.util.control.Breaks
    val label = new Breaks
    label.breakable {
      for (end <- ('2' to '9').reverse) {
        Helper.permutation[Char](('1' to end).reverse.toArray, a => {
          val n = new String(a).toInt
          if (Helper.isPrime(n)) {
            max = n
            label.break
          }
        })
      }
    }

    max
  }
  def puzzle_42() : Int = {
    val nums = Stream.from(1).map(v => v * (v + 1) / 2)
    def valid(word : String) : Boolean = {
      val n = word.map(_ - 'A' + 1).sum
      nums.dropWhile(_ < n).head == n
    }

    val words = """\"(\w+)\"""".r.findAllMatchIn(scala.io.Source.fromFile("words.txt").mkString).map(_.group(1))
    words filter valid length
  }
  def puzzle_43() : Long = {
    def valid(a : Array[Int]) : Boolean = {
      def iterate(i : Int, primes : Stream[Int]) : Boolean = {
        if (i == 8) true
        else if (Helper.arrayToLong(a, i, i + 3) % primes.head == 0) iterate(i + 1, primes.tail)
        else false
      }
      iterate(1, Helper.primes)
    }

    var nums = List[Long]()
    Helper.permutation[Int]((0 to 9).toArray, a => {
      if (valid(a)) nums = Helper.arrayToLong(a, 0, a.length, 1) :: nums
    })
    nums.sum
  }
  def puzzle_44() : Int = {
    def valid(r : Long) : Boolean = {
      val sqrtbbp4ac = math.sqrt(1 + 24 * r)
      ((1 + sqrtbbp4ac) / 6).isValidInt
    }

    val nums = Stream.range(1L, 2000L).map(v => v * (3 * v - 1) / 2)
    (for (
      d <- nums;
      j <- nums;
      k = d + j;
      if valid(k) && valid(j + k)
    ) yield d).head.toInt
  }
  def puzzle_45() : Long = {
    def isTriangle(n : Long) : Boolean = {
      val sqrtbbp4ac = math.sqrt(1 + 8 * n)
      ((-1 + sqrtbbp4ac) / 2).isValidInt
    }
    def isPentagonal(n : Long) : Boolean = {
      val sqrtbbp4ac = math.sqrt(1 + 24 * n)
      ((1 + sqrtbbp4ac) / 6).isValidInt
    }
    Stream.from(144).map(v => v * (2 * v - 1)).filter(v => isTriangle(v) && isPentagonal(v)).head
  }
  def puzzle_46() : Int = {
    def isOddComposite(n : Int) : Boolean = {
      n % 2 == 1 && Helper.isPrime(n)
    }
    def diff(s1 : Stream[Int], s2 : Stream[Int]) : Stream[Int] = {
      if (s1.head == s2.head) diff(s1.tail, s2.tail)
      else if (s1.head < s2.head) s1.head #:: diff(s1.tail, s2)
      else diff(s1, s2.tail)
    }

    val validNums = Helper.mergeStream[Int](Stream.from(1).map(v => v * v * 2), Helper.primes,
      (a, b) => a._1 + a._2 < b._1 + b._2).map(v => v._1 + v._2)
    diff(Stream.from(3).filter(v => v % 2 == 1 && !Helper.isPrime(v)), validNums).head
  }
  def puzzle_47() : Int = {
    val counts = Array.fill(140000)(0)
    for (
      p <- Helper.primes.takeWhile(_ < counts.length);
      i <- (p + p until counts.length by p)
    ) {
      counts(i) += 1
    }

    Stream.from(1).sliding(4).filter(l => l.forall(i => counts(i) == 4)).next()(0)
  }
  def puzzle_48() : String = {
    val s = ((1 to 1000) map (i => BigInt(i).pow(i)) sum).toString
    s.substring(s.length - 10)
  }
  def puzzle_49() : String = {
    val m = Helper.primes.dropWhile(_ < 1000).takeWhile(_ < 10000).toList.groupBy(v => v.toString.sorted)
    (for (
      (_, nums) <- m;
      a <- nums;
      b <- nums if a < b;
      c <- nums if b < c && b - a == c - b
    ) yield a.toString + b + c).head
  }
  def puzzle_50() : Int = {
    val validPrimes = Helper.primes.takeWhile(_ < 50000)
    lazy val primeSums : Stream[Int] = 0 #:: primeSums.zip(validPrimes).map { case (i, j) => i + j }
    val sumTable = primeSums.toArray

    def findWithLength(len : Int) : Int = {
      def iterate(i : Int) : Int = {
        if (i + len >= sumTable.length) 0
        else {
          val n = sumTable(i + len) - sumTable(i)
          if (n < 1000000 && Helper.isPrime(n)) n else iterate(i + 1)
        }
      }
      iterate(0)
    }

    Stream.range(validPrimes.length, 22, -1).map(findWithLength).dropWhile(_ == 0).head
  }

  Utils.timeit("1-15", 1) {
    assert(puzzle_1() == 233168)
    assert(puzzle_2() == 4613732)
    assert(puzzle_3() == 6857)
    assert(puzzle_4() == 906609)
    assert(puzzle_5() == 232792560)
    assert(puzzle_6() == 25164150)
    assert(puzzle_7() == 104743)
    assert(puzzle_8() == 23514624000L)
    assert(puzzle_9() == 31875000)
    assert(puzzle_10() == 142913828922L)
    assert(puzzle_11() == 70600674)
    assert(puzzle_12() == 76576500)
    assert(puzzle_13() == "5537376230")
    assert(puzzle_14() == 837799)
    assert(puzzle_15() == 137846528820L)
  }
  Utils.timeit("16-30", 1) {
    assert(puzzle_16() == 1366)
    assert(puzzle_18() == 1074)
    assert(puzzle_19() == 171)
    assert(puzzle_20() == 648)
    assert(puzzle_21() == 31626)
    assert(puzzle_22() == 871198282L)
    assert(puzzle_23() == 4179871)
    assert(puzzle_24() == 2783915460L)
    assert(puzzle_25() == 4782)
    assert(puzzle_26() == 983)
    assert(puzzle_27() == -59231)
    assert(puzzle_28() == 669171001)
    assert(puzzle_29() == 9183)
    assert(puzzle_30() == 443839)
  }

  Utils.timeit("31-50", 1) {
    assert(puzzle_31() == 73682)
    assert(puzzle_32() == 45228)
    assert(puzzle_33() == 100)
    assert(puzzle_34() == 40730)
    assert(puzzle_35() == 55)
    assert(puzzle_36() == 872187)
    assert(puzzle_37() == 748317)
    assert(puzzle_38() == 932718654)
    assert(puzzle_39() == 840)
    assert(puzzle_40() == 210)
    assert(puzzle_41() == 7652413)
    assert(puzzle_42() == 162)
    assert(puzzle_43() == 16695334890L)
    assert(puzzle_44() == 5482660)
    assert(puzzle_45() == 1533776805L)
    assert(puzzle_46() == 5777)
    assert(puzzle_47() == 134043)
    assert(puzzle_48() == "9110846700")
    assert(puzzle_49() == "296962999629")
    assert(puzzle_50() == 997651)
  }

  println("pass!")
}
