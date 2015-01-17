package parsing

trait IGrammarExpr[+T] {
  outer =>

  type Action = List[Any] => List[Any]
  def eval() : List[(List[IGrammarSymbol], Action)]

  def ^^[R](action : T => R) : IGrammarExpr[R] = new IGrammarExpr[R] {
    def eval() = outer.eval().map(p => (p._1, { stack =>
      val stack2 = p._2(stack)
      action(stack2.head.asInstanceOf[T]) :: stack2.tail
    } : Action))
  }

  def as[U] : IGrammarExpr[U] = new IGrammarExpr[U] {
    def eval() : List[(List[IGrammarSymbol], Action)] = outer.eval()
  }

  def |[U >: T](other : IGrammarExpr[U]) : IGrammarExpr[U] = new IGrammarExpr[U] {
    def eval() = outer.eval() ::: other.eval()
  }

  def ~[U](other : IGrammarExpr[U]) : IGrammarExpr[T ~ U] = concat[U, T ~ U](other, (a, b) => new ~(a, b))
  def <~[U](other : IGrammarExpr[U]) : IGrammarExpr[T] = concat[U, T](other, (a, b) => a)
  def ~>[U](other : IGrammarExpr[U]) : IGrammarExpr[U] = concat[U, U](other, (a, b) => b)

  private def concat[U, R](other : IGrammarExpr[U], f : (T, U) => R) : IGrammarExpr[R] = new IGrammarExpr[R] {
    def eval() = for (i <- outer.eval();
                      j <- other.eval()) yield (
      i._1 ::: j._1, {
      stack =>
        val stack2 = j._2(stack)
        val b = stack2.head.asInstanceOf[U]
        val stack3 = i._2(stack2.tail)
        val a = stack3.head.asInstanceOf[T]
        f(a, b) :: stack3.tail
    } : Action)
  }
}