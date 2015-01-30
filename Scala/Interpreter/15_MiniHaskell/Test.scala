/**
factor := INTEGER
            STRING
            BOOLEAN
            "(" expr ")"
            NAME
  call := factor "(" [expr {"," expr }] ")"
  unary := { "-" | "not" } (call | factor)
  mul := unary { ("*" | "/" | "%") unary }
  add := mul { ("+" | "-") mul }
  relat := add { ("<" | "<=" | ">" | ">=" | "==" | "!=") add }
  logic := relat { ("and" | "or") relat }
  expr := logic
          "if" expr "then" expr "else" expr
          "let" stmts "in" expr
          "\" {NAME} "->" expr

  stmts := stmt { [";"]  stmt }
  stmt := "var" NAME "=" expr
          NAME { NAME } "=" expr
          NAME { NAME } guards
          call
  guards := guard { guard }
  guard := "|" logic "=" expr

  main := stmts
  */

import scala.collection.immutable

class ScopedMap[K, V](list : List[(K, V)]) extends immutable.Seq[(K, V)] {
  def get(key : K) : Option[V] = list.find(_._1 == key).map(_._2)
  def getOrElse(key : K, default : => V) : V = get(key).getOrElse(default)
  def +(kv : (K, V)) = new ScopedMap(kv :: list)
  def ++(kvs : TraversableOnce[(K, V)]) = new ScopedMap(kvs.toList ::: list)
  def updated(key : K, value : V) = new ScopedMap(list.updated(list.indexWhere(_._1 == key), (key, value)))
  override def length : Int = list.length
  override def apply(idx : Int) : (K, V) = list(idx)
  override def iterator : Iterator[(K, V)] = list.iterator
}

sealed abstract class Either[+T, +U]
case class Left[+T](value : T) extends Either[T, Nothing]
case class Right[+U](value : U) extends Either[Nothing, U]

object ErrorList {
  type ErrorList[T] = Either[List[T], String]
  implicit class ErrorListMonadOps[T](errorList : ErrorList[T]) {
    def map[R](f : T => R) : ErrorList[R] = errorList match {
      case Left(list) if list.nonEmpty => Left(list.map(f))
      case Right(error) => Right(error)
    }
    def flatMap[R](f : T => ErrorList[R]) : ErrorList[R] = errorList match {
      case Left(list) if list.nonEmpty =>
        val list2 = list.map(f)
        list2.collect { case Left(list3) if list3.nonEmpty => list3}.flatMap(identity) match {
          case Nil => list2.last
          case list3 => Left(list3)
        }
      case Right(error) => Right(error)
    }
    def filter(f : T => Boolean) : ErrorList[T] = errorList match {
      case Left(list) => list.filter(f) match {
        case Nil => Right("error by filter")
        case list2 => Left(list2)
      }
      case Right(error) => Right(error)
    }
  }
}

object TypeSystem {

  import ErrorList._

  sealed abstract class Type
  case class TyConstant(name : String) extends Type {
    override def toString = name
  }
  case class TyVar(name : String) extends Type {
    override def toString = name
  }
  case class TyBinary(name : String, first : Type, second : Type) extends Type {
    override def toString = first match {
      case _ : TyBinary => s"($first) $name $second"
      case _ => s"$first $name $second"
    }
  }
  case class TyFun(formals : List[String], body : Type) extends Type {
    override def toString = body.toString
  }
  case class TyCall(fun : Type, actuals : List[Type]) extends Type {
    override def toString = fun match {
      case TyFun(formals, body) => substitute(body, new TypeEnv(formals.zip(actuals))).toString
      case _ => s"$fun[${actuals.mkString(",")}]"
    }
  }

  def genTyVar() : TyVar = TyVar(s"T${Utils.genID()}")

  type TypeEnv = ScopedMap[String, Type]
  type ValueEnv = ScopedMap[String, List[Type]]

  def substitute(ty : Type, env : TypeEnv) : Type = {
    def iterate(ty : Type, env : TypeEnv, occurs : List[String]) : Type = ty match {
      case _ : TyConstant => ty
      case TyVar(name) if occurs.contains(name) => throw new Exception("Illegal: Recursive type")
      case TyVar(name) => env.get(name) match {
        case None => ty
        case Some(target) => iterate(target, env, name :: occurs)
      }
      case TyBinary(name, first, second) => TyBinary(name, iterate(first, env, occurs), iterate(second, env, occurs))
      case TyFun(formals, body) => TyFun(formals, iterate(body, env, occurs))
      case TyCall(fun, actuals) => TyCall(iterate(fun, env, occurs), actuals.map(a => iterate(a, env, occurs))) match {
        case TyCall(TyFun(formals, body), actuals2) => iterate(body, env ++ formals.zip(actuals2), occurs)
        case c => c
      }
    }
    iterate(ty, env, Nil)
  }

  def generalize(ty : Type, outerTypes : Set[String]) : Type = {
    var innerTypes = Set[String]()
    def collectInnerTypes(ty : Type, outerTypes : Set[String]) : Unit = ty match {
      case _ : TyConstant =>
      case TyVar(name) if outerTypes(name) =>
      case TyVar(name) => innerTypes += name
      case TyBinary(_, first, second) =>
        collectInnerTypes(first, outerTypes)
        collectInnerTypes(second, outerTypes)
      case TyFun(formals, body) =>
        collectInnerTypes(body, outerTypes ++ formals)
      case TyCall(fun, actuals) =>
        collectInnerTypes(fun, outerTypes)
        actuals.foreach { a => collectInnerTypes(a, outerTypes)}
    }
    collectInnerTypes(ty, outerTypes)
    if (innerTypes.isEmpty) ty
    else TyFun(innerTypes.toList, ty)
  }

  def instantiate(ty : Type) : Type = ty match {
    case fun : TyFun => substitute(fun.body, new TypeEnv(fun.formals.map(name => (name, genTyVar()))))
    case _ => ty
  }

  def unify(ty1 : Type, ty2 : Type, env : TypeEnv)(implicit pos : scala.util.parsing.input.Position) : ErrorList[TypeEnv] =
    (substitute(ty1, env), substitute(ty2, env)) match {
      case (TyVar(name1), TyVar(name2)) => Left(List(env + (if (name1 < name2) (name2, TyVar(name1)) else (name1, TyVar(name2)))))
      case (TyVar(name1), _ty2) => Left(List(env +(name1, _ty2)))
      case (_ty1, TyVar(name2)) => Left(List(env +(name2, _ty1)))
      case (TyConstant(name1), TyConstant(name2)) if name1 == name2 => Left(List(env))
      case (TyBinary(name1, ty11, ty12), TyBinary(name2, ty21, ty22)) if name1 == name2 =>
        for (env2 <- unify(ty11, ty21, env);
             env3 <- unify(ty12, ty22, env2)) yield env3
      case (TyFun(formals1, body1), TyFun(formals2, body2)) if formals1.length == formals2.length =>
        for (env2 <- unify(body1, substitute(body2, new TypeEnv(formals2.zip(formals1).map { case (name2, name1) => (name2, TyVar(name1))})), env))
        yield env ++ env2.dropRight(env.length).filter(p => !formals1.contains(p._1))
      case (TyCall(fun1, actuals1), TyCall(fun2, actuals2)) if actuals1.length == actuals2.length =>
        ((fun1, fun2) :: actuals1.zip(actuals2)).foldLeft(Left(List(env)) : ErrorList[TypeEnv]) {
          case (errorList, (_ty1, _ty2)) =>
            for (env2 <- errorList;
                 env3 <- unify(_ty1, _ty2, env2)) yield env3
        }
      case (ty1_, ty2_) => Right(s"Unify failed: $ty1_, $ty2_ \n${if (pos != null) pos.longString else ""}")
    }
}

object MiniHaskellAST {

  import TypeSystem.{Type, TyBinary}

  sealed abstract class Tree extends scala.util.parsing.input.Positional {
    var ty : Type = null
    override def clone() : Tree = throw new CloneNotSupportedException()
    def withType(ty : Type) : Tree = {
      val tree = clone()
      tree.ty = ty
      tree
    }
  }
  case class Constant(value : Any) extends Tree {
    override def clone() = Constant(value)
  }
  case class Var(name : String) extends Tree {
    override def clone() = Var(name)
  }
  case class If(test : Tree, conseq : Tree, alt : Tree) extends Tree {
    override def clone() = If(test, conseq, alt)
  }
  case class Fun(formals : List[String], body : Tree) extends Tree {
    override def clone() = Fun(formals, body)
  }
  case class Call(fun : Tree, actuals : List[Tree]) extends Tree {
    override def clone() = Call(fun, actuals)
  }
  case class Letrec(nameValues : List[(String, Tree)], body : Tree) extends Tree {
    override def clone() = Letrec(nameValues, body)
  }

  def toScript(tree : Tree) : String = tree match {
    case Constant(value) => value.toString
    case Var(name) => name
    case If(test, conseq, alt) => s"if (${toScript(test)}) {\n${Utils.leftPadding(toScript(conseq), "\t")}\n} else {\n${Utils.leftPadding(toScript(alt), "\t")}\n}"
    case Fun(List(name), body) => s"function($name : ${tree.ty.asInstanceOf[TyBinary].first}) {\n${Utils.leftPadding(toScript(body), "\t")}\n}"
    case Fun(formals, body) => s"function(${formals.mkString(",")}) {\n${Utils.leftPadding(toScript(body), "\t")}\n}"
    case Call(Var(name), List(first, second)) if name.startsWith("..") => s"${toScript(first)} ${name.substring(2)} ${toScript(second)}"
    case Call(Var(name), List(expr)) if name.startsWith(".") => s"${name.substring(1)} ${toScript(expr)}"
    case Call(Var(name), actuals) => s"$name(${actuals.map(toScript).mkString(",")})"
    case Call(fun, actuals) => s"(${toScript(fun)})(${actuals.map(toScript).mkString(",")})"
    case Letrec(nameValues, body) => s"${nameValues.map(nameValue => s"var ${nameValue._1} : (${nameValue._2.ty}) = {${toScript(nameValue._2)}};").mkString("\n")}\n${toScript(body)}"
  }
}

object MiniHaskellTypeSystem {

  import TypeSystem._

  val STRING = TyConstant("String")
  val INT = TyConstant("Int")
  val DOUBLE = TyConstant("Double")
  val BOOL = TyConstant("Boolean")
  val VOID = TyConstant("Void")
  val FunctionConstructor = "->"

  def makeFun(types : Type*) = types match {
    case List(ty) => TyBinary(FunctionConstructor, VOID, ty)
    case _ => types.dropRight(1).foldRight(types.last) {
      case (formalTy, resultTy) => TyBinary(FunctionConstructor, formalTy, resultTy)
    }
  }

  def getConstantType(value : Any) : List[Type] = value match {
    case _ : Int => List(INT, DOUBLE)
    case _ : Double => List(DOUBLE)
    case _ : String => List(STRING)
    case _ : Boolean => List(BOOL)
  }

  val BuiltinValue2Type = new ValueEnv(List(
    ".-" -> List(makeFun(INT, INT), makeFun(DOUBLE, DOUBLE)),
    ".not" -> List(makeFun(BOOL, BOOL)),
    "..+" -> List(makeFun(INT, INT, INT), makeFun(DOUBLE, DOUBLE, DOUBLE)),
    "..-" -> List(makeFun(INT, INT, INT), makeFun(DOUBLE, DOUBLE, DOUBLE)),
    "..*" -> List(makeFun(INT, INT, INT), makeFun(DOUBLE, DOUBLE, DOUBLE)),
    "../" -> List(makeFun(INT, INT, INT), makeFun(DOUBLE, DOUBLE, DOUBLE)),
    "..%" -> List(makeFun(INT, INT, INT), makeFun(DOUBLE, DOUBLE, DOUBLE)),
    "..<" -> List(makeFun(INT, INT, BOOL), makeFun(DOUBLE, DOUBLE, BOOL)),
    "..<=" -> List(makeFun(INT, INT, BOOL), makeFun(DOUBLE, DOUBLE, BOOL)),
    "..>" -> List(makeFun(INT, INT, BOOL), makeFun(DOUBLE, DOUBLE, BOOL)),
    "..>=" -> List(makeFun(INT, INT, BOOL), makeFun(DOUBLE, DOUBLE, BOOL)),
    "..==" -> List(makeFun(INT, INT, BOOL), makeFun(DOUBLE, DOUBLE, BOOL)),
    "..!=" -> List(makeFun(INT, INT, BOOL), makeFun(DOUBLE, DOUBLE, BOOL)),
    "..and" -> List(makeFun(BOOL, BOOL, BOOL)),
    "..or" -> List(makeFun(BOOL, BOOL, BOOL)),
    "add1" -> List(makeFun(INT, INT), makeFun(DOUBLE, DOUBLE)),
    "zero" -> List(makeFun(INT, BOOL), makeFun(DOUBLE, BOOL))
  ))

  def annotateType(tree : MiniHaskellAST.Tree) : MiniHaskellAST.Tree = {
    import MiniHaskellAST._
    import ErrorList._

    def infer(tree : Tree, vEnv : ValueEnv, tEnv : TypeEnv) : ErrorList[(Tree, TypeEnv)] = tree match {
      case Constant(value) =>
        Left(getConstantType(value).map(ty => (tree.withType(ty), tEnv)))
      case Var(name) => vEnv.get(name) match {
        case Some(types) => Left(types.map {
          case ty : Type => (tree.withType(instantiate(ty)), tEnv)
        })
        case None => Right(s"Can't find variable's type: $name")
      }
      case If(test, conseq, alt) =>
        for ((test, tEnv2) <- infer(test, vEnv, tEnv);
             tEnv2 <- unify(test.ty, BOOL, tEnv2)(test.pos);
             (conseq, tEnv2) <- infer(conseq, vEnv, tEnv2);
             (alt, tEnv2) <- infer(alt, vEnv, tEnv2);
             tEnv2 <- unify(conseq.ty, alt.ty, tEnv2)(conseq.pos))
        yield (If(test, conseq, alt).withType(alt.ty), tEnv2)
      case Fun(formals, body) =>
        val name2Type = formals.map(name => (name, List(genTyVar())))
        val vEnv2 = vEnv ++ name2Type
        for ((body, tEnv2) <- infer(body, vEnv2, tEnv))
        yield (Fun(formals, body).withType(makeFun(name2Type.map(_._2(0)) ::: List(body.ty) : _*)), tEnv2)
      case Call(fun, actuals) =>
        for ((actuals, tEnv2) <- actuals.foldRight(Left(List((Nil, tEnv))) : ErrorList[(List[Tree], TypeEnv)]) {
          case (a, errorList) =>
            for ((actuals, tEnv2) <- errorList;
                 (a2, tEnv2) <- infer(a, vEnv, tEnv2))
            yield (a2 :: actuals, tEnv2)
        };
             (fun, tEnv2) <- infer(fun, vEnv, tEnv2);
             resultTy = genTyVar();
             tEnv2 <- unify(fun.ty, makeFun(actuals.map(_.ty) ::: List(resultTy) : _*), tEnv2)(fun.pos))
        yield (Call(fun, actuals).withType(resultTy), tEnv2)
      case Letrec(nameValues, body) =>
        val name2TyValues = nameValues.zipWithIndex.groupBy(_._1._1).toList.sortBy(_._2(0)._2).map {
          case (name, values) => (name, values.map { case ((_, value), _) => (genTyVar(), value)})
        }
        val vEnv2 = vEnv ++ name2TyValues.map { case (name, tyValues) => (name, tyValues.map(_._1))}
        val outerTypes = vEnv2.flatMap(p => p._2.collect { case TyVar(name) => name}).toSet
        for ((name2Values, vEnv2, tEnv2) <- name2TyValues.foldLeft(Left(List((Nil, vEnv2, tEnv))) : ErrorList[(List[(String, List[Tree])], ValueEnv, TypeEnv)]) {
          case (errorList, (name, tyValues)) =>
            for ((name2Values, vEnv2, tEnv2) <- errorList;
                 (values, tEnv2) <- tyValues.foldLeft(Left(List((Nil, tEnv2))) : ErrorList[(List[Tree], TypeEnv)]) {
                   case (errorList2, (ty, value)) =>
                     for ((values, tEnv2) <- errorList2;
                          (value, tEnv2) <- infer(value, vEnv2.updated(name, List(ty)), tEnv2);
                          tEnv2 <- unify(value.ty, ty, tEnv2)(value.pos))
                     yield (values ::: List(value), tEnv2)
                 })
            yield (
              name2Values ::: List((name, values)),
              vEnv2.updated(name, values.map { case value => generalize(substitute(value.ty, tEnv2), outerTypes)}),
              tEnv2)
        };
             (body, tEnv2) <- infer(body, vEnv2, tEnv2))
        yield (Letrec(name2Values.flatMap { case (name, values) => values.map(value => (name, value))}, body).withType(body.ty), tEnv2)
    }

    def annotate(tree : Tree, tEnv : TypeEnv) : Unit = {
      tree.ty = substitute(tree.ty, tEnv)
      tree match {
        case _ : Constant | _ : Var =>
        case If(test, conseq, alt) =>
          annotate(test, tEnv)
          annotate(conseq, tEnv)
          annotate(alt, tEnv)
        case Fun(_, body) => annotate(body, tEnv)
        case Call(fun, actuals) =>
          annotate(fun, tEnv)
          actuals.foreach { a => annotate(a, tEnv)}
        case Letrec(nameValues, body) =>
          nameValues.foreach { case (_, value) => annotate(value, tEnv)}
          annotate(body, tEnv)
      }
    }

    infer(tree, BuiltinValue2Type, new TypeEnv(Nil)) match {
      case Right(error) => throw new Exception(error)
      case Left(Nil) => throw new Exception("Illegal state!!")
      case Left((tree2, tEnv) :: _) =>
        annotate(tree2, tEnv)
        tree2
    }
  }
}

class MiniHaskellParser extends scala.util.parsing.combinator.RegexParsers with scala.util.parsing.combinator.PackratParsers {

  import MiniHaskellAST._

  type ParserT[T] = Parser[T]

  private def genTempName() : String = s"__temp_${Utils.genID()}"

  private val INTEGER : ParserT[Int] = """\d+""".r ^^ (_.toInt)
  private val DOUBLE : ParserT[Double] = """(\d+)?\.\d+""".r ^^ (_.toDouble)
  private val STRING : ParserT[String] = """"(\\.|[^"])*"""".r : Parser[String]
  private val BOOLEAN : ParserT[Boolean] = """true|false""".r ^^ (_ == "true")
  private val NAME : ParserT[String] = """[a-zA-Z_]\w*""".r : Parser[String]
  private lazy val factor : ParserT[Tree] = (
    DOUBLE ^^ Constant
      | INTEGER ^^ Constant
      | STRING ^^ Constant
      | BOOLEAN ^^ Constant
      | "(" ~> expr <~ ")"
      | NAME ^^ Var)
  private lazy val call : ParserT[Tree] = positioned(factor) ~ rep1("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case fun ~ allActuals => allActuals.foldLeft(fun) { case (e, actuals) => Call(e, actuals)}
  }
  private lazy val unary : ParserT[Tree] = rep("-" | "not") ~ (call | factor) ^^ { case ops ~ e => ops.foldLeft(e) { case (e2, op) => Call(Var(s".$op"), List(e2))}}
  private lazy val mul : ParserT[Tree] = unary ~ rep( """[\*\/\%]""".r ~ unary) ^^ { case e ~ opes => opes.foldLeft(e) { case (e2, op ~ e3) => Call(Var(s"..$op"), List(e2, e3))}}
  private lazy val add : ParserT[Tree] = mul ~ rep( """[+-]""".r ~ mul) ^^ { case e ~ opes => opes.foldLeft(e) { case (e2, op ~ e3) => Call(Var(s"..$op"), List(e2, e3))}}
  private lazy val relat : ParserT[Tree] = add ~ rep( """<=|<|>=|>|==|!=""".r ~ add) ^^ { case e ~ opes => opes.foldLeft(e) { case (e2, op ~ e3) => Call(Var(s"..$op"), List(e2, e3))}}
  private lazy val logic : ParserT[Tree] = relat ~ rep( """and|or""".r ~ relat) ^^ { case e ~ opes => opes.foldLeft(e) { case (e2, op ~ e3) => Call(Var(s"..$op"), List(e2, e3))}}
  private lazy val expr : ParserT[Tree] = (
    "if" ~> positioned(expr) ~ ("then" ~> positioned(expr)) ~ ("else" ~> positioned(expr)) ^^ { case test ~ conseq ~ alt => If(test, conseq, alt)}
      | "let" ~> stmts ~ ("in" ~> expr) ^^ { case nameValues ~ e => Letrec(nameValues, e)}
      | "\\" ~> rep(NAME) ~ ("->" ~> expr) ^^ { case formals ~ body => Fun(formals, body)}
      | logic)
  private lazy val stmt : ParserT[(String, Tree)] = (
    "var" ~> NAME ~ ("=" ~> positioned(expr)) ^^ { case name ~ e => (name, e)}
      | NAME ~ rep(NAME) ~ ("=" ~> positioned(expr)) ^^ { case fname ~ formals ~ e => (fname, Fun(formals, e).setPos(e.pos))}
      | NAME ~ rep(NAME) ~ positioned(guards) ^^ { case fname ~ formals ~ e => (fname, Fun(formals, e).setPos(e.pos))}
      | positioned(call) ^^ { case e => (genTempName(), e)})
  private lazy val stmts : ParserT[List[(String, Tree)]] = rep1sep(stmt, opt(";"))
  private lazy val guards : ParserT[Tree] = rep1(guard) ^^ { case ks => ks.foldRight(Constant(0) : Tree) { case (k, e) => k(e)}}
  private lazy val guard : ParserT[Tree => Tree] = (
    "|" ~> "otherwise" ~> ("=" ~> positioned(expr)) ^^ {
      case e => (_ : Tree) => e
    }
      | "|" ~> positioned(logic) ~ ("=" ~> positioned(expr)) ^^ {
      case test ~ conseq => (alt : Tree) => If(test, conseq, alt)
    })
  private lazy val main : ParserT[Tree] = stmts ^^ { case nameValues => Letrec(nameValues, Constant(0))}

  def parse(input : String) : Tree = {
    val input2 = input.replaceAll( """--[^\n]*""", "")
    val result = parseAll(main, input2)
    if (result.successful) result.get
    else throw new Exception(result.toString)
  }
}

object Test extends App {
  val sources = List(
    """
    var v = \x-> x
    """,
    """
    var v = \f-> \x-> f(x)
    """,
    """
    var v = \f-> \x-> f(f(x))
    """,
    """
    var v = (\f-> \x-> f(f(x)))(add1)
    """,
    """
    var v = if zero(0) then true else false
    """,
    """
    var v = \f-> \x-> f(x + 1)
    """,
    """
    var v = \m-> \n-> \f-> \x-> m(n(f))(x)
    """,
    """
    var v = (\f-> f(1))(\x->x)
    """,
    """
    var S = \x-> \y-> \z-> x(z)(y(z))
    """,
    """
    var SK = (\x-> \y-> \z-> x(z)(y(z)))(\x-> \y-> x)
    """,
    """
    var SKK = (\x-> \y-> \z-> x(z)(y(z)))(\x-> \y-> x)(\x-> \y-> x)
    """,
    """
    identity x = x
    identity("abcdef")
    identity(2)
    identity(true)
    """,
    """
    fib n 
      | n < 2 = 1
      | otherwise = fib(n - 1) + fib(n - 2)
    """,
    """
    var even =
      let
        even x | x < 2 = x == 0 | otherwise = odd(x - 1)
        odd x | x < 2 = x == 1 | otherwise = even(x - 1)
        in even
    even(5)
    even(10)
    """,
    """
    var t1 = 3 + 2
    var t2 = 3.14 + 2.48
    """,
    """
    var d = 3.14 + 3
    """,
    """
    f x = x + 1
    f(1)
    f(1.2)
    """,
    """
    f x = x + 1
    f x = x == 1
    f(2)
    """,
    """
    f x = x + 1
    f x = x and false
    f(3.14)
    f(false)
    """,
    """
    f n | n == 0 = true | otherwise = not f(n - 1)
    f n | n < 2 = 1 | otherwise = f(n - 1) + f(n - 2)
    f a b | a == 0 = b | otherwise = f(b % a, a)
    var t = f(3.14) and f(3)
    var t2 = f(3.14) + f(3.14)
    f(18, 32)
    """
  )

  val parser = new MiniHaskellParser()
  for (source <- sources) {
    val tree = parser.parse(source)
    val typedTree = MiniHaskellTypeSystem.annotateType(tree)
    println(s"\n${source.trim()}\n\t==>\n${MiniHaskellAST.toScript(typedTree)}")
  }
}
