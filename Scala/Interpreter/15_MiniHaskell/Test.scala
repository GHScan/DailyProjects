/**
factor := INTEGER
            STRING
            BOOLEAN
            "(" expr ")"
            {NAME} "->" unary
            NAME
  postfix := factor { "(" [expr {"," expr }] ")" }
  unary := { "-" | "not" } postfix
  mul := unary { ("*" | "/" | "%") unary }
  add := mul { ("+" | "-") mul }
  relat := add { ("<" | "<=" | ">" | ">=" | "==" | "!=") add }
  logic := relat { ("and" | "or") relat }
  expr := logic
          "if" expr "then" expr "else" expr

  stmts := stmt | "{" stmt { [";"]  stmt } "}"
  stmt := "var" NAME rhs
          NAME { NAME } rhs
          call
  rhs := "=" expr ["where" stmts]

  main := stmt { [";"] stmt }

  */

object TypeSystem {
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
  case class TyFunc(formals : List[String], body : Type) extends Type {
    override def toString = body.toString
  }
  case class TyCall(func : Type, actuals : List[Type]) extends Type {
    override def toString = func match {
      case TyFunc(formals, body) => substitute(body, formals.zip(actuals)).toString
      case _ => s"$func[${actuals.mkString(",")}}]"
    }
  }

  def substitute(ty : Type, env : List[(String, Type)]) : Type = {
    def iterate(ty : Type, env : List[(String, Type)], occurs : List[String]) : Type = ty match {
      case _ : TyConstant => ty
      case TyVar(name) if occurs.contains(name) => throw new Exception("Illegal: Recursive type")
      case TyVar(name) => env.find(_._1 == name) match {
        case None => ty
        case Some((_, target)) => iterate(target, env, name :: occurs)
      }
      case TyBinary(name, first, second) => TyBinary(name, iterate(first, env, occurs), iterate(second, env, occurs))
      case TyFunc(formals, body) => TyFunc(formals, iterate(body, env, occurs))
      case TyCall(func, actuals) => TyCall(iterate(func, env, occurs), actuals.map(a => iterate(a, env, occurs))) match {
        case TyCall(TyFunc(formals, body), actuals2) => iterate(body, formals.zip(actuals2) ::: env, occurs)
        case c => c
      }
    }
    iterate(ty, env, Nil)
  }

  def generalize(ty : Type, outerTypes : Set[String]) : TyFunc = {
    var innerTypes = Set[String]()
    def collectInnerTypes(ty : Type, outerTypes : Set[String]) : Unit = ty match {
      case _ : TyConstant =>
      case TyVar(name) if outerTypes(name) =>
      case TyVar(name) => innerTypes += name
      case TyBinary(_, first, second) =>
        collectInnerTypes(first, outerTypes)
        collectInnerTypes(second, outerTypes)
      case TyFunc(formals, body) =>
        collectInnerTypes(body, outerTypes ++ formals)
      case TyCall(func, actuals) =>
        collectInnerTypes(func, outerTypes)
        actuals.foreach { a => collectInnerTypes(a, outerTypes)}
    }
    collectInnerTypes(ty, outerTypes)
    TyFunc(innerTypes.toList, ty)
  }

  def unify(ty1 : Type, ty2 : Type, env : List[(String, Type)]) : List[(String, Type)] =
    (substitute(ty1, env), substitute(ty2, env)) match {
      case (TyVar(name1), TyVar(name2)) => (if (name1 < name2) (name2, TyVar(name1)) else (name1, TyVar(name2))) :: env
      case (TyVar(name1), _ty2) => (name1, _ty2) :: env
      case (_ty1, TyVar(name2)) => (name2, _ty1) :: env
      case (TyConstant(name1), TyConstant(name2)) if name1 == name2 => env
      case (TyBinary(name1, ty11, ty12), TyBinary(name2, ty21, ty22)) if name1 == name2 => unify(ty12, ty22, unify(ty11, ty21, env))
      case (TyFunc(formals1, body1), TyFunc(formals2, body2)) if formals1.length == formals2.length =>
        val env2 = unify(body1, substitute(body2, formals2.zip(formals1).map { case (name2, name1) => (name2, TyVar(name1))}), env)
        env2.dropRight(env.length).filter(p => !formals1.contains(p._1)) ::: env
      case (TyCall(func1, actuals1), TyCall(func2, actuals2)) if actuals1.length == actuals2.length =>
        ((func1, func2) :: actuals1.zip(actuals2)).foldLeft(env) { case (env2, (_ty1, _ty2)) => unify(_ty1, _ty2, env2)}
      case (ty1_, ty2_) => throw new Exception(s"Unify failed: $ty1_, $ty2_")
    }
}

object MiniHaskellAST {
  sealed abstract class Tree extends scala.util.parsing.input.Positional {
    var ty : TypeSystem.Type = null
  }
  case class Constant(value : Any) extends Tree
  case class Var(name : String) extends Tree
  case class UnaryOp(op : String, expr : Tree) extends Tree
  case class BinaryOp(op : String, first : Tree, second : Tree) extends Tree
  case class If(test : Tree, conseq : Tree, alt : Tree) extends Tree
  case class Func(formals : List[String], body : Tree) extends Tree
  case class Call(func : Tree, actuals : List[Tree]) extends Tree

  def toScript(tree : Tree) : String = tree match {
    case Constant(value) => value.toString
    case Var(name) => name
    case UnaryOp(op, expr) => s"$op ${toScript(expr)}"
    case BinaryOp(op, first, second) => s"(${toScript(first)} $op ${toScript(second)})"
    case If(test, conseq, alt) => s"if (${toScript(test)}) {\n${Utils.leftPadding(toScript(conseq), "\t")}\n} else {\n${Utils.leftPadding(toScript(alt), "\t")}\n}"
    case Func(List(name), body) => s"function($name : ${tree.ty.asInstanceOf[TypeSystem.TyBinary].first}}) {\n${Utils.leftPadding(toScript(body), "\t")}\n}"
    case Func(formals, body) => s"function(${formals.mkString(",")}) {\n${Utils.leftPadding(toScript(body), "\t")}\n}"
    case Call(Var(name), actuals) => s"$name(${actuals.map(toScript).mkString(",")})"
    case Call(func@Func(List(name), body), actuals) =>
      s"var $name : ${func.ty.asInstanceOf[TypeSystem.TyBinary].first} = {${actuals.map(toScript).mkString(",")}};\n${toScript(body)}"
    case Call(func, actuals) => s"(${toScript(func)})(${actuals.map(toScript).mkString(",")})"
  }
}

object MiniHaskellTypeSystem {

  import TypeSystem._

  val StringType = TyConstant("String")
  val IntegerType = TyConstant("Int")
  val BooleanType = TyConstant("Boolean")
  val VoidType = TyConstant("Void")
  val FunctionConstructor = "->"
  def createFuncType(formalTy : Type, resultTy : Type) = TyBinary(FunctionConstructor, formalTy, resultTy)

  def getConstantType(value : Any) : Type = value match {
    case _ : Int => IntegerType
    case _ : String => StringType
    case _ : Boolean => BooleanType
  }

  val BuiltinValue2Type = List(
    "+" -> createFuncType(IntegerType, createFuncType(IntegerType, IntegerType)),
    "-" -> createFuncType(IntegerType, createFuncType(IntegerType, IntegerType)),
    "*" -> createFuncType(IntegerType, createFuncType(IntegerType, IntegerType)),
    "/" -> createFuncType(IntegerType, createFuncType(IntegerType, IntegerType)),
    "%" -> createFuncType(IntegerType, createFuncType(IntegerType, IntegerType)),
    "<" -> createFuncType(IntegerType, createFuncType(IntegerType, BooleanType)),
    "<=" -> createFuncType(IntegerType, createFuncType(IntegerType, BooleanType)),
    ">" -> createFuncType(IntegerType, createFuncType(IntegerType, BooleanType)),
    ">=" -> createFuncType(IntegerType, createFuncType(IntegerType, BooleanType)),
    "==" -> createFuncType(IntegerType, createFuncType(IntegerType, BooleanType)),
    "!=" -> createFuncType(IntegerType, createFuncType(IntegerType, BooleanType)),
    "and" -> createFuncType(IntegerType, createFuncType(IntegerType, BooleanType)),
    "or" -> createFuncType(IntegerType, createFuncType(IntegerType, BooleanType)),
    "add1" -> createFuncType(IntegerType, IntegerType),
    "zero" -> createFuncType(IntegerType, BooleanType)
  )

  def genTyVar() : TyVar = TyVar(s"T${Utils.genID()}")

  def annotateType(tree : MiniHaskellAST.Tree) : MiniHaskellAST.Tree = {
    import MiniHaskellAST._

    def infer(tree : Tree, vEnv : List[(String, Type)], tEnv : List[(String, Type)]) : List[(String, Type)] = tree match {
      case Constant(value) =>
        tree.ty = getConstantType(value)
        tEnv
      case Var(name) => vEnv.find(_._1 == name) match {
        case Some((_, ty)) =>
          tree.ty = ty
          tEnv
        case None => throw new Exception(s"Can't find variable's type: $name")
      }
      // TODO: FIX IT by lookup vEnv
      case UnaryOp(op, expr) =>
        val tEnv2 = infer(expr, vEnv, tEnv)
        op match {
          case "-" =>
            tree.ty = IntegerType
            unify(expr.ty, IntegerType, tEnv2)
          case "not" =>
            tree.ty = BooleanType
            unify(expr.ty, BooleanType, tEnv2)
        }
      case BinaryOp(op, first, second) =>
        vEnv.find(_._1 == op) match {
          case Some((_, TyBinary(FunctionConstructor, formalTy1, TyBinary(FunctionConstructor, formalTy2, resultTy)))) =>
            var tEnv2 = infer(first, vEnv, tEnv)
            tEnv2 = unify(formalTy1, first.ty, tEnv2)
            tEnv2 = infer(second, vEnv, tEnv2)
            tEnv2 = unify(formalTy2, second.ty, tEnv2)
            tree.ty = resultTy
            tEnv2
          case _ => throw new Exception(s"Illegal binary op $op")
        }
      case If(test, conseq, alt) =>
        var tEnv2 = infer(test, vEnv, tEnv)
        tEnv2 = unify(test.ty, BooleanType, tEnv2)
        tEnv2 = infer(conseq, vEnv, tEnv2)
        tEnv2 = infer(alt, vEnv, tEnv2)
        tEnv2 = unify(conseq.ty, alt.ty, tEnv2)
        tree.ty = conseq.ty
        tEnv2
      case Func(formals, body) =>
        val name2Type = formals.map(name => (name, genTyVar()))
        val tEnv2 = infer(body, name2Type ::: vEnv, tEnv)
        tree.ty =
          if (formals.isEmpty) createFuncType(VoidType, body.ty)
          else name2Type.map(_._2).foldRight(body.ty) { case (formalTy, resultTy) => createFuncType(formalTy, resultTy)}
        tEnv2
      case Call(func, actuals) =>
        var tEnv2 = tEnv
        actuals.foreach { a => tEnv2 = infer(a, vEnv, tEnv2)}
        tEnv2 = infer(func, vEnv, tEnv2)
        val resultTy = genTyVar()
        val expectFuncType =
          if (actuals.isEmpty) createFuncType(VoidType, resultTy)
          else actuals.map(_.ty).foldRight(resultTy : Type) { case (formalTy, resultTy2) => createFuncType(formalTy, resultTy2)}
        tEnv2 = unify(func.ty, expectFuncType, tEnv2)
        tree.ty = resultTy
        tEnv2
    }

    def annotate(tree : Tree, tEnv : List[(String, Type)]) : Unit = {
      tree.ty = substitute(tree.ty, tEnv)
      tree match {
        case _ : Constant | _ : Var =>
        case UnaryOp(_, expr) => annotate(expr, tEnv)
        case BinaryOp(_, first, second) =>
          annotate(first, tEnv)
          annotate(second, tEnv)
        case If(test, conseq, alt) =>
          annotate(test, tEnv)
          annotate(conseq, tEnv)
          annotate(alt, tEnv)
        case Func(_, body) => annotate(body, tEnv)
        case Call(func, actuals) =>
          annotate(func, tEnv)
          actuals.foreach { a => annotate(a, tEnv)}
      }
    }

    val tEnv2 = infer(tree, BuiltinValue2Type, Nil)
    annotate(tree, tEnv2)
    tree
  }
}

class MiniHaskellParser extends scala.util.parsing.combinator.RegexParsers with scala.util.parsing.combinator.PackratParsers {

  import MiniHaskellAST._

  private def genTempName() : String = s"__temp_${Utils.genID()}"

  private val INTEGER : PackratParser[Int] = """\d+""".r ^^ (_.toInt)
  private val STRING : PackratParser[String] = """'(\\.|[^'])*'""".r : Parser[String]
  private val BOOLEAN : PackratParser[Boolean] = """true|false""".r ^^ (_ == "true")
  private val NAME : PackratParser[String] = """[a-zA-Z_]\w*""".r : Parser[String]
  private lazy val factor : PackratParser[Tree] = (
    INTEGER ^^ Constant
      | STRING ^^ Constant
      | BOOLEAN ^^ Constant
      | "(" ~> expr <~ ")"
      | rep(NAME) ~ ("->" ~> unary) ^^ { case formals ~ body => Func(formals, body)}
      | NAME ^^ Var)
  private lazy val call : PackratParser[Tree] = factor ~ rep("(" ~> repsep(expr, ",") <~ ")") ^^ {
    case func ~ allActuals => allActuals.foldLeft(func) { case (e, actuals) => Call(e, actuals)}
  }
  private lazy val unary : PackratParser[Tree] = rep("-" | "not") ~ call ^^ { case ops ~ e => ops.foldLeft(e) { case (e2, op) => UnaryOp(op, e2)}}
  private lazy val mul : PackratParser[Tree] = unary ~ rep( """[\*\/\%]""".r ~ unary) ^^ { case e ~ opes => opes.foldLeft(e) { case (e2, op ~ e3) => BinaryOp(op, e2, e3)}}
  private lazy val add : PackratParser[Tree] = mul ~ rep( """[+-]""".r ~ mul) ^^ { case e ~ opes => opes.foldLeft(e) { case (e2, op ~ e3) => BinaryOp(op, e2, e3)}}
  private lazy val relat : PackratParser[Tree] = add ~ rep( """<=|<|>=|>|==|!=""".r ~ add) ^^ { case e ~ opes => opes.foldLeft(e) { case (e2, op ~ e3) => BinaryOp(op, e2, e3)}}
  private lazy val logic : PackratParser[Tree] = relat ~ rep( """and|or""".r ~ relat) ^^ { case e ~ opes => opes.foldLeft(e) { case (e2, op ~ e3) => BinaryOp(op, e2, e3)}}
  private lazy val expr : PackratParser[Tree] = (
    "if" ~> expr ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ { case test ~ conseq ~ alt => If(test, conseq, alt)}
      | logic)
  private lazy val rhs : PackratParser[(Tree, Tree => Tree)] = "=" ~> expr ~ opt("where" ~> stmts) ^^ {
    case e ~ None => (e, identity[Tree] _)
    case e ~ Some(k) => (e, k)
  }
  private lazy val stmt : PackratParser[Tree => Tree] = (
    "var" ~> NAME ~ rhs ^^ {
      case name ~ ((e, k)) => (body : Tree) => Call(Func(List(name), body), List(k(e)))
    }
      | NAME ~ rep(NAME) ~ rhs ^^ {
      case fname ~ formals ~ ((e, k)) => (body : Tree) => Call(Func(List(fname), body), List(Func(formals, k(e))))
    }
      | call ^^ { case e => (body : Tree) => Call(Func(List(genTempName()), body), List(e))}
    )
  private lazy val stmts : PackratParser[Tree => Tree] = (
    stmt
      | "{" ~> rep1sep(stmt, opt(";")) <~ "}" ^^ { case ks => (body : Tree) => ks.foldRight(body) { case (k, e) => k(e)}}
    )
  private lazy val main : PackratParser[Tree] = rep1sep(stmt, opt(";")) ^^ { case ks => ks.foldRight(Constant(0) : Tree) { case (k, e) => k(e)}}

  def parse(input : String) : Tree = {
    val input2 = input.replaceAll( """--[^\n]*""", "")
    val result = parseAll(main, input2)
    if (result.successful) MiniHaskellTypeSystem.annotateType(result.get)
    else throw new Exception(result.toString)
  }
}

object Test extends App {
  val sources = List(
    """
    var v = x-> x
    """.stripMargin,
    """
    var v = f-> x-> f(x)
    """.stripMargin,
    """
    var v = f-> x-> f(f(x))
    """.stripMargin,
    """
    var v = (f-> x-> f(f(x)))(add1)
    """.stripMargin,
    """
    var v = if zero(0) then true else false
    """.stripMargin,
    """
    var v = f-> x-> f(x + 1)
    """.stripMargin,
    """
    var v = m-> n-> f-> x-> m(n(f))(x)
    """.stripMargin,
    """
    var v = (f-> f(1))(x->x)
    """.stripMargin,
    """
    var S = x-> y-> z-> x(z)(y(z))
    """.stripMargin,
    """
    var SK = (x-> y-> z-> x(z)(y(z)))(x-> y-> x)
    """.stripMargin,
    """
    var SKK = (x-> y-> z-> x(z)(y(z)))(x-> y-> x)(x-> y-> x)
    """.stripMargin,
    """
      var even = _even
        where {
          _even x = if x == 0 then true else _odd(x - 1)
          _odd x = if x == 1 then true else _even(x - 1)
        }
      println(even(5))
      println(even(10))
    """.stripMargin
  )

  val parser = new MiniHaskellParser()
  for (source <- sources) {
    val r = parser.parse(source)
    println(s"\n${source.trim()}\n\t==>\n${MiniHaskellAST.toScript(r)}")
  }
}
