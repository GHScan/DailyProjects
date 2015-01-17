package demo.lua

class PCLuaParser extends scala.util.parsing.combinator.RegexParsers with scala.util.parsing.combinator.PackratParsers {

  import demo.lua.LuaAST._

  override val whiteSpace = """\s+|\-\-[^\n]*|\[\[([^\]]|\][^\]])*\]\]|\[=\[([^\]]|\][^\]])*\]=\]""".r

  private val NAME : Parser[String] = """[a-zA-Z_]\w*""".r
  private val NUMBER : Parser[Double] = """((\d+)?\.)?\d+""".r ^^ (_.toDouble)
  private val STRING : Parser[String] = """"(\\.|[^"])*"|'(\\.|[^'])*'""".r ^^ utils.Func.unescape

  val unop : Parser[String] = "-" | "not" | "#"
  val powop : Parser[String] = "^"
  val mulop : Parser[String] = "*" | "/" | "%"
  val addop : Parser[String] = "+" | "-"
  val concatop : Parser[String] = ".."
  val relatop : Parser[String] = "<" | "<=" | ">" | ">=" | "==" | "~="
  val andop : Parser[String] = "and"
  val orop : Parser[String] = "or"
  val fieldsep : Parser[Any] = "," | ";"
  lazy val field : Parser[(Expr, Expr)] = (
    "[" ~ exp ~ "]" ~ "=" ~ exp ^^ { case _ ~ key ~ _ ~ _ ~ value => (key, value)}
      | NAME ~ "=" ~ exp ^^ { case key ~ _ ~ value => (Constant(key), value)}
      | exp ^^ { e => (Constant(-1), e)})
  lazy val fieldlist : Parser[List[(Expr, Expr)]] = field ~ rep(fieldsep ~> field) <~ opt(fieldsep) ^^ { case head ~ tail => head :: tail}
  lazy val tableconstructor : Parser[TableConstructor] = "{" ~> opt(fieldlist) <~ "}" ^^ (e => TableConstructor(e.getOrElse(Nil)))
  lazy val namelist : Parser[List[String]] = rep1sep(NAME, ",")
  lazy val explist : Parser[List[Expr]] = rep1sep(exp, ",")
  lazy val parlist : Parser[(List[String], Boolean)] = (
    namelist ~ opt("," ~ "...") ^^ {
      case list ~ Some(_) => (list, true)
      case list ~ None => (list, false)
    }
      | "..." ^^ (_ => (Nil, true))
    )
  lazy val funcbody : Parser[((List[String], Boolean), Block)] =
    ("(" ~> opt(parlist) <~ ")") ~ block <~ "end" ^^ { case plist ~ _block => (plist.getOrElse((Nil, false)), _block)}
  lazy val function : Parser[Func] =
    "function" ~> funcbody ^^ { case ((formals, hasVarArg), body) => Func(formals, body, hasVarArg, hasSelf = false)}
  lazy val args : Parser[List[Expr]] = (
    "(" ~> opt(explist) <~ ")" ^^ (_.getOrElse(Nil))
      | tableconstructor ^^ (List(_))
      | "String" ^^ (s => List(Constant(s))))
  lazy val functioncall : Parser[Expr] = (
    prefixexp ~ args ^^ { case func ~ _args => Call(func, _args)}
      | prefixexp ~ ":" ~ NAME ~ args ^^ { case obj ~ _ ~ methodNAME ~ _args => MethodCall(obj, methodNAME, _args)})
  lazy val prefixexp : Parser[Expr] = (
    _var
      | functioncall
      | "(" ~> exp <~ ")")
  lazy val factor : Parser[Expr] = (
    "nil" ^^ (_ => Constant(null))
      | "false" ^^ (_ => Constant(false))
      | "true" ^^ (_ => Constant(true))
      | "Number" ^^ (v => Constant(v.asInstanceOf[Double]))
      | "String" ^^ (v => Constant(v))
      | "..." ^^ (_ => VarArgument)
      | function
      | prefixexp
      | tableconstructor)
  lazy val unary : Parser[Expr] =
    rep(unop) ~ factor ^^ { case ops ~ e => ops.foldLeft(e) { (e, op) => UnaryOp(op, e)}}
  lazy val pow : Parser[Expr] =
    unary ~ rep(powop ~ unary) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  lazy val mul : Parser[Expr] =
    pow ~ rep(mulop ~ pow) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  lazy val add : Parser[Expr] =
    mul ~ rep(addop ~ mul) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  lazy val concat : Parser[Expr] =
    add ~ rep(concatop ~ add) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  lazy val relat : Parser[Expr] =
    concat ~ rep(relatop ~ concat) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  lazy val and : Parser[Expr] =
    relat ~ rep(andop ~ relat) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  lazy val or : Parser[Expr] =
    and ~ rep(orop ~ and) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  lazy val exp : Parser[Expr] = or
  lazy val _var : Parser[Expr] = (
    NAME ^^ Variable
      | prefixexp ~ "[" ~ exp ~ "]" ^^ { case t ~ _ ~ key ~ _ => TableLookup(t, key)}
      | prefixexp ~ "." ~ NAME ^^ { case obj ~ _ ~ fieldNAME => FieldOf(obj, fieldNAME)})
  lazy val varlist : Parser[List[Expr]] = rep1sep(_var, ",")
  lazy val funcname : Parser[(Expr, Boolean)] =
    NAME ~ rep("." ~> NAME) ~ opt(":" ~> NAME) ^^ {
      case name ~ fields ~ method =>
        val prefix = fields.foldLeft(Variable(name) : Expr) { (e, name) => FieldOf(e, name)}
        method match {
          case Some(m) => (FieldOf(prefix, m), true)
          case None => (prefix, false)
        }
    }
  lazy val laststat : Parser[Statement] = (
    "return" ~> opt(explist) ^^ (es => Return(es.getOrElse(Nil)))
      | "break" ^^ (_ => Break))
  lazy val stat : Parser[Statement] = (
    varlist ~ "=" ~ explist ^^ { case lefts ~ _ ~ rights => Assign(lefts, rights)}
      | functioncall ^^ CallStatement
      | "do" ~> block <~ "end"
      | "while" ~> exp ~ ("do" ~> block <~ "end") ^^ { case e ~ _block => While(e, _block)}
      | "repeat" ~> block ~ ("until" ~> exp) ^^ { case _block ~ e => Repeat(_block, e)}
      | ("if" ~> exp ~ ("then" ~> block)) ~ rep("elseif" ~> exp ~ ("then" ~> block)) ~ opt("else" ~> block) <~ "end" ^^ {
      case case1 ~ cases ~ fallback => Cond((case1 :: cases).map { case e ~ b => (e, b)}, fallback.orNull)
    }
      | ("for" ~> NAME <~ "=") ~ ((exp <~ ",") ~ exp ~ opt("," ~> exp)) ~ ("do" ~> block <~ "end") ^^ {
      case name ~ (first ~ last ~ step) ~ _block => RangeFor(name, first, last, step.orNull, _block)
    }
      | ("for" ~> namelist <~ "in") ~ explist ~ ("do" ~> block <~ "end") ^^ {
      case names ~ initExpr ~ body => IterateFor(names, initExpr, body)
    }
      | "function" ~> funcname ~ funcbody ^^ { case (left, hasSelf) ~ (((formals, hasVarArg), body)) => Assign(List(left), List(Func(formals, body, hasVarArg, hasSelf)))}
      | "local" ~> "function" ~> NAME ~ funcbody ^^ { case name ~ (((formals, hasVarArg), body)) => LocalDef(List(name), List(Func(formals, body, hasVarArg, hasSelf = false)))}
      | "local" ~> namelist ~ opt("=" ~> explist) ^^ { case names ~ exprs => LocalDef(names, exprs.getOrElse(Nil))})
  lazy val chunk : Parser[List[Statement]] =
    rep(stat <~ opt(";")) ~ opt(laststat <~ opt(";")) ^^ {
      case l ~ Some(last) => l ::: List(last)
      case l ~ None => l
    }
  lazy val block : Parser[Block] = chunk ^^ Block

  def parse(input : String) : Any = {
    val result = parseAll(chunk, input)
    if (result.successful) result.get else throw new Exception(result.toString)
  }
}
