package demo.lua

import scala.util.parsing.input.CharSequenceReader

class PCLuaParser extends scala.util.parsing.combinator.RegexParsers with scala.util.parsing.combinator.PackratParsers {

  import demo.lua.LuaAST._

  override val whiteSpace = """(\s|--[^\n]*|\[\[([^\]]|\][^\]])*\]\]|\[=\[([^\]]|\][^\]])*\]=\])+""".r

  private val NAME : PackratParser[String] = """[a-zA-Z_]\w*""".r : Parser[String]
  private val NUMBER : PackratParser[Double] = """((\d+)?\.)?\d+""".r ^^ (_.toDouble)
  private val STRING : PackratParser[String] = """"(\\.|[^"])*"|'(\\.|[^'])*'""".r ^^ utils.Func.unescape

  private val unop : PackratParser[String] = "-" | "not" | "#"
  private val powop : PackratParser[String] = "^" : Parser[String]
  private val mulop : PackratParser[String] = "*" | "/" | "%"
  private val addop : PackratParser[String] = "+" | "-"
  private val concatop : PackratParser[String] = ".." : Parser[String]
  private val relatop : PackratParser[String] = "<" | "<=" | ">" | ">=" | "==" | "~="
  private val andop : PackratParser[String] = "and" : Parser[String]
  private val orop : PackratParser[String] = "or" : Parser[String]
  private val fieldsep : PackratParser[Any] = "," | ";"
  private lazy val field : PackratParser[(Expr, Expr)] = (
    "[" ~ exp ~ "]" ~ "=" ~ exp ^^ { case _ ~ key ~ _ ~ _ ~ value => (key, value)}
      | NAME ~ "=" ~ exp ^^ { case key ~ _ ~ value => (Constant(key), value)}
      | exp ^^ { e => (Constant(-1), e)})
  private lazy val fieldlist : PackratParser[List[(Expr, Expr)]] = field ~ rep(fieldsep ~> field) <~ opt(fieldsep) ^^ { case head ~ tail => head :: tail}
  private lazy val tableconstructor : PackratParser[TableConstructor] = "{" ~> opt(fieldlist) <~ "}" ^^ (e => TableConstructor(e.getOrElse(Nil)))
  private lazy val namelist : PackratParser[List[String]] = rep1sep(NAME, ",")
  private lazy val explist : PackratParser[List[Expr]] = rep1sep(exp, ",")
  private lazy val parlist : PackratParser[(List[String], Boolean)] = (
    "..." ^^ (_ => (Nil, true))
      | namelist ~ opt("," ~ "...") ^^ {
      case list ~ Some(_) => (list, true)
      case list ~ None => (list, false)
    })
  private lazy val funcbody : PackratParser[((List[String], Boolean), Block)] =
    ("(" ~> opt(parlist) <~ ")") ~ block <~ "end" ^^ { case plist ~ _block => (plist.getOrElse((Nil, false)), _block)}
  private lazy val function : PackratParser[Func] =
    "function" ~> funcbody ^^ { case ((formals, hasVarArg), body) => Func(formals, body, hasVarArg, hasSelf = false)}
  private lazy val args : PackratParser[List[Expr]] = (
    "(" ~> opt(explist) <~ ")" ^^ (_.getOrElse(Nil))
      | tableconstructor ^^ (List(_))
      | STRING ^^ (s => List(Constant(s)))
      | failure("parse args failed"))
  private lazy val functioncall : PackratParser[Expr] = (
    prefixexp ~ ":" ~ NAME ~ args ^^ { case obj ~ _ ~ methodNAME ~ _args => MethodCall(obj, methodNAME, _args)}
      | prefixexp ~ args ^^ { case func ~ _args => Call(func, _args)})
  private lazy val prefixexp : PackratParser[Expr] = (
    "(" ~> exp <~ ")"
      | functioncall
      | _var)
  private lazy val factor : PackratParser[Expr] = (
    "nil" ^^ (_ => Constant(null))
      | "false" ^^ (_ => Constant(false))
      | "true" ^^ (_ => Constant(true))
      | NUMBER ^^ (v => Constant(v.asInstanceOf[Double]))
      | STRING ^^ (v => Constant(v))
      | "..." ^^ (_ => VarArgument)
      | function
      | tableconstructor
      | prefixexp
      | failure("factor parse failed"))
  private lazy val unary : PackratParser[Expr] =
    rep(unop) ~ factor ^^ { case ops ~ e => ops.foldLeft(e) { (e, op) => UnaryOp(op, e)}}
  private lazy val pow : PackratParser[Expr] =
    unary ~ rep(powop ~ unary) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  private lazy val mul : PackratParser[Expr] =
    pow ~ rep(mulop ~ pow) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  private lazy val add : PackratParser[Expr] =
    mul ~ rep(addop ~ mul) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  private lazy val concat : PackratParser[Expr] =
    add ~ rep(concatop ~ add) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  private lazy val relat : PackratParser[Expr] =
    concat ~ rep(relatop ~ concat) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  private lazy val and : PackratParser[Expr] =
    relat ~ rep(andop ~ relat) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  private lazy val or : PackratParser[Expr] =
    and ~ rep(orop ~ and) ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ _e)) => BinaryOp(op, v, _e)}}
  private lazy val exp : PackratParser[Expr] = or
  private lazy val _var : PackratParser[Expr] = (
    prefixexp ~ "[" ~ exp ~ "]" ^^ { case t ~ _ ~ key ~ _ => TableLookup(t, key)}
      | prefixexp ~ "." ~ NAME ^^ { case obj ~ _ ~ fieldNAME => FieldOf(obj, fieldNAME)}
      | NAME ^^ Variable)
  private lazy val varlist : PackratParser[List[Expr]] = rep1sep(_var, ",")
  private lazy val funcname : PackratParser[(Expr, Boolean)] =
    NAME ~ rep("." ~> NAME) ~ opt(":" ~> NAME) ^^ {
      case name ~ fields ~ method =>
        val prefix = fields.foldLeft(Variable(name) : Expr) { (e, name) => FieldOf(e, name)}
        method match {
          case Some(m) => (FieldOf(prefix, m), true)
          case None => (prefix, false)
        }
    }
  private lazy val laststat : PackratParser[Statement] = (
    "return" ~> opt(explist) ^^ (es => Return(es.getOrElse(Nil)))
      | "break" ^^ (_ => Break))
  private lazy val stat : PackratParser[Statement] = (
    "do" ~> block <~ "end"
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
      | "local" ~> namelist ~ opt("=" ~> explist) ^^ { case names ~ exprs => LocalDef(names, exprs.getOrElse(Nil))}
      | functioncall ^^ CallStatement
      | varlist ~ "=" ~ explist ^^ { case lefts ~ _ ~ rights => Assign(lefts, rights)}
      | failure("parse stat failed!"))
  private lazy val chunk : PackratParser[List[Statement]] =
    rep(stat <~ opt(";")) ~ opt(laststat <~ opt(";")) ^^ {
      case l ~ Some(last) => l ::: List(last)
      case l ~ None => l
    }
  private lazy val block : PackratParser[Block] = chunk ^^ Block

  def parse(input : String) : Any = {
    val result = phrase(chunk)(new PackratReader(new CharSequenceReader(input)))
    if (result.successful) result.get else throw new Exception(result.toString)
  }
}
