package demo.lua

import parsing.{ParserFactory, GrammarBuilder}

object LuaParser {

  private val grammar = new GrammarBuilder {

    import LuaScanner.ScannerBuilder.Implicits._
    import LuaAST._
    import parsing._

    def start : INonTerminalSymbol = chunk

    val Name : GenericNonTerminalSymbol[String] = nonTerm("Name" ^^ (_.asInstanceOf[String]))
    val fieldsep : GenericNonTerminalSymbol[Any] = nonTerm("," | ";")
    val field : GenericNonTerminalSymbol[(Expr, Expr)] = nonTerm(
      "[" ~ exp ~ "]" ~ "=" ~ exp ^^ { case _ ~ key ~ _ ~ _ ~ value => (key, value)}
        | Name ~ "=" ~ exp ^^ { case key ~ _ ~ value => (Const(key), value)}
        | exp ^^ { e => (Const(-1), e)})
    val fieldlist : GenericNonTerminalSymbol[List[(Expr, Expr)]] = nonTerm(
      field ~ (fieldsep ~> field).rep <~ fieldsep.opt ^^ { case head ~ tail => head :: tail})
    val tableconstructor : GenericNonTerminalSymbol[TableConstructor] = nonTerm(
      "{" ~> fieldlist.opt <~ "}" ^^ (e => TableConstructor(e.getOrElse(Nil))))
    val namelist : GenericNonTerminalSymbol[List[String]] = nonTerm(
      Name.rep1sep(","))
    val explist : GenericNonTerminalSymbol[List[Expr]] = nonTerm(
      exp.rep1sep(","))
    val parlist : GenericNonTerminalSymbol[(List[String], Boolean)] = nonTerm(
      namelist ~ ("," ~ "...").opt ^^ {
        case list ~ Some(_) => (list, true)
        case list ~ None => (list, false)
      }
        | "..." ^^ (_ => (Nil, true))
    )
    val funcbody : GenericNonTerminalSymbol[((List[String], Boolean), Block)] = nonTerm(
      ("(" ~> parlist.opt <~ ")") ~ block <~ "end" ^^ { case plist ~ block => (plist.getOrElse((Nil, false)), block)}
    )
    val function : GenericNonTerminalSymbol[Func] = nonTerm(
      "function" ~> funcbody ^^ { case ((formals, hasVarArg), body) => Func(formals, body, hasVarArg, false)}
    )
    val args : GenericNonTerminalSymbol[List[Expr]] = nonTerm(
      "(" ~> explist.opt <~ ")" ^^ (_.getOrElse(Nil))
        | tableconstructor ^^ (List(_))
        | "String" ^^ (s => List(Const(s.asInstanceOf[String]))))
    val functioncall : GenericNonTerminalSymbol[Expr] = nonTerm(
      prefixexp ~ args ^^ { case func ~ args => Call(func, args)}
        | prefixexp ~ ":" ~ Name ~ args ^^ { case obj ~ _ ~ methodName ~ args => MethodCall(obj, methodName, args)})
    val prefixexp : GenericNonTerminalSymbol[Expr] = nonTerm(
      _var
        | functioncall
        | "(" ~> exp <~ ")")
    val factor : GenericNonTerminalSymbol[Expr] = nonTerm(
      "nil" ^^ (_ => Const(null))
        | "Boolean" ^^ Const
        | "Number" ^^ Const
        | "String" ^^ Const
        | "..." ^^ (_ => VarLengthArguments)
        | function
        | prefixexp
        | tableconstructor)
    val unary : GenericNonTerminalSymbol[Expr] = nonTerm(
      ("-" | "UnaryOp").rep ~ factor ^^ { case ops ~ e => ops.foldLeft(e) { (e, op) => UnaryOp(op.asInstanceOf[String], e)}})
    val pow : GenericNonTerminalSymbol[Expr] = nonTerm(
      unary ~ ("^" ~ unary).rep ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ e)) => BinaryOp(op.asInstanceOf[String], v, e)}})
    val mul : GenericNonTerminalSymbol[Expr] = nonTerm(
      pow ~ ("MulOp" ~ pow).rep ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ e)) => BinaryOp(op.asInstanceOf[String], v, e)}})
    val add : GenericNonTerminalSymbol[Expr] = nonTerm(
      mul ~ (("+" | "-") ~ mul).rep ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ e)) => BinaryOp(op.asInstanceOf[String], v, e)}})
    val concat : GenericNonTerminalSymbol[Expr] = nonTerm(
      add ~ (".." ~ add).rep ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ e)) => BinaryOp(op.asInstanceOf[String], v, e)}})
    val relat : GenericNonTerminalSymbol[Expr] = nonTerm(
      concat ~ ("RelatOp" ~ concat).rep ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ e)) => BinaryOp(op.asInstanceOf[String], v, e)}})
    val and : GenericNonTerminalSymbol[Expr] = nonTerm(
      relat ~ ("and" ~ relat).rep ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ e)) => BinaryOp(op.asInstanceOf[String], v, e)}})
    val or : GenericNonTerminalSymbol[Expr] = nonTerm(
      and ~ ("or" ~ and).rep ^^ { case e ~ eops => eops.foldLeft(e) { case (v, (op ~ e)) => BinaryOp(op.asInstanceOf[String], v, e)}})
    val exp : GenericNonTerminalSymbol[Expr] = nonTerm(or)
    val _var : GenericNonTerminalSymbol[Expr] = nonTerm(
      Name ^^ Variable
        | prefixexp ~ "[" ~ exp ~ "]" ^^ { case t ~ _ ~ key ~ _ => TableLookup(t, key)}
        | prefixexp ~ "." ~ Name ^^ { case obj ~ _ ~ fieldName => FieldOf(obj, fieldName)})
    val varlist : GenericNonTerminalSymbol[List[Expr]] = nonTerm(
      _var.rep1sep(","))
    val funcname : GenericNonTerminalSymbol[(Expr, Boolean)] = nonTerm(
      Name ~ ("." ~> Name).rep ~ (":" ~> Name).opt ^^ {
        case name ~ fields ~ method =>
          val prefix = fields.foldLeft(Variable(name) : Expr) { (e, name) => FieldOf(e, name)}
          method match {
            case Some(m) => (FieldOf(prefix, m), true)
            case None => (prefix, false)
          }
      })
    val laststat : GenericNonTerminalSymbol[Statement] = nonTerm(
      "return" ~> explist.opt ^^ (es => Return(es.getOrElse(Nil)))
        | "break" ^^ (_ => Break))
    val stat : GenericNonTerminalSymbol[Statement] = nonTerm(
      varlist ~ "=" ~ explist ^^ { case lefts ~ _ ~ rights => Assignments(lefts, rights)}
        | functioncall ^^ CallStatement
        | "do" ~> block <~ "end"
        | "while" ~> exp ~ ("do" ~> block <~ "end") ^^ { case e ~ block => While(e, block)}
        | "repeat" ~> block ~ ("until" ~> exp) ^^ { case block ~ e => Repeat(block, e)}
        | ("if" ~> exp ~ ("then" ~> block)) ~ ("elseif" ~> exp ~ ("then" ~> block)).rep ~ ("else" ~> block).opt <~ "end" ^^ {
        case case1 ~ cases ~ fallback => Cond((case1 :: cases).map { case e ~ b => (e, b)}, fallback.orNull)
      }
        | ("for" ~> Name <~ "=") ~ ((exp <~ ",") ~ exp ~ ("," ~> exp).opt) ~ ("do" ~> block <~ "end") ^^ {
        case name ~ (first ~ last ~ step) ~ block => RangeFor(name, first, last, step.orNull, block)
      }
        | ("for" ~> namelist <~ "in") ~ explist ~ ("do" ~> block <~ "end") ^^ {
        case names ~ initExpr ~ body => IterateFor(names, initExpr, body)
      }
        | "function" ~> funcname ~ funcbody ^^ { case (left, hasSelf) ~ (((formals, hasVarArg), body)) => Assignments(List(left), List(Func(formals, body, hasVarArg, hasSelf)))}
        | "local" ~> "function" ~> Name ~ funcbody ^^ { case name ~ (((formals, hasVarArg), body)) => LocalDefines(List(name), List(Func(formals, body, hasVarArg, false)))}
        | "local" ~> namelist ~ ("=" ~> explist).opt ^^ { case names ~ exprs => LocalDefines(names, exprs.getOrElse(Nil))})
    val chunk : GenericNonTerminalSymbol[List[Statement]] = nonTerm(
      (stat <~ ";".opt).rep ~ (laststat <~ ";".opt).opt ^^ {
        case l ~ Some(last) => l ::: List(last)
        case l ~ None => l
      })
    val block : GenericNonTerminalSymbol[Block] = nonTerm(chunk ^^ Block)

  }.result

}

class LuaParser(parserType : String) {
  val parser = ParserFactory.get(parserType).create(LuaParser.grammar, false)

  def parse(source : String) : Any = {
    val scanner = LuaScanner(source)
    val result = parser.parse(scanner)
    if (parser.errors != Nil) {
      val message = parser.errors.mkString("\n")
      parser.errors = Nil
      throw new Exception(message)
    }
    result
  }
}
