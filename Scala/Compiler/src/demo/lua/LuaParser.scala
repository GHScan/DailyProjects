package demo.lua

import parsing.{ParserFactory, GrammarBuilder}

object LuaParser {

  private val grammar = new GrammarBuilder {

    import LuaScanner.ScannerBuilder.Implicits._
    import LuaAST._
    import parsing._

    def start : INonTerminalSymbol = chunk

    val Name : GenericNonTerminalSymbol[String] = nonTerm("Name".as[String])
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
      ("(" ~> parlist.opt <~ ")") ~ block <~ "end" ^^ { case plist ~ _block => (plist.getOrElse((Nil, false)), _block)}
    )
    val function : GenericNonTerminalSymbol[Func] = nonTerm(
      "function" ~> funcbody ^^ { case ((formals, hasVarArg), body) => Func(formals, body, hasVarArg, false)}
    )
    val args : GenericNonTerminalSymbol[List[Expr]] = nonTerm(
      "(" ~> explist.opt <~ ")" ^^ (_.getOrElse(Nil))
        | tableconstructor ^^ (List(_))
        | "String" ^^ (s => List(Const(s.asInstanceOf[String]))))
    val functioncall : GenericNonTerminalSymbol[Expr] = nonTerm(
      prefixexp ~ args ^^ { case func ~ _args => Call(func, _args)}
        | prefixexp ~ ":" ~ Name ~ args ^^ { case obj ~ _ ~ methodName ~ _args => MethodCall(obj, methodName, _args)})
    lazy val prefixexp : GenericNonTerminalSymbol[Expr] = nonTerm(
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
        | tableconstructor
        | ("-" | "UnaryOp") ~ factor ^^ { case op ~ e => UnaryOp(op.asInstanceOf[String], e)})
    lazy val exp : GenericNonTerminalSymbol[Expr] = nonTerm(
      factor
        | exp ~ ("^" | "MulOp" | "+" | "-" | ".." | "RelatOp" | "and" | "or") ~ exp ^^ { case a ~ op ~ b => BinaryOp(op.asInstanceOf[String], a, b)})
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
        | "while" ~> exp ~ ("do" ~> block <~ "end") ^^ { case e ~ _block => While(e, _block)}
        | "repeat" ~> block ~ ("until" ~> exp) ^^ { case _block ~ e => Repeat(_block, e)}
        | ("if" ~> exp ~ ("then" ~> block)) ~ ("elseif" ~> exp ~ ("then" ~> block)).rep ~ ("else" ~> block).opt <~ "end" ^^ {
        case case1 ~ cases ~ fallback => Cond((case1 :: cases).map { case e ~ b => (e, b)}, fallback.orNull)
      }
        | ("for" ~> Name <~ "=") ~ ((exp <~ ",") ~ exp ~ ("," ~> exp).opt) ~ ("do" ~> block <~ "end") ^^ {
        case name ~ (first ~ last ~ step) ~ _block => RangeFor(name, first, last, step.orNull, _block)
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
    lazy val block : GenericNonTerminalSymbol[Block] = nonTerm(chunk ^^ Block)

    override def terminalsSymbol2Attribute = List[(List[TerminalSymbol], Associativity.Value)](
      (List("or"), Associativity.Left),
      (List("and"), Associativity.Left),
      (List("RelatOp"), Associativity.Left),
      (List(".."), Associativity.Left),
      (List("+", "-"), Associativity.Left),
      (List("MulOp"), Associativity.Left),
      (List("^"), Associativity.Left))

  }.result
}

class LuaParser(parserType : String) {
  val parser = ParserFactory.get(parserType).create(LuaParser.grammar, false)

  def parse(source : String) : Any = {
    val scanner = LuaScanner.create(source)
    val result = parser.parse(scanner)
    if (parser.errors != Nil) {
      val message = parser.errors.mkString("\n")
      parser.errors = Nil
      throw new Exception(message)
    }
    result
  }
}
