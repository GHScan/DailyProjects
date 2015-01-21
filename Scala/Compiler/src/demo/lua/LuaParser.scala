package demo.lua

import parsing.{ParserFactory, GrammarBuilder}

object LuaParser {

  private val lrGrammar = new GrammarBuilder {

    import LuaScanner.ScannerBuilder.Implicits._
    import LuaAST._
    import parsing._

    def start : INonTerminalSymbol = chunk

    val Name : GenericNonTerminalSymbol[String] = nonTerm("Name".as[String])
    val fieldsep : GenericNonTerminalSymbol[Any] = nonTerm("," | ";")
    val field : GenericNonTerminalSymbol[(Expr, Expr)] = nonTerm(
      "[" ~ exp ~ "]" ~ "=" ~ exp ^^ { case _ ~ key ~ _ ~ _ ~ value => (key, value)}
        | Name ~ "=" ~ exp ^^ { case key ~ _ ~ value => (Constant(key), value)}
        | exp ^^ { e => (Constant(-1), e)})
    val fieldlist : GenericNonTerminalSymbol[List[(Expr, Expr)]] = nonTerm(
      field ~ rep(fieldsep ~> field) <~ opt(fieldsep) ^^ { case head ~ tail => head :: tail})
    val tableconstructor : GenericNonTerminalSymbol[TableConstructor] = nonTerm(
      "{" ~> opt(fieldlist) <~ "}" ^^ (e => TableConstructor(e.getOrElse(Nil))))
    val namelist : GenericNonTerminalSymbol[List[String]] = nonTerm(
      rep1sep(Name, ","))
    val explist : GenericNonTerminalSymbol[List[Expr]] = nonTerm(
      rep1sep(exp, ","))
    val parlist : GenericNonTerminalSymbol[(List[String], Boolean)] = nonTerm(
      namelist ~ opt("," ~ "...") ^^ {
        case list ~ Some(_) => (list, true)
        case list ~ None => (list, false)
      }
        | "..." ^^ (_ => (Nil, true))
    )
    val funcbody : GenericNonTerminalSymbol[((List[String], Boolean), Block)] = nonTerm(
      ("(" ~> opt(parlist) <~ ")") ~ block <~ "end" ^^ { case plist ~ _block => (plist.getOrElse((Nil, false)), _block)}
    )
    val function : GenericNonTerminalSymbol[Func] = nonTerm(
      "function" ~> funcbody ^^ { case ((formals, hasVarArg), body) => Func(formals, body, hasVarArg, hasSelf = false)}
    )
    val args : GenericNonTerminalSymbol[List[Expr]] = nonTerm(
      "(" ~> opt(explist) <~ ")" ^^ (_.getOrElse(Nil))
        | tableconstructor ^^ (List(_))
        | "String" ^^ (s => List(Constant(s.asInstanceOf[String]))))
    val functioncall : GenericNonTerminalSymbol[Expr] = nonTerm(
      prefixexp ~ args ^^ { case func ~ _args => Call(func, _args)}
        | prefixexp ~ ":" ~ Name ~ args ^^ { case obj ~ _ ~ methodName ~ _args => MethodCall(obj, methodName, _args)})
    lazy val prefixexp : GenericNonTerminalSymbol[Expr] = nonTerm(
      _var
        | functioncall
        | "(" ~> exp <~ ")")
    val factor : GenericNonTerminalSymbol[Expr] = nonTerm(
      "nil" ^^ (_ => Constant(null))
        | "Boolean" ^^ Constant
        | "Number" ^^ Constant
        | "String" ^^ Constant
        | "..." ^^ (_ => VarArgument)
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
      rep1sep(_var, ","))
    val funcname : GenericNonTerminalSymbol[(Expr, Boolean)] = nonTerm(
      Name ~ rep("." ~> Name) ~ opt(":" ~> Name) ^^ {
        case name ~ fields ~ method =>
          val prefix = fields.foldLeft(Variable(name) : Expr) { (e, name) => FieldOf(e, name)}
          method match {
            case Some(m) => (FieldOf(prefix, m), true)
            case None => (prefix, false)
          }
      })
    val laststat : GenericNonTerminalSymbol[Statement] = nonTerm(
      "return" ~> opt(explist) ^^ (es => Return(es.getOrElse(Nil)))
        | "break" ^^ (_ => Break))
    val stat : GenericNonTerminalSymbol[Statement] = nonTerm(
      varlist ~ "=" ~ explist ^^ { case lefts ~ _ ~ rights => Assign(lefts, rights)}
        | functioncall ^^ CallStatement
        | "do" ~> block <~ "end"
        | "while" ~> exp ~ ("do" ~> block <~ "end") ^^ { case e ~ _block => While(e, _block)}
        | "repeat" ~> block ~ ("until" ~> exp) ^^ { case _block ~ e => Repeat(_block, e)}
        | ("if" ~> exp ~ ("then" ~> block)) ~ rep("elseif" ~> exp ~ ("then" ~> block)) ~ opt("else" ~> block) <~ "end" ^^ {
        case case1 ~ cases ~ fallback => Cond((case1 :: cases).map { case e ~ b => (e, b)}, fallback.orNull)
      }
        | ("for" ~> Name <~ "=") ~ ((exp <~ ",") ~ exp ~ opt("," ~> exp)) ~ ("do" ~> block <~ "end") ^^ {
        case name ~ (first ~ last ~ step) ~ _block => RangeFor(name, first, last, step.orNull, _block)
      }
        | ("for" ~> namelist <~ "in") ~ explist ~ ("do" ~> block <~ "end") ^^ {
        case names ~ initExpr ~ body => IterateFor(names, initExpr, body)
      }
        | "function" ~> funcname ~ funcbody ^^ { case (left, hasSelf) ~ (((formals, hasVarArg), body)) => Assign(List(left), List(Func(formals, body, hasVarArg, hasSelf)))}
        | "local" ~> "function" ~> Name ~ funcbody ^^ { case name ~ (((formals, hasVarArg), body)) => LocalDef(List(name), List(Func(formals, body, hasVarArg, hasSelf = false)))}
        | "local" ~> namelist ~ opt("=" ~> explist) ^^ { case names ~ exprs => LocalDef(names, exprs.getOrElse(Nil))})
    val chunk : GenericNonTerminalSymbol[List[Statement]] = nonTerm(
      rep(stat <~ opt(";")) ~ opt(laststat <~ opt(";")) ^^ {
        case l ~ Some(last) => l ::: List(last)
        case l ~ None => l
      })
    lazy val block : GenericNonTerminalSymbol[Block] = nonTerm(chunk ^^ Block)

    override def terminalSymbol2Attribute = List[(List[TerminalSymbol], Associativity.Value)](
      (List("or"), Associativity.Left),
      (List("and"), Associativity.Left),
      (List("RelatOp"), Associativity.Left),
      (List(".."), Associativity.Left),
      (List("+", "-"), Associativity.Left),
      (List("MulOp"), Associativity.Left),
      (List("^"), Associativity.Left))

    override def syncWord2ErrorRecoveryAction = Map[lexical.Token, ErrorRecoveryAction]()
      .updated("end", ErrorRecoveryAction(block.name, _ => Block(Nil), consumeSyncWord = false))
      .updated("elseif", ErrorRecoveryAction(block.name, _ => Block(Nil), consumeSyncWord = false))
      .updated("else", ErrorRecoveryAction(block.name, _ => Block(Nil), consumeSyncWord = false))
      .updated("until", ErrorRecoveryAction(block.name, _ => Block(Nil), consumeSyncWord = false))
      .updated(";", ErrorRecoveryAction(stat.name, _ => Break, consumeSyncWord = false))

  }.result

  private val llGrammar = new GrammarBuilder {

    import LuaScanner.ScannerBuilder.Implicits._
    import LuaAST._
    import parsing._

    def start : INonTerminalSymbol = chunk

    val Name : GenericNonTerminalSymbol[String] = nonTerm("Name".as[String])
    val fieldsep : GenericNonTerminalSymbol[Any] = nonTerm("," | ";")
    val field : GenericNonTerminalSymbol[(Expr, Expr)] = nonTerm(
      "[" ~ exp ~ "]" ~ "=" ~ exp ^^ { case _ ~ key ~ _ ~ _ ~ value => (key, value)}
        | exp ^^ { e => (Constant(-1), e)}
        | Name ~ "=" ~ exp ^^ { case key ~ _ ~ value => (Constant(key), value)})
    val fieldlist : GenericNonTerminalSymbol[List[(Expr, Expr)]] = nonTerm(
      field ~ rep(fieldsep ~> field) <~ opt(fieldsep) ^^ { case head ~ tail => head :: tail})
    val tableconstructor : GenericNonTerminalSymbol[TableConstructor] = nonTerm(
      "{" ~> opt(fieldlist) <~ "}" ^^ (e => TableConstructor(e.getOrElse(Nil))))
    val namelist : GenericNonTerminalSymbol[List[String]] = nonTerm(
      rep1sep(Name, ","))
    val explist : GenericNonTerminalSymbol[List[Expr]] = nonTerm(
      rep1sep(exp, ","))
    val parlist : GenericNonTerminalSymbol[(List[String], Boolean)] = nonTerm(
      "..." ^^ (_ => (Nil, true))
        | namelist ~ opt("," ~ "...") ^^ {
        case list ~ Some(_) => (list, true)
        case list ~ None => (list, false)
      })
    val funcbody : GenericNonTerminalSymbol[((List[String], Boolean), Block)] = nonTerm(
      ("(" ~> opt(parlist) <~ ")") ~ block <~ "end" ^^ { case plist ~ _block => (plist.getOrElse((Nil, false)), _block)}
    )
    val function : GenericNonTerminalSymbol[Func] = nonTerm(
      "function" ~> funcbody ^^ { case ((formals, hasVarArg), body) => Func(formals, body, hasVarArg, hasSelf = false)}
    )
    val args : GenericNonTerminalSymbol[List[Expr]] = nonTerm(
      "(" ~> opt(explist) <~ ")" ^^ (_.getOrElse(Nil))
        | tableconstructor ^^ (List(_))
        | "String" ^^ (s => List(Constant(s.asInstanceOf[String]))))
    val functioncall : GenericNonTerminalSymbol[Expr] = nonTerm(
      prefixexp ~ args ^^ { case func ~ _args => Call(func, _args)}
        | prefixexp ~ ":" ~ Name ~ args ^^ { case obj ~ _ ~ methodName ~ _args => MethodCall(obj, methodName, _args)})
    lazy val prefixexp : GenericNonTerminalSymbol[Expr] = nonTerm(
      "(" ~> exp <~ ")"
        | functioncall
        | _var)
    val factor : GenericNonTerminalSymbol[Expr] = nonTerm(
      "nil" ^^ (_ => Constant(null))
        | "Boolean" ^^ Constant
        | "Number" ^^ Constant
        | "String" ^^ Constant
        | "..." ^^ (_ => VarArgument)
        | function
        | tableconstructor
        | prefixexp)
    val unary : GenericNonTerminalSymbol[Expr] = nonTerm(
      rep("-" | "UnaryOp") ~ factor ^^ { case ops ~ e => ops.foldLeft(e) { case (v, op) => UnaryOp(op.asInstanceOf[String], v)}}
    )
    val pow : GenericNonTerminalSymbol[Expr] = nonTerm(
      unary ~ rep("^" ~ unary) ^^ { case e ~ opes => opes.foldLeft(e) { case (v, op ~ e2) => BinaryOp(op.asInstanceOf[String], v, e2)}}
    )
    val mul : GenericNonTerminalSymbol[Expr] = nonTerm(
      pow ~ rep("MulOp" ~ pow) ^^ { case e ~ opes => opes.foldLeft(e) { case (v, op ~ e2) => BinaryOp(op.asInstanceOf[String], v, e2)}}
    )
    val add : GenericNonTerminalSymbol[Expr] = nonTerm(
      mul ~ rep(("+" | "-") ~ mul) ^^ { case e ~ opes => opes.foldLeft(e) { case (v, op ~ e2) => BinaryOp(op.asInstanceOf[String], v, e2)}}
    )
    val concat : GenericNonTerminalSymbol[Expr] = nonTerm(
      add ~ rep(".." ~ add) ^^ { case e ~ opes => opes.foldLeft(e) { case (v, op ~ e2) => BinaryOp(op.asInstanceOf[String], v, e2)}}
    )
    val relat : GenericNonTerminalSymbol[Expr] = nonTerm(
      concat ~ rep("RelatOp" ~ concat) ^^ { case e ~ opes => opes.foldLeft(e) { case (v, op ~ e2) => BinaryOp(op.asInstanceOf[String], v, e2)}}
    )
    val and : GenericNonTerminalSymbol[Expr] = nonTerm(
      relat ~ rep("and" ~ relat) ^^ { case e ~ opes => opes.foldLeft(e) { case (v, op ~ e2) => BinaryOp(op.asInstanceOf[String], v, e2)}}
    )
    lazy val exp : GenericNonTerminalSymbol[Expr] = nonTerm(
      and ~ rep("or" ~ and) ^^ { case e ~ opes => opes.foldLeft(e) { case (v, op ~ e2) => BinaryOp(op.asInstanceOf[String], v, e2)}})
    val _var : GenericNonTerminalSymbol[Expr] = nonTerm(
      prefixexp ~ "[" ~ exp ~ "]" ^^ { case t ~ _ ~ key ~ _ => TableLookup(t, key)}
        | prefixexp ~ "." ~ Name ^^ { case obj ~ _ ~ fieldName => FieldOf(obj, fieldName)}
        | Name ^^ Variable)
    val varlist : GenericNonTerminalSymbol[List[Expr]] = nonTerm(
      rep1sep(_var, ","))
    val funcname : GenericNonTerminalSymbol[(Expr, Boolean)] = nonTerm(
      Name ~ rep("." ~> Name) ~ opt(":" ~> Name) ^^ {
        case name ~ fields ~ method =>
          val prefix = fields.foldLeft(Variable(name) : Expr) { (e, name) => FieldOf(e, name)}
          method match {
            case Some(m) => (FieldOf(prefix, m), true)
            case None => (prefix, false)
          }
      })
    val laststat : GenericNonTerminalSymbol[Statement] = nonTerm(
      "return" ~> opt(explist) ^^ (es => Return(es.getOrElse(Nil)))
        | "break" ^^ (_ => Break))
    val stat : GenericNonTerminalSymbol[Statement] = nonTerm(
      "do" ~> block <~ "end"
        | "while" ~> exp ~ ("do" ~> block <~ "end") ^^ { case e ~ _block => While(e, _block)}
        | "repeat" ~> block ~ ("until" ~> exp) ^^ { case _block ~ e => Repeat(_block, e)}
        | ("if" ~> exp ~ ("then" ~> block)) ~ rep("elseif" ~> exp ~ ("then" ~> block)) ~ opt("else" ~> block) <~ "end" ^^ {
        case case1 ~ cases ~ fallback => Cond((case1 :: cases).map { case e ~ b => (e, b)}, fallback.orNull)
      }
        | ("for" ~> Name <~ "=") ~ ((exp <~ ",") ~ exp ~ opt("," ~> exp)) ~ ("do" ~> block <~ "end") ^^ {
        case name ~ (first ~ last ~ step) ~ _block => RangeFor(name, first, last, step.orNull, _block)
      }
        | ("for" ~> namelist <~ "in") ~ explist ~ ("do" ~> block <~ "end") ^^ {
        case names ~ initExpr ~ body => IterateFor(names, initExpr, body)
      }
        | "function" ~> funcname ~ funcbody ^^ { case (left, hasSelf) ~ (((formals, hasVarArg), body)) => Assign(List(left), List(Func(formals, body, hasVarArg, hasSelf)))}
        | "local" ~> "function" ~> Name ~ funcbody ^^ { case name ~ (((formals, hasVarArg), body)) => LocalDef(List(name), List(Func(formals, body, hasVarArg, hasSelf = false)))}
        | "local" ~> namelist ~ opt("=" ~> explist) ^^ { case names ~ exprs => LocalDef(names, exprs.getOrElse(Nil))}
        | varlist ~ "=" ~ explist ^^ { case lefts ~ _ ~ rights => Assign(lefts, rights)}
        | functioncall ^^ CallStatement)
    val chunk : GenericNonTerminalSymbol[List[Statement]] = nonTerm(
      rep(stat <~ opt(";")) ~ opt(laststat <~ opt(";")) ^^ {
        case l ~ Some(last) => l ::: List(last)
        case l ~ None => l
      })
    lazy val block : GenericNonTerminalSymbol[Block] = nonTerm(chunk ^^ Block)

  }.result
}

class LuaParser(val name : String) {
  val parser = {
    val factory = ParserFactory.get(name)
    factory.create(if (factory.ll) LuaParser.llGrammar else LuaParser.lrGrammar, reportConflict = false)
  }

  def parse(source : String) : Any = {
    val result = parser.parse(LuaScanner.create(source))
    if (parser.errors.nonEmpty) throw new Exception(parser.errors.mkString("\n"))
    else result
  }
}
