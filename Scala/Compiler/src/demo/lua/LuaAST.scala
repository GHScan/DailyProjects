package demo.lua

object LuaAST {

  sealed abstract class Tree

  sealed abstract class Expr extends Tree
  case class Variable(name : String) extends Expr
  case object VarArgument extends Expr
  case class Constant(value : Any) extends Expr
  case class MethodCall(obj : Expr, methodName : String, args : List[Expr]) extends Expr
  case class FieldOf(obj : Expr, fieldName : String) extends Expr
  case class TableLookup(table : Expr, key : Expr) extends Expr
  case class UnaryOp(op : String, expr : Expr) extends Expr
  case class BinaryOp(op : String, left : Expr, right : Expr) extends Expr
  case class Call(func : Expr, args : List[Expr]) extends Expr
  case class Func(formals : List[String], body : Block, hasVarArg : Boolean, hasSelf : Boolean) extends Expr
  case class TableConstructor(keyValues : List[(Expr, Expr)]) extends Expr

  sealed abstract class Statement extends Tree
  case class Block(statements : List[Statement]) extends Statement
  case object Break extends Statement
  case class Return(values : List[Expr]) extends Statement
  case class Assign(lefts : List[Expr], rights : List[Expr]) extends Statement
  case class LocalDef(names : List[String], rights : List[Expr]) extends Statement
  case class CallStatement(call : Expr) extends Statement
  case class While(cond : Expr, body : Block) extends Statement
  case class Repeat(body : Block, cond : Expr) extends Statement
  case class Cond(cases : List[(Expr, Block)], fallback : Block) extends Statement
  case class RangeFor(name : String, first : Expr, last : Expr, step : Expr, body : Block) extends Statement
  case class IterateFor(names : List[String], initExprs : List[Expr], body : Block) extends Statement
}
