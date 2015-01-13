package demo.lua

import lexical.{StringCharSource, TableDrivenScannerBuilder}

object LuaScanner {

  val ScannerBuilder = new TableDrivenScannerBuilder()
    .literals(";", "=", ",", ".", ":", "...", "(", ")", "{", "}", "[", "]", "+", "-", "^", "..")
    .literals("and", "or", "break", "do", "else", "elseif", "end", "for", "function", "if", "in", "local", "nil", "repeat", "return", "then", "until", "while")
    .token("WS", """\s+""", _ => null)
    .token("Comment", """\-\-[^\n]*|\[\[([^\]]|\][^\]])*\]\]|\[=\[([^\]]|\][^\]])*\]=\]""", _ => null)
    .token("UnaryOp", """#|not""", identity)
    .token("RelatOp", """<|<=|>|>=|==|~=""", identity)
    .token("MulOp", """\*|\/|\%""", identity)
    .token("Boolean", """true|false""", _ == true)
    .token("Name", """[a-zA-Z_]\w*""", identity)
    .token("Number", """((\d+)?\.)?\d+""", _.toDouble)
    .token("String", """"(\\.|[^"])*"|'(\\.|[^'])*'""", utils.Func.unescape)

  val CommentToken = ScannerBuilder.getToken("Comment")
  val WSToken = ScannerBuilder.getToken("WS")

  def apply(source : String) = ScannerBuilder.create(new StringCharSource(source)).filter(t => t != CommentToken && t != WSToken)
}