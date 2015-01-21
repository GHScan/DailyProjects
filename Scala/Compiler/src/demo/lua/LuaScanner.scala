package demo.lua

import lexical._

object LuaScanner {

  val ScannerBuilder = new TableDrivenScannerBuilder()
    .literals(";", "=", ",", ".", ":", "...", "(", ")", "{", "}", "[", "]", "+", "-", "^", "..")
    .literals("and", "or", "break", "do", "else", "elseif", "end", "for", "function", "if", "in", "local", "nil", "repeat", "return", "then", "until", "while")
    .token("WS", """\s+""", IgnoreHandler)
    .token("Comment", """--[^\n]*|\[\[([^\]]|\][^\]])*\]\]|\[=\[([^\]]|\][^\]])*\]=\]""", IgnoreHandler)
    .token("UnaryOp", """#|not""", identity)
    .token("RelatOp", """<|<=|>|>=|==|~=""", identity)
    .token("MulOp", """\*|\/|\%""", identity)
    .token("Boolean", """true|false""", _ == "true")
    .token("Name", """[a-zA-Z_]\w*""", identity)
    .token("Number", """((\d+)?\.)?\d+""", _.toDouble)
    .token("String", """"(\\.|[^"])*"|'(\\.|[^'])*'""", utils.Func.unescape)

  def create(source : String) = ScannerBuilder.create(new StringCharSource(source), new FastTokenFactory)
}