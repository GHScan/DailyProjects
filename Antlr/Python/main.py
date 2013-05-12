# vim:fileencoding=gbk

import sys
import antlr3
import antlr3.tree
from ExprLexer import ExprLexer
from ExprParser import ExprParser
from TreeExpr import TreeExpr

istream = antlr3.ANTLRInputStream(sys.stdin)
lexer = ExprLexer(istream)
parser = ExprParser(antlr3.CommonTokenStream(lexer))
nstream = antlr3.tree.CommonTreeNodeStream(parser.prog().getTree())
walker = TreeExpr(nstream)
walker.prog()
