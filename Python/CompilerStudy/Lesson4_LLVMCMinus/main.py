# vim:fileencoding=gbk

import argparse
import sys
import antlr3
import antlr3.tree
from CMinusLexer import CMinusLexer
from CMinusParser import CMinusParser
from llvmCompiler import LLVMCompiler

parser = argparse.ArgumentParser()
parser.add_argument('filename', help='the name of script file')
parser.add_argument('-O', '--optimize', action='store_true', help='optimize the bytecode')
parser.add_argument('-v', '--verbose', action='store_true', help='output the bytecode to ll file')
args = parser.parse_args()

istream = antlr3.ANTLRFileStream(args.filename, 'utf-8')
lexer = CMinusLexer(istream)
parser = CMinusParser(antlr3.CommonTokenStream(lexer))
meta = parser.program()

compiler = LLVMCompiler(meta)
compiler.compile(args.optimize)
if args.verbose: 
    compiler.dump(args.filename + '.ll')
compiler.run()
