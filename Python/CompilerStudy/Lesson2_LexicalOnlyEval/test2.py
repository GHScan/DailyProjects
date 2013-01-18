#!/usr/bin/env python

import re

def op_add(a, b):
    if type(a) == str: return a + str(b)
    return a + b
def op_sub(a, b): return a - b
def op_mul(a, b):
    if type(a) == str: return ''.join(a for i in range(b))
    return a * b
def op_div(a, b): return a / b
def op_power(a, b): return a ** b
opFuncs = { '+':op_add, '-':op_sub, '*':op_mul, '/':op_div, '!': op_power}
opPriority = {'+':1, '-':1, '*':2, '/':2, '!': 3}

def evalTokens(tokens):
    if len(tokens) == 1: return tokens[0]
    midx, mop = 1, tokens[1]
    for i in range(3, len(tokens), 2):
        if tokens[i][1] < mop[1]: 
            midx, mop = i, tokens[i]
    return opFuncs[mop[0]](evalTokens(tokens[:midx]), evalTokens(tokens[midx + 1:]))

def myEval(expr):
    depth, tokens = 0, []
    for i, m in enumerate(
            re.finditer(r'(\d+)|([_a-zA-Z]\w*)|(".*")|([\+\-\*\/\!])|(\()|(\))', expr)):
        if m.group(1):
            tokens.append(long(m.group(1)))
        elif m.group(2):
            tokens.append(globals()[m.group(2)])
        elif m.group(3):
            tokens.append(m.group(3)[1:-1])
        elif m.group(4):
            tokens.append([m.group(4), (depth << 24) | (opPriority[m.group(4)] << 16) | (0xffff - i)])
        elif m.group(5): depth += 1
        elif m.group(6): depth -= 1
        else: assert False
    return evalTokens(tokens)

g_a = 10
print myEval('2 + (13-(2+3) ) * 5')
print myEval('"abc"*5')
print myEval('"abc" +(1+3+5-2)')
print myEval('2!10-3')
print myEval('"a"*(g_a / 2)+g_a')
