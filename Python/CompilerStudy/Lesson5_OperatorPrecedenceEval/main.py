# vim:fileencoding=utf-8

import re

lbps = {'+':1, '-':1, '*':2, '/':2, '%':2, ')':0}
rbps = lbps
def _expr(tokens, rbp):
    lv = _atom(tokens)
    while len(tokens) > 0 and rbp < lbps[tokens[0]]:
        lv = _infix(tokens, lv)
    return lv
def _atom(tokens):
    t = tokens.pop(0)
    if t == '(':
        ret = _expr(tokens, 0)
        assert tokens.pop(0) == ')'
        return ret
    elif t == '-':
        return - _expr(tokens, 5)
    else: return int(t)
def _infix(tokens, lv):
    op = tokens.pop(0)
    rv = _expr(tokens, rbps[op])
    if op == '+': return lv + rv
    elif op == '-': return lv - rv
    elif op == '*': return lv * rv
    elif op == '/': return lv / rv
    elif op == '%': return lv % rv
    else: assert False
def myeval(src):
    return _expr(re.findall('\d+|\S', src), 0)

assert myeval('5-2*(1+2)') == -1
assert myeval('9-8+7*6% 5 -4 +3') == 2
assert myeval('1+2--3+(-3+5*-1)') == -2
