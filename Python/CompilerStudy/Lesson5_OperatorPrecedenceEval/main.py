# vim:fileencoding=utf-8

import re

PRIORS = {'+':1, '-':1, '*':2, '/':2, '%':2, ')':0}
def _atom(tokens):
    if tokens[0] == '(':
        tokens.pop(0)
        r = _expr(tokens, 0)
        tokens.pop(0)
    else:
        r = int(tokens[0])
        tokens.pop(0)
    return r
def _infix(tokens, lv):
    t = tokens[0]
    tokens.pop(0)
    rv = _expr(tokens, PRIORS[t])
    if t == '+': return lv + rv
    elif t == '-': return lv - rv
    elif t == '*': return lv * rv
    elif t == '/': return lv / rv
    elif t == '%': return lv % rv
    else: assert False
def _expr(tokens, prior):
    v = _atom(tokens)
    while len(tokens) > 0 and prior < PRIORS[tokens[0]]:
        v = _infix(tokens, v)
    return v

def myeval(src):
    tokens = re.findall(r'\d+|\S', src)
    return _expr(tokens, 0)

print myeval('5-2*(1+2)')
print myeval('9-8+7*6% 5 -4 +3')
