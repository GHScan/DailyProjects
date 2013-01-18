# vim:fileencoding=gbk

import re

opPriority = {'+':1, '-':1, '*':2, '/':2}
class Operator(object):
    def __init__(self, **arg):
        self.__dict__.update(**arg)
    def _hash(self):
        return (self.depth << 24) | (opPriority[self.op] << 16) | (0xffff - self.id)
    def __lt__(self, o):
        return self._hash() > o._hash()
    def __call__(self, a, b):
        if self.op == '+':
            if type(a) == str: return a + str(b)
            return a + b
        elif self.op == '-': return a - b
        elif self.op == '*': 
            if type(a) == str:
                return ''.join(a for i in range(b))
            return a * b
        elif self.op == '/': return a / b
        else : assert False

def myEval(expr):
    depth = 0
    operators, next_opid = [], 0
    values , next_vid = dict(), 0
    for m in re.finditer(r'(\d+)|(".*")|([_a-zA-Z]\w*)|([\+\-\*\/])|(\()|(\))', expr):
        if m.group(1):
            next_vid += 1
            values[next_vid] = long(m.group(1))
        elif m.group(2):
            next_vid += 1
            values[next_vid] = m.group(2)[1:-1]
        elif m.group(3):
            next_vid += 1
            values[next_vid] = globals()[m.group(3)]
        elif m.group(4):
            next_opid += 1
            operators.append(Operator(op = m.group(4), depth = depth, id = next_opid,
                lval = next_vid, rval = next_vid + 1))
        elif m.group(5):
            depth += 1
        elif m.group(6):
            depth -= 1
        else: assert False
    operators.sort()
    r = None
    for op in operators:
        r = op(values[op.lval], values[op.rval])
        values[op.lval], values[op.rval] = r, r
    return r

g_a = 3
assert myEval('(1+2)*3 / 2') == 4
assert myEval('((3+2) + 2)*4 - 1 + (3 *2)') == 33
assert myEval('15/ (g_a - 1)') == 7
assert myEval('"a"*10') == 'aaaaaaaaaa'
assert myEval('"abc" + 123') == 'abc123'
assert myEval('4+5-6+7-8') == 2
assert myEval("2*(2*(5+2) + 1) / 3") == 10 # failed! see golang version!
