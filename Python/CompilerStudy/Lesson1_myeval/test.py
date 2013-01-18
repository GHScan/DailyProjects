# vim:fileencoding=gbk

import re

class TokenType(object):
    operator = 1
    value = 2
class Token(object):
    __slots__ = ['type', 'value']
    def __init__(self, t, val):
        self.type = t
        self.value = val

def op_add(a, b):
    if type(a) == str:
        return a + str(b)
    return a + b
def op_sub(a, b):
    return a -b
def op_mul(a, b):
    if type(a) == str:
        return ''.join(a for i in range(b))
    return a * b
def op_div(a, b):
    return a / b

opPriority = {'+':1, '-':1, '*':2, '/':2}
opFuncs = {'+':op_add, '-':op_sub, '*':op_mul, '/':op_div}

def lexicalAnalysis(expr):
    tokens = []
    for m in re.finditer(
            r'(\d+)|("[^"]*")|([_a-zA-Z]\w*)|([\+\-\*/\(\)])', expr):
        if m.group(1):
            tokens.append(Token(TokenType.value, long(m.group(1))))
        elif m.group(2):
            tokens.append(Token(TokenType.value, m.group(2)[1:-1]))
        elif m.group(3):
            tokens.append(Token(TokenType.value, globals()[m.group(3)]))
        elif m.group(4):
            tokens.append(Token(TokenType.operator, m.group(4)))
        else:
            assert nil
    return tokens

def evalTokens(tokens):
    rStack, opStack = [], []
    # infix to suffix
    for token in tokens:
        if token.type == TokenType.value:
            rStack.append(token)
        else:
            if token.value == '(':
                opStack.append(token)
            elif token.value == ')':
                while opStack[len(opStack) - 1].value != '(':
                    rStack.append(opStack.pop())
                opStack.pop()
            else:
                while len(opStack) > 0:
                    last = opStack[len(opStack) - 1]
                    if last.value == '(' or opPriority[last.value] < opPriority[token.value]:
                        break
                    rStack.append(opStack.pop())
                opStack.append(token)
    while len(opStack) > 0:
        rStack.append(opStack.pop())
    # eval
    while len(rStack) > 1:
        for i in range(len(rStack) - 1, 1, -1):
            opToken, valToken1, valToken2 = rStack[i], rStack[i - 2], rStack[i - 1]
            if opToken.type == TokenType.value: continue
            if valToken1.type == TokenType.operator: continue
            if valToken2.type == TokenType.operator: continue
            rStack[i - 2] = Token(TokenType.value, 
                    opFuncs[opToken.value](valToken1.value, valToken2.value))
            rStack.pop(i)
            rStack.pop(i - 1)
    return rStack[0].value

def myeval(expr):
    return evalTokens(lexicalAnalysis(expr))

assert __name__ == '__main__'
g_a = 2
g_b = 'abc'
while True:
    s = raw_input()
    if len(s) == 0: break
    print myeval(s)
