#! vim:fileencoding=utf-8

import re, math, time, random, sys
#------------------------------
def printSExp(s):
    print s
#------------------------------
class Env(object):
    def __init__(self, preEnv, initFrame):
        self.preEnv = preEnv
        self.frame = initFrame
    def lookup(self, name):
        if name in self.frame:
            return self.frame[name]
        else:
            assert self.preEnv, 'Cant find variable: "%s"' % name
            return self.preEnv.lookup(name)
    def define(self, name, value):
        self.frame[name] = value
        return None
    def set(self, name, value):
        if name in self.frame:
            self.frame[name] = value
            return None
        else:
            assert self.preEnv, 'Cant find variable: "%s"' % name
            return self.preEnv.set(name, value)

def tokenize(s):
    s = re.sub(r';[^\n]*\n', '', s)
    s = s.replace('(', ' ( ').replace(')', ' ) ')
    s = s.replace('[', ' ( ').replace(']', ' ) ')
    return s.split()

def parse(tokens):
    if not tokens: 
        return None
    elif tokens[0] == '(':
        l = []
        tokens.pop(0)
        while tokens[0] != ')':
            l.append(parse(tokens))
        tokens.pop(0)
        return l
    elif re.match(r'\d+', tokens[0]):
        return int(tokens.pop(0))
    elif re.match(r'\d*\.\d+', tokens[0]):
        return float(tokens.pop(0))
    else:
        return tokens.pop(0)

def evalSequentially(env, expSeq, k):
    return eval(env, expSeq[0], k) if len(expSeq) == 1 else eval(env, expSeq[0], lambda _: evalSequentially(env, expSeq[1:], k))

def evalList(env, expList, k, result = []):
    return eval(env, expList[0], lambda v: evalList(env, expList[1:], k, result + [v])) if expList else (result, k)

def eval(env, exp, k):
    if isinstance(exp, str):
        return env.lookup(exp), k
    elif not isinstance(exp, list):
        return exp, k
    elif exp[0] == 'quote':
        (_, datum) = exp
        return datum, k
    elif exp[0] == 'if':
        (_, predExp, thenExp, elseExp) = exp
        return eval(env, predExp, lambda b: eval(env, thenExp, k) if b else eval(env, elseExp, k))
    elif exp[0] == 'lambda':
        (_, formals), expList = exp[:2], exp[2:]
        return (lambda actuals, k2: evalSequentially(Env(env, dict(zip(formals, actuals))), expList, k2)), k
    elif exp[0] == 'begin':
        _, expList = exp[0], exp[1:]
        return evalSequentially(env, expList, k)
    elif exp[0] == 'cond':
        (_, case), caseList = exp[:2], exp[2:]
        if caseList:
            casePred, caseExpList = case[0], case[1:]
            return eval(env, ['if', casePred, ['begin'] + caseExpList, ['cond'] + caseList], k)
        else:
            return evalSequentially(env, case, k)
    elif exp[0] == 'define':
        if isinstance(exp[1], str):
            (_, name, value) = exp
            return eval(env, value, lambda v: (env.define(name, v), k))
        else:
            name, formals, expList = exp[1][0], exp[1][1:], exp[2:]
            return eval(env, ['define', name, ['lambda', formals] + expList], k)
    elif exp[0] == 'set!':
        (_, name, value) = exp
        return eval(env, value, lambda v: (env.set(name, v), k))
    elif exp[0] == 'let':
        nameList, valueList, expList = [kv[0] for kv in exp[1]], [kv[1] for kv in exp[1]], exp[2:]
        return eval(env, [['lambda', nameList] + expList] + valueList, k)
    else:
        return evalList(env, exp, lambda args: args[0](args[1:], k))

def forceEval(env, exp):
    v, k = eval(env, exp, None)
    while k:
        v, k = k(v)
    return v

def funcsToCPS(funcs):
    return dict((name, lambda actuals, k, f=f: (f(*actuals), k)) for name, f in funcs.iteritems())

G = Env(False, 
        dict(
        funcsToCPS({
            '+':lambda a,b: a+b, '-':lambda a,b: a-b, '*':lambda a,b: a*b, '/':lambda a,b: a/b,
            'quotient':lambda a,b: a//b, 'remainder':lambda a,b: a%b,
            'sqr': lambda a: a*a, 'sqrt': lambda a: math.sqrt(a),
            'identity': lambda a: a,
            '=':lambda a,b: a==b, 'not':lambda a: not a,
            '<':lambda a,b: a<b, '>':lambda a,b: a>b, '<=':lambda a,b: a<=b, '>=':lambda a,b: a>=b,
            'eq':lambda a,b: a==b,
            'cons': lambda a,b: [a]+b, 'car': lambda l: l[0], 'cdr': lambda l: l[1:],
            'cadr': lambda l: l[1], 'caddr': lambda l: l[2], 'cadddr': lambda l: l[3],
            'drop': lambda l,n: l[n:], 'append': lambda a,b: a+b, 'length': lambda l: len(l), 
            'empty?': lambda l: len(l) == 0,
            'pretty-print': printSExp, 'display': lambda e: sys.stdout.write(repr(e)),
            'current-inexact-milliseconds': lambda :time.time() * 1000,
            'random': lambda a: random.randint(0, a),
            'eval': lambda e: forceEval(G, e),
            'exit': lambda : sys.exit(1),
            }).items() +
            {
            'true': True, 'false': False, 'else': True,
            'empty': [], 
            'call/cc': (lambda actuals,k: actuals[0]([(lambda actuals2,k2: (actuals2[0], k))], k)),
            }.items()
        ))

#------------------------------
tokens = tokenize(sys.stdin.read())
while True:
    s = parse(tokens)
    if not s: break
    v = forceEval(G, s)
    if v != None: printSExp(v)
