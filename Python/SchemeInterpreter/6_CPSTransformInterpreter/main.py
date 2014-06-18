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
    def extend(self, formals, actuals):
        if isinstance(formals, str):
            return Env(self, {formals:actuals})
        else:
            return Env(self, dict(zip(formals, actuals)))
    def lookup(self, name):
        if name in self.frame:
            return self.frame[name]
        else:
            assert self.preEnv, 'Cant find variable: "%s"' % name
            return self.preEnv.lookup(name)
    def define(self, name, value):
        self.frame[name] = value
    def set(self, name, value):
        if name in self.frame:
            self.frame[name] = value
        else:
            assert self.preEnv, 'Cant find variable: "%s"' % name
            self.preEnv.set(name, value)

def tokenize(s):
    s = re.sub(r'^#lang[^\n]*\n', '', s)
    s = re.sub(r';[^\n]*\n', '', s)
    s = s.replace('(', ' ( ').replace(')', ' ) ')
    s = s.replace('[', ' ( ').replace(']', ' ) ')
    s = s.replace("'", " ' ")
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
    elif tokens[0] == "'":
        tokens.pop(0)
        return ['quote', parse(tokens)]
    elif re.match(r'\d+', tokens[0]):
        return int(tokens.pop(0))
    elif re.match(r'\d*\.\d+', tokens[0]):
        return float(tokens.pop(0))
    else:
        return tokens.pop(0)

def applyScriptProecdure(f, actuals):
    if isinstance(f, tuple):
        env, formals, expList = f
        env = env.extend(formals, actuals)
        for e in expList[:-1]: eval(env, e)
        return eval(env, expList[-1])
    else:
        return f(*actuals)

def eval(env, exp):
    while True:
        if isinstance(exp, str):
            return env.lookup(exp)
        elif not isinstance(exp, list):
            return exp
        elif exp[0] == 'quote':
            (_, datum) = exp
            return datum
        elif exp[0] == 'if':
            (_, predExp, thenExp, elseExp) = exp
            exp = thenExp if eval(env, predExp) else elseExp
        elif exp[0] == 'lambda':
            (_, formals), expList = exp[:2], exp[2:]
            return (env, formals, expList)
        elif exp[0] == 'begin':
            _, expList = exp[0], exp[1:]
            for e in expList[:-1]: eval(env, e)
            exp = expList[-1]
        elif exp[0] == 'cond':
            for i in range(1, len(exp)):
                pred, expList = exp[i][0], exp[i][1:]
                if eval(env, pred):
                    for e in expList[:-1]: eval(env, e)
                    exp = expList[-1]
                    break
        elif exp[0] == 'define':
            if isinstance(exp[1], str):
                (_, name, value) = exp
                env.define(name, eval(env, value))
                return None
            else:
                name, formals, expList = exp[1][0], exp[1][1:], exp[2:]
                env.define(name, (env, formals, expList))
                return None
        elif exp[0] == 'set!':
            (_, name, value) = exp
            env.set(name, eval(env, value))
            return None
        elif exp[0] == 'let':
            nameList, valueList, expList = [kv[0] for kv in exp[1]], [kv[1] for kv in exp[1]], exp[2:]
            exp = [['lambda', nameList] + expList] + valueList
        else:
            f, actuals = exp[0], exp[1:]
            f, actuals = eval(env, f), [eval(env, e) for e in actuals]
            if isinstance(f, tuple):
                env, formals, expList = f
                env = env.extend(formals, actuals)
                for e in expList[:-1]: eval(env, e)
                exp = expList[-1]
            else:
                return f(*actuals)

G = Env(None, {
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
    'drop-right': lambda l,n: l[:-n], 'last': lambda l: l[-1],
    'empty?': lambda l: len(l) == 0,
    'pretty-print': printSExp, 'apply': applyScriptProecdure,
    'current-inexact-milliseconds': lambda :time.time() * 1000,
    'random': lambda a: random.randint(0, a),
    'eval': lambda e: eval(G, e),
    'void': lambda : None,

    'true': True, 'false': False, 'else': True,
    'empty': [],
    }
    )

#------------------------------
tokens = tokenize(sys.stdin.read())
while True:
    s = parse(tokens)
    if not s: break
    v = eval(G, s)
    if v != None: printSExp(v)
