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
    def set(self, name, value):
        if name in self.frame:
            self.frame[name] = value
        else:
            assert self.preEnv, 'Cant find variable: "%s"' % name
            self.prevEnv.set(name, value)

INT = re.compile(r'\d+')
FLOAT = re.compile(r'\d*\.\d+')
def parse(s):
    s = re.sub(r';[^\n]*\n', '', s)
    s = s.replace('(', ' ( ').replace(')', ' ) ')
    s = s.replace('[', ' ( ').replace(']', ' ) ')
    r = [[]]
    for token in s.split():
        if token == '(':
            r.append([])
        elif token == ')':
            l = r.pop()
            r[-1].append(l)
        elif INT.match(token):
            r[-1].append(int(token))
        elif FLOAT.match(token):
            r[-1].append(float(token))
        else:
            r[-1].append(token)
    return r[0]

def eval(env, exp):
    while True:
        if isinstance(exp, (bool, int, long, float)):
            return exp
        elif isinstance(exp, str):
            return env.lookup(exp)
        elif exp[0] == 'quote':
            return exp[1]
        elif exp[0] == 'if':
            if eval(env, exp[1]):
                exp = exp[2]
            else:
                exp = exp[3]
        elif exp[0] == 'lambda':
            return (env, exp[1], exp[2:])
        elif exp[0] == 'begin':
            for e in exp[1:-1]: eval(env, e)
            exp = exp[-1]
        elif exp[0] == 'cond':
            for i in range(1, len(exp)):
                if eval(env, exp[i][0]):
                    break
            assert i < len(exp), 'cond shold be end with else: %s' % exp
            exp = exp[i][1:]
            for e in exp[:-1]: eval(env, e)
            exp = exp[-1]
        elif exp[0] == 'define':
            if isinstance(exp[1], str):
                env.define(exp[1], eval(env, exp[2]))
                return None
            else:
                env.define(exp[1][0], (env, exp[1][1:], exp[2:]))
                return None
        elif exp[0] == 'set!':
            env.set(exp[1], eval(env, exp[2]))
            return None
        elif exp[0] == 'let':
            exp = [['lambda', [kv[0] for kv in exp[1]]] + exp[2:]] + [kv[1] for kv in exp[1]]
        else:
            f = eval(env, exp[0])
            actuals = [eval(env, e) for e in exp[1:]]
            if isinstance(f, tuple):
                env = Env(f[0], dict(zip(f[1], actuals)))
                exp = f[2]
                for e in exp[:-1]: eval(env, e)
                exp = exp[-1]
            else:
                return f(*actuals)

G = Env(None,
        {
            '+':lambda a,b: a+b, '-':lambda a,b: a-b, '*':lambda a,b: a*b, '/':lambda a,b: a/b,
            'quotient':lambda a,b: a//b, 'remainder':lambda a,b: a%b,
            'sqr': lambda a: a*a, 'sqrt': lambda a: math.sqrt(a),
            'identity': lambda a: a,
            'true': True, 'false': False, 'else': True,
            '=':lambda a,b: a==b, 'not':lambda a: not a,
            '<':lambda a,b: a<b, '>':lambda a,b: a>b, '<=':lambda a,b: a<=b, '>=':lambda a,b: a>=b,
            'eq':lambda a,b: a==b,
            'cons': lambda a,b: [a]+b, 'car': lambda l: l[0], 'cdr': lambda l: l[1:],
            'cadr': lambda l: l[1], 'caddr': lambda l: l[2], 'cadddr': lambda l: l[3],
            'drop': lambda l,n: l[n:], 'append': lambda a,b: a+b, 'length': lambda l: len(l), 
            'empty': [], 'empty?': lambda l: len(l) == 0,
            'print': printSExp,
            'clock': lambda :time.clock(),
            'random': lambda a: random.randint(0, a),
            'eval': lambda e: eval(G, e),
            })

#------------------------------
for s in parse(sys.stdin.read()):
    v = eval(G, s)
    if v != None:
        printSExp(v)
