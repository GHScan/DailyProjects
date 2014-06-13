#! vim:fileencoding=utf-8

import re, math, time, random, sys
#------------------------------
Sempty = list()

def Scons(a, b):
    return (a, b)
def Scar(p):
    return p[0]
def Scdr(p):
    return p[1]

def Scadr(p):
    return p[1][0]
def Scaddr(p):
    return p[1][1][0]
def Scadddr(p):
    return p[1][1][1][0]

def Slist(*args):
    r = Sempty
    for v in reversed(args):
        r = Scons(v, r)
    return r
def Sappend(a, b):
    return Slist(*(fromSlist(a) + fromSlist(b)))
def Sdrop(l, n):
    while n > 0:
        l = Scdr(l)
        n -= 1
    return l
def Sref(l, i):
    return Scar(Sdrop(l, i))
def Sfoldl(f, init, l):
    while l != Sempty:
        init = f(Scar(l), init)
        l = Scdr(l)
    return init
def Slength(l):
    n = 0
    while l != Sempty:
        l = Scdr(l)
        n += 1
    return n

def fromSlist(l):
    r = []
    while l != Sempty:
        r.append(Scar(l))
        l = Scdr(l)
    return r

def fromSExp(s):
    if isinstance(s, tuple):
        return map(fromSExp, fromSlist(s))
    else:
        return s

def printSExp(s):
    print fromSExp(s)
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
            l = Slist(*r.pop())
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
        elif Sref(exp, 0) == 'quote':
            return Sref(exp, 1)
        elif Sref(exp, 0) == 'if':
            if eval(env, Sref(exp, 1)):
                env, exp = env, Sref(exp, 2)
            else:
                env, exp = env, Sref(exp, 3)
        elif Sref(exp, 0) == 'lambda':
            return (env, fromSlist(Sref(exp, 1)), Scons('begin', Sdrop(exp, 2)))
        elif Sref(exp, 0) == 'begin':
            exp, next = Sdrop(exp, 1), Sdrop(exp, 2)
            while next != Sempty:
                eval(env, Scar(exp))
                exp, next = next, Scdr(next)
            exp = Scar(exp)
        elif Sref(exp, 0) == 'cond':
            if Slength(exp) == 2:
                exp = Scons('begin', Sref(exp, 1))
            else:
                case = Sref(exp, 1)
                if eval(env, Scar(case)):
                    exp = Scons('begin', Sdrop(case, 1))
                else:
                    exp = Scons('cond', Sdrop(exp, 2))
        elif Sref(exp, 0) == 'define':
            if isinstance(Sref(exp, 1), str):
                env.define(Sref(exp, 1), eval(env, Sref(exp, 2)))
                return None
            else:
                exp = Slist('define', 
                        Sref(Sref(exp, 1), 0), 
                        Sappend(
                            Slist('lambda', Sdrop(Sref(exp, 1), 1)), 
                            Sdrop(exp, 2)))
        elif Sref(exp, 0) == 'set!':
            env.set(Sref(exp, 1), eval(env, Sref(exp, 2)))
            return None
        elif Sref(exp, 0) == 'let':
            nameValues = fromSlist(Sref(exp, 1))
            names = Slist(*map(Scar, nameValues))
            values = Slist(*map(Scadr, nameValues))
            exp = Scons(Sappend(Slist('lambda', names), Sdrop(exp, 2)), values)
        else:
            f = eval(env, Sref(exp, 0))
            actuals = map(lambda e: eval(env, e), fromSlist(Sdrop(exp, 1)))
            if isinstance(f, tuple):
                env = Env(f[0], dict(zip(f[1], actuals)))
                exp = f[2]
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
            'cons': Scons, 'car': Scar, 'cdr': Scdr,
            'cadr': Scadr, 'caddr': Scaddr, 'cadddr': Scadddr,
            'drop': Sdrop, 'append': Sappend, 'length': Slength, 'foldl': Sfoldl,
            'empty': Sempty, 'empty?': lambda l: l==Sempty,
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
