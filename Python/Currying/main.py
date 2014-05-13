#vim:fileencoding=utf-8

import inspect

def curry(f, *boundArgs):
    n = len(inspect.getargspec(f).args)
    def curried(*args):
        allArgs = boundArgs + args
        if len(allArgs) >= n:
            return f(*allArgs)
        else:
            return curry(f, *allArgs)
    return curried

#------------------------------
def sum4(a, b, c, d): 
    return a + b + c + d

print sum4(1, 2, 3, 4)
print curry(sum4)(1)(2)(3)(4)
print curry(sum4)(1, 2)(3)(4)
print curry(sum4)(1, 2)(3, 4)
print curry(sum4)(1, 2, 3, 4)
print curry(sum4, 1, 2)(3, 4)

#------------------------------
def add(a, b): return a + b
def sub(a, b): return a - b
def mul(a, b): return a * b
def div(a, b): return a / b
print map(curry(mul, 2), range(10))
