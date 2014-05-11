#vim:fileencoding=utf-8

import inspect

def curry(f, *_args):
    args = list(_args)
    argn = len(inspect.getargspec(f).args)
    def curried(*newargs):
        args.extend(newargs)
        if len(args) >= argn:
            return f(*args)
        else:
            return curried
    return curried

def sum4(a, b, c, d): 
    return a + b + c + d

print sum4(1, 2, 3, 4)
print curry(sum4)(1)(2)(3)(4)
print curry(sum4)(1, 2)(3)(4)
print curry(sum4)(1, 2)(3, 4)
print curry(sum4)(1, 2, 3, 4)
print curry(sum4, 1, 2)(3, 4)
