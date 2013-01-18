class PipeFuncs(object):
    @staticmethod
    def select(_iter, func):
        for i in _iter:
            yield func(i)

    @staticmethod
    def where(_iter, func):
        for i in _iter:
            if func(i):
                yield i

    @staticmethod
    def takeWhile(_iter, func):
        for i in _iter:
            if func(i):
                yield i
            else:
                break

    @staticmethod
    def skipWhile(_iter, func):
        skip = True
        for i in _iter:
            if not skip:
                yield i
            else:
                skip = func(i)

    @staticmethod
    def sort(_iter, func = lambda a, b: a - b):
        return sorted(_iter, func)

    @staticmethod
    def all(_iter, func):
        for i in _iter:
            if not func(i):
                yield False
                return
        yield True

    @staticmethod
    def any(_iter, func):
        for i in _iter:
            if func(i):
                yield True
                return
        yield False

    @staticmethod
    def average(_iter):
        sum, n = 0, 0
        for i in _iter:
            sum += i
            n += 1
        if not n:
            yield None
        else:
            yield sum / n

    @staticmethod
    def contain(_iter, v):
        for i in _iter:
            if v == i:
                yield True
                return
        yield False

    @staticmethod
    def count(_iter, func):
        n = 0
        for i in _iter:
            if func(i):
                n += 1
        yield n

    @staticmethod
    def unique(_iter):
        s = set()
        for i in _iter:
            if not i in s:
                s.add(i)
                yield i

    @staticmethod
    def first(_iter, n = 1):
        assert n > 0
        for i in _iter:
            n -= 1
            if n >= 0:
                yield i
            if n == 0:
                return

    @staticmethod
    def last(_iter, n = 1):
        assert n > 0
        l = list(_iter)
        n = min(len(l), n)
        for i in range(len(l) - n, len(l)):
            yield l[i]

    @staticmethod
    def groupBy(_iter, func):
        r = dict()
        for i in _iter:
            r.setdefault(func(i), []).append(i)
        return r.iteritems()

    @staticmethod
    def intersect(_iter, _iter2):
        s = set(_iter)
        s &= set(_iter2)
        return s

    @staticmethod
    def union(_iter, _iter2):
        s = set(_iter) 
        s |= set(_iter2)
        return s

    @staticmethod
    def max(_iter):
        _iter = iter(_iter)
        v = next(_iter)
        for i in _iter:
            if v < i:
                v = i
        yield v

    @staticmethod
    def min(_iter):
        _iter = iter(_iter)
        v = next(_iter)
        for i in _iter:
            if v > i:
                v = i
        yield v

    @staticmethod
    def reverse(_iter):
        return reversed(_iter)

    @staticmethod
    def reduce(_iter, func, init):
        for i in _iter:
            init = func(init, i)
        yield init

    @staticmethod
    def random(_iter):
        import random
        l = list(_iter)
        _len = len(l)
        for i in range(_len):
            j = random.randint(i, _len - 1)
            l[i], l[j] = l[j], l[i]
        return l


class Pipe(object):
    def __init__(self, _iter):
        self.m_iter = _iter
    def __iter__(self):
        return iter(self.m_iter)

    def __getattr__(self, k):
        def _func(*arg):
            return Pipe(eval('PipeFuncs.' + k)(self.m_iter, *arg))
        return _func

print list(Pipe(range(10)).where(lambda i: not i % 3).select(lambda i: i * i))
