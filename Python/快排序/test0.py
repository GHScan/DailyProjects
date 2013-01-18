# vim:fileencoding=gbk

import random
import time

class Timer(object):
    def __init__(self, name):
        self._name = name
    def __enter__(self, *arg):
        self._t = time.clock()
    def __exit__(self, *arg):
        print self._name, time.clock() - self._t

def qsort(a):
    if not a: return a
    return qsort([x for x in a[1:] if x <= a[0]]) + a[0:1] + qsort(
            [x for x in a[1:] if x > a[0]])

def random_seq(l):
    def _seq(l):
        for i in range(l):
            yield random.randrange(1, l)
    return list(_seq(l))

loop = 100
l = 1 << 12

with Timer('sorted'):
    for _ in range(loop):
        sorted(random_seq(l))

with Timer('qsort'):
    for _ in range(loop):
        qsort(random_seq(l))
