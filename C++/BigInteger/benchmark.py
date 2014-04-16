# vim:fileencoding=utf-8

import time

def correctnessTest():
    big = 1
    for i in range(37, 3997): big *= i

    for i in range(10, 20):
        mod = big % (1<<i)
        lo = big - 23 * mod

    for i in range(3996, 36, -1):
        big /= i
    assert big == 1

def benchmark_correctness():
    start = time.time()
    for i in range(20): correctnessTest()
    print time.time() - start

def benchmark_pow():
    big = 37
    for i in range(10, 23):
        start = time.time()
        r = big**(1<<i)
        print i, r.bit_length(), time.time() - start

def benchmark_mod():
    big = 1
    for i in range(37, 3997): big *= i

    for i in range(10, 23):
        start = time.time()
        for j in range(2, (1<<i)): big % j
        print i, time.time() - start

def benchmark_toString():
    big = 37
    for i in range(10, 23):
        r = big**(1<<i)
        start = time.time()
        l = len(str(r))
        print i, l, time.time() - start

def benchmark_fromString():
    big = 37
    for i in range(10, 23):
        r = big**(1<<i)
        s = str(r)
        start = time.time()
        l = long(s)
        print i, r == l, time.time() - start

correctnessTest()
#benchmark_correctness()
#benchmark_pow()
#benchmark_mod()
#benchmark_toString()
#benchmark_fromString()
