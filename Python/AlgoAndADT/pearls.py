# vim:fileencoding=utf-8

import itertools

def qsort(l):
    if len(l) < 2: return l
    else: 
        return qsort([v for v in l[1:] if v <= l[0]]) + l[0:1] + qsort([v for v in l[1:] if v > l[0]])

def permutation(l):
    if not len(l): return [[]]
    else: return [rest[0:i]+l[0:1]+rest[i:] 
            for i in range(len(l)) 
            for rest in permutation(l[1:])]

def combination(l, n):
    if not n: return [[]]
    elif not l: return []
    else: return combination(l[1:], n) + [
            l[0:1]+rest for rest in combination(l[1:], n - 1)]
    
def primes(limit):
    nums = itertools.count(2)
    n = nums.next()
    while n < limit:
        yield n
        nums = itertools.ifilter(lambda v,n=n:v%n, nums)
        n = nums.next()

print qsort([3, 5, 1, 4, 2])
print len(permutation(range(4)))
print len(combination(range(8), 4))
print list(primes(20))
