
import itertools

def primes(maxN):
    n = 2
    nums = itertools.count(n)
    while n < maxN:
        yield n
        nums = itertools.ifilter(lambda i,n=n: i % n, nums)
        n = nums.next()

print list(primes(100))
