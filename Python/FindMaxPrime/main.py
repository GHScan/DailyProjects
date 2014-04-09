#! vim:fileencoding=utf-8

def isPrime(n):
    import random
    assert n > 1
    for i in range(10):
        k = random.randint(1, n - 1)
        if pow(k, n - 1, n) != 1: return False
    return True

def findMaxPrime(n):
    while not isPrime(n): n -= 1
    return n

for i in range(4, 32):
    n = 1 << i
    print n, ':', findMaxPrime(n)
