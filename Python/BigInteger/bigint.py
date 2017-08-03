#vim:fileencoding=utf-8

from math import pi, e, ceil, log
import numpy as np
import time

#------------------------------
def fft(a, flag, N=1):
    if len(a) == 1: return [a[0]/(N if flag<0 else 1)]
    b1 = fft(a[0::2], flag, max(len(a), N))
    b2 = fft(a[1::2], flag, max(len(a), N))
    ws = [e**(1j*flag*i*pi/len(b1)) for i in range(len(b1))]
    ts = list(zip(b1, b2, ws))
    return [c1+c2*w for (c1,c2,w) in ts] + [c1-c2*w for (c1,c2,w) in ts]

def fft_np(a, flag, N=1):
    if len(a) == 1: return np.array([a[0]/(N if flag<0 else 1)])
    b1 = fft_np(a[0::2], flag, max(len(a), N))
    b2 = fft_np(a[1::2], flag, max(len(a), N))
    w = np.exp(flag*1j*pi/len(b1) * np.arange(len(b1)))
    return np.concatenate((b1+w*b2, b1-w*b2))

#------------------------------
def ntt(a, flag, P=3221225473, G=5, N=1):
    if len(a) == 1: return [a[0]*(pow(N, P-2, P) if flag<0 else 1)%P]
    b1 = ntt(a[0::2], flag, P, G, max(len(a), N))
    b2 = ntt(a[1::2], flag, P, G, max(len(a), N))
    ws = [pow(G, (P-1+flag*i*(P-1)//len(a))%(P-1), P) for i in range(len(b1))]
    ts = list(zip(b1, b2, ws))
    return [(c1+c2*w)%P for (c1,c2,w) in ts] + [(c1-c2*w)%P for (c1,c2,w) in ts]

#------------------------------
def classic_convolve(a, b):
    c = [0]*(len(a)+len(b)-1)
    for i in range(len(a)):
        for j in range(len(b)):
            c[i+j]+=a[i]*b[j]
    return c

def convolve(a, b):
    n = 1<<int(ceil(log(len(a)+len(b)-1)/log(2)))
    fa = fft(a + [0]*(n-len(a)), 1)
    fb = fft(b + [0]*(n-len(b)), 1)
    fc = [va*vb for (va, vb) in zip(fa, fb)]
    c = fft(fc, -1)
    return [c[i].real for i in range(len(a)+len(b)-1)]

def convolve_np(a, b):
    n = 1<<int(ceil(log(len(a)+len(b)-1)/log(2)))
    fa = fft_np(a + [0]*(n-len(a)), 1)
    fb = fft_np(b + [0]*(n-len(b)), 1)
    fc = fa * fb
    c = fft_np(fc, -1)
    return [c[i].real for i in range(len(a)+len(b)-1)]

def convolve_ntt(a, b, P=3221225473, G=5):
    n = 1<<int(ceil(log(len(a)+len(b)-1)/log(2)))
    fa = ntt(a + [0]*(n-len(a)), 1, P, G)
    fb = ntt(b + [0]*(n-len(b)), 1, P, G)
    fc = [va*vb%P for (va, vb) in zip(fa, fb)]
    c = ntt(fc, -1, P, G)
    return c[:len(a)+len(b)-1]
#------------------------------
def string_to_bigint(s):
    return list(reversed([int(c) for c in s]))

def bigint_to_string(i):
    return ''.join(str(v) for v in reversed(i))

def bigint_remove_leading_zeros(i):
    size = len(i)
    while size > 1 and i[size-1]==0: size -= 1
    return i[:size]

def bigint_carry(i):
    c, ni = 0, []
    for v in i:
        c += v
        ni.append(c%10)
        c //= 10
    while c > 0:
        ni.append(c%10)
        c //= 10
    return ni

def bigint_normalized(i):
    return bigint_carry(bigint_remove_leading_zeros(i))

def bigint_add(a, b):
    if len(a) < len(b): return bigint_add(b, a)
    return bigint_normalized([av+bv for (av, bv) in zip(a, b + [0]*(len(a)-len(b)))])

def bigint_mul(a, b):
    return bigint_normalized([round(v) for v in convolve(a, b)])

def bigint_pow(a, b):
    if b == 0: return [1]
    return bigint_mul(a if b & 1 else [1], bigint_pow(bigint_mul(a, a), b >> 1))

def bigint_fac(n, d=1):
    if n <= d: return string_to_bigint(str(max(1, n)))
    return bigint_mul(bigint_fac(n, d*2), bigint_fac(n-d, d*2))

#------------------------------
def test_fft():
    for f in [fft, fft_np, ntt]:
        for bits in range(1, 5):
            l = list(range(1<<bits))
            l2 = [round(complex(v).real) for v in f(f(l ,1) ,-1)]
            assert(l == l2)

def test_convolve():
    for f in [convolve, convolve_np, convolve_ntt]:
        for bits in range(1, 5):
            ns = list(range(1<<bits))
            l = classic_convolve(ns, ns)
            l2 = [round(v) for v in f(ns, ns)]
            assert(l == l2)

def test_bigint():
    ns = [0, 1, 15, 65, 129, 255, 513, 1025, 2049, 4095]
    for a in ns:
        for b in ns:
            ia = string_to_bigint(str(a))
            ib = string_to_bigint(str(b))
            sadd = bigint_to_string(bigint_add(ia, ib))
            smul = bigint_to_string(bigint_mul(ia, ib))
            assert(sadd == str(a + b))
            assert(smul == str(a * b))

            ipowa = bigint_to_string(bigint_pow(ia, 14))
            assert(ipowa == str(a**14))
    assert(bigint_to_string(bigint_fac(30)) == "265252859812191058636308480000000")

#------------------------------
def benchmark_convolve():
    for f in [convolve, convolve_np, convolve_ntt]:
        for bits in range(10,15):
            l = list(range(1<<bits))
            start = time.clock()
            f(l,l)
            print(f.__name__, len(l), time.clock() - start)

def benchmark_bigint():
    for bits in range(5, 12):
        n = 1 << bits
        start = time.clock()
        bigint_fac(n)
        print(str(n) + '!', time.clock() - start)

#------------------------------
if __name__ == '__main__':
    test_fft()
    test_convolve()
    test_bigint()
    # benchmark_convolve()
    # benchmark_bigint()

