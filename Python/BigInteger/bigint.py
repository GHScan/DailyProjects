#vim:fileencoding=utf-8

from math import pi,e,ceil,log2
from functools import reduce
import numpy as np
import time

#------------------------------
def fft(a, flag, N=1):
    if len(a) == 1: return [a[0]/(N if flag<0 else 1)]
    b1 = fft(a[0::2], flag, max(len(a), N))
    b2 = fft(a[1::2], flag, max(len(a), N))
    ws = [e**(2j*flag*i*pi/len(a)) for i in range(len(b1))]
    ts = list(zip(b1, b2, ws))
    return [c1+c2*w for (c1,c2,w) in ts] + [c1-c2*w for (c1,c2,w) in ts]

def fft_dif(a, flag, N=1):
    if len(a) == 1: return [a[0]/(N if flag<0 else 1)]
    ws = [e**(2j*flag*i*pi/len(a)) for i in range(len(a)//2)]
    ts = list(zip(a[:len(ws)], a[len(ws):], ws))
    b1 = fft_dif([c1+c2 for (c1,c2,w) in ts], flag, max(len(a), N))
    b2 = fft_dif([(c1-c2)*w for (c1,c2,w) in ts], flag, max(len(a), N))
    return reduce(lambda a,b:a+b, map(list, zip(b1, b2)))

def fft_np(a, flag, N=1):
    if len(a) == 1: return np.array([a[0]/(N if flag<0 else 1)])
    b1 = fft_np(a[0::2], flag, max(len(a), N))
    b2 = fft_np(a[1::2], flag, max(len(a), N))
    w = np.exp(2j*flag*pi/len(a) * np.arange(len(b1)))
    return np.concatenate((b1+w*b2, b1-w*b2))

def fft_r4(a, flag, N=1):
    if len(a) <= 2: return fft(a, flag, N)
    b1 = fft_r4(a[0::4], flag, max(len(a), N))
    b2 = fft_r4(a[1::4], flag, max(len(a), N))
    b3 = fft_r4(a[2::4], flag, max(len(a), N))
    b4 = fft_r4(a[3::4], flag, max(len(a), N))
    ws = [e**(2j*flag*i*pi/len(a)) for i in range(len(b1))]
    ts = list(zip(b1, b2, b3, b4, ws))
    return \
        [c1+c2*w+c3*w*w+c4*w*w*w for (c1,c2,c3,c4,w) in ts] + \
        [c1+flag*1j*c2*w-c3*w*w-flag*1j*c4*w*w*w for (c1,c2,c3,c4,w) in ts] + \
        [c1-c2*w+c3*w*w-c4*w*w*w for (c1,c2,c3,c4,w) in ts] + \
        [c1-flag*1j*c2*w-c3*w*w+flag*1j*c4*w*w*w for (c1,c2,c3,c4,w) in ts]

def fft_r4_dif(a, flag, N=1):
    if len(a) <= 2: return fft_dif(a, flag, N)
    ws = [e**(2j*flag*i*pi/len(a)) for i in range(len(a)//4)]
    ts = list(zip(a[:len(ws)],a[len(ws):2*len(ws)],a[2*len(ws):3*len(ws)],a[3*len(ws):],ws))
    b1 = fft_r4_dif([c1+c2+c3+c4 for (c1,c2,c3,c4,w) in ts], flag, max(len(a), N))
    b2 = fft_r4_dif([(c1+flag*1j*c2-c3-flag*1j*c4)*w for (c1,c2,c3,c4,w) in ts], flag, max(len(a), N))
    b3 = fft_r4_dif([(c1-c2+c3-c4)*w*w for (c1,c2,c3,c4,w) in ts], flag, max(len(a), N))
    b4 = fft_r4_dif([(c1-flag*1j*c2-c3+flag*1j*c4)*w*w*w for (c1,c2,c3,c4,w) in ts], flag, max(len(a), N))
    return reduce(lambda a,b:a+b, map(list, zip(b1, b2, b3, b4)))

def fft_sr(a, flag, N=1):
    if len(a) <= 2: return fft(a, flag, N)
    b1 = fft_sr(a[0::2], flag, max(len(a), N))
    b2 = fft_sr(a[1::4], flag, max(len(a), N))
    b3 = fft_sr(a[3::4], flag, max(len(a), N))
    ws = [e**(2j*flag*i*pi/len(a)) for i in range(len(b2))]
    ts1 = list(zip(b1[:len(b2)], b2, b3, ws))
    ts2 = list(zip(b1[len(b2):], b2, b3, ws))
    return \
        [c1+(c2*w+c3*w*w*w) for (c1,c2,c3,w) in ts1] + \
        [c1+flag*1j*(c2*w-c3*w*w*w) for (c1,c2,c3,w) in ts2] + \
        [c1-(c2*w+c3*w*w*w) for (c1,c2,c3,w) in ts1] + \
        [c1-flag*1j*(c2*w-c3*w*w*w) for (c1,c2,c3,w) in ts2]

#------------------------------
def ntt(a, flag, P=3221225473, G=5, N=1):
    if len(a) == 1: return [a[0]*(pow(N, P-2, P) if flag<0 else 1)%P]
    b1 = ntt(a[0::2], flag, P, G, max(len(a), N))
    b2 = ntt(a[1::2], flag, P, G, max(len(a), N))
    ws = [pow(G, (P-1+flag*i*(P-1)//len(a))%(P-1), P) for i in range(len(b1))]
    ts = list(zip(b1, b2, ws))
    return [(c1+c2*w)%P for (c1,c2,w) in ts] + [(c1+P*P-c2*w)%P for (c1,c2,w) in ts]

def ntt2(a, flag, qn=64, N=1):
    assert(qn % N == 0)
    P=2**qn+1
    if len(a) == 1: return [a[0]*(2**(2*qn)//N%P if flag<0 else 1)%P]
    b1 = ntt2(a[0::2], flag, qn, max(len(a), N))
    b2 = ntt2(a[1::2], flag, qn, max(len(a), N))
    ws = [pow(2, (2*qn+flag*i*qn//len(b1))%(2*qn), P) for i in range(len(b1))]
    ts = list(zip(b1, b2, ws))
    return [(c1+w*c2)%P for (c1,c2,w) in ts] + [(c1+P*P-c2*w)%P for (c1,c2,w) in ts]

#------------------------------
def classic_convolve(a, b):
    c = [0]*(len(a)+len(b)-1)
    for i in range(len(a)):
        for j in range(len(b)):
            c[i+j]+=a[i]*b[j]
    return c

def convolve(a, b):
    n = 1<<int(ceil(log2(len(a)+len(b)-1)))
    fa = fft(a + [0]*(n-len(a)), 1)
    fb = fft(b + [0]*(n-len(b)), 1)
    fc = [va*vb for (va, vb) in zip(fa, fb)]
    c = fft(fc, -1)
    return [c[i].real for i in range(len(a)+len(b)-1)]

def convolve_dif(a, b):
    n = 1<<int(ceil(log2(len(a)+len(b)-1)))
    fa = fft_dif(a + [0]*(n-len(a)), 1)
    fb = fft_dif(b + [0]*(n-len(b)), 1)
    fc = [va*vb for (va, vb) in zip(fa, fb)]
    c = fft_dif(fc, -1)
    return [c[i].real for i in range(len(a)+len(b)-1)]

def convolve_np(a, b):
    n = 1<<int(ceil(log2(len(a)+len(b)-1)))
    fa = fft_np(a + [0]*(n-len(a)), 1)
    fb = fft_np(b + [0]*(n-len(b)), 1)
    fc = fa * fb
    c = fft_np(fc, -1)
    return [c[i].real for i in range(len(a)+len(b)-1)]

def convolve_r4(a, b):
    n = 1<<(int(ceil(log2(len(a)+len(b)-1)/log2(4)))*2)
    assert(n % 4 == 0)
    fa = fft_r4(a + [0]*(n-len(a)), 1)
    fb = fft_r4(b + [0]*(n-len(b)), 1)
    fc = [va*vb for (va, vb) in zip(fa, fb)]
    c = fft_r4(fc, -1)
    return [c[i].real for i in range(len(a)+len(b)-1)]

def convolve_r4_dif(a, b):
    n = 1<<(int(ceil(log2(len(a)+len(b)-1)/log2(4)))*2)
    assert(n % 4 == 0)
    fa = fft_r4_dif(a + [0]*(n-len(a)), 1)
    fb = fft_r4_dif(b + [0]*(n-len(b)), 1)
    fc = [va*vb for (va, vb) in zip(fa, fb)]
    c = fft_r4_dif(fc, -1)
    return [c[i].real for i in range(len(a)+len(b)-1)]

def convolve_sr(a, b):
    n = 1<<(int(ceil(log2(len(a)+len(b)-1)/log2(4)))*2)
    assert(n % 4 == 0)
    fa = fft_sr(a + [0]*(n-len(a)), 1)
    fb = fft_sr(b + [0]*(n-len(b)), 1)
    fc = [va*vb for (va, vb) in zip(fa, fb)]
    c = fft_sr(fc, -1)
    return [c[i].real for i in range(len(a)+len(b)-1)]

def convolve_ntt(a, b, P=3221225473, G=5):
    n = 1<<int(ceil(log2(len(a)+len(b)-1)))
    fa = ntt(a + [0]*(n-len(a)), 1, P, G)
    fb = ntt(b + [0]*(n-len(b)), 1, P, G)
    fc = [va*vb%P for (va, vb) in zip(fa, fb)]
    c = ntt(fc, -1, P, G)
    return c[:len(a)+len(b)-1]

def convolve_ntt2(a, b, qn=64):
    P=2**qn+1
    n = 1<<int(ceil(log2(len(a)+len(b)-1)))
    fa = ntt2(a + [0]*(n-len(a)), 1, qn)
    fb = ntt2(b + [0]*(n-len(b)), 1, qn)
    fc = [va*vb%P for (va, vb) in zip(fa, fb)]
    c = ntt2(fc, -1, qn)
    return c[:len(a)+len(b)-1]
#------------------------------
def string_to_bigint(s):
    return list(reversed([int(c) for c in s]))

def bigint_to_string(i):
    return ''.join(str(v) for v in reversed(i))

def bigint_remove_leading_zeros(i):
    return bigint_remove_leading_zeros(i[:-1]) if len(i)>1 and i[-1]==0 else i

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
    for f in [fft, fft_dif, fft_np, fft_r4, fft_r4_dif, fft_sr, ntt, ntt2]:
        for bits in range(1, 5):
            l = list(range(1<<bits))
            l2 = [round(complex(v).real) for v in f(f(l ,1) ,-1)]
            assert(l == l2)

def test_convolve():
    for f in [convolve, convolve_dif, convolve_np, convolve_r4, convolve_r4_dif, convolve_sr, convolve_ntt, convolve_ntt2]:
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
