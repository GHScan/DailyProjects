#! vim:fileencoding=utf-8

def matrix2MulVec2(v0, m1):
    return (v0[0] * m1[0][0] + v0[1] * m1[1][0], v0[0] * m1[0][1] + v0[1] * m1[1][1])
def matrix2MulMatrix2(m0, m1):
    return ((m0[0][0] * m1[0][0] + m0[0][1] * m1[1][0], m0[0][0] * m1[0][1] + m0[0][1] * m1[1][1]),
            (m0[1][0] * m1[0][0] + m0[1][1] * m1[1][0], m0[1][0] * m1[0][1] + m0[1][1] * m1[1][1]))

def _febn(v0, m0, n):
    if n == 0:
        return v0[1]
    elif n % 2:
        return _febn(matrix2MulVec2(v0, m0), m0, n - 1)
    else:
        return _febn(v0, matrix2MulMatrix2(m0, m0), n / 2)
def febn(n):
    return _febn((1, 0), ((1, 1), (1, 0)), n)

def febn_numpy(n):
    import numpy
    m = numpy.mat('1 1; 1 0', object)
    return ((1, 0)*(m**n)).item(0, 1)

for i in range(10):
    print febn(i)
print febn(1000)
