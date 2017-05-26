#vim:fileencoding=utf-8

import math

def root_newton(f, x0, n=10):
    dx = 0.0001
    for _ in range(n):
        y0 = f(x0)
        t = (f(x0 + dx) - y0) / dx
        x0 = x0 - y0 / t
    return x0

def my_sqrt(v):
    return root_newton(lambda x:x*x-v, v)
def my_cuberoot(v):
    return root_newton(lambda x:x*x*x-v, v)
def my_arcsin(v):
    return root_newton(lambda x:math.sin(x)-v, 0)
def my_arctan(v):
    return root_newton(lambda x:math.tan(x)-v, 0)

def main():
    print('# n, sqrt(n), cuberoot(n)')
    for i in range(1, 10):
        print('\t', i, my_sqrt(i), my_cuberoot(i))

    print('# v, arcsin(v)')
    for v in [0, 0.5, math.sqrt(3)/2, 1/math.sqrt(2), 1]:
        print('\t', v, my_arcsin(v)/math.pi*180)

    print('# v, arctan(v)')
    for v in [0, 1/math.sqrt(3), math.sqrt(3), 1]:
        print('\t', v, my_arctan(v)/math.pi*180)

main()
