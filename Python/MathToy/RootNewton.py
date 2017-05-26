#vim:fileencoding=utf-8

def root_newton(f, x0, n=10, dx=0.0001):
    for _ in range(n):
        y0 = f(x0)
        t = (f(x0 + dx) - y0) / dx
        x0 = x0 - y0 / t
    return x0

def inverse_func(f):
    return lambda x: root_newton(lambda y: f(y)-x, x)

def main():
    import math

    my_sqrt = inverse_func(lambda x:x*x)
    my_cuberoot = inverse_func(lambda x:x*x*x)
    my_arcsin = inverse_func(math.sin)
    my_arctan = inverse_func(math.tan)
    

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
