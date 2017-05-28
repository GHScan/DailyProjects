#vim:fileencoding=utf-8

def integrate_linear_approximately(f, x1, x2, n = 10):
    dx = (x2 - x1) / n
    return dx * ((f(x1) + f(x2)) / 2 + sum(f(x1 + dx * i) for i in range(1, n)))

def integrate_quadric_approximately(f, x1, x2, n = 10):
    dx = (x2 - x1) / (2 * n)
    return dx * (f(x1) + f(x2) 
            + 4 * sum(f(x1 + dx * i) for i in range(1, 2 * n, 2)) 
            + 2 * sum(f(x1 + dx * i) for i in range(2, 2 * n, 2))) / 3


def main():
    import math

    f, x1, x2 = lambda x:1/x, 1, 5
    print(integrate_linear_approximately(f, x1, x2))
    print(integrate_quadric_approximately(f, x1, x2))

    f, x1, x2 = lambda x:math.e ** (-x**2), 0, 1
    print(integrate_quadric_approximately(f, x1, x2))

    f, x1, x2 = lambda x:1/(1+x**2), 0, 1
    print(integrate_linear_approximately(f, x1, x2))
    print(integrate_quadric_approximately(f, x1, x2))

    f, x1, x2 = lambda x:math.sin(x)/x, 0.00001, math.pi/2
    print(integrate_linear_approximately(f, x1, x2))
    print(integrate_quadric_approximately(f, x1, x2))


main()
