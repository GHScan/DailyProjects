#vim:fileencoding=utf-8

import math

def make_d(f, dx=0.00001):
    return lambda x: (f(x+dx)-f(x))/dx

# second order taylor expansion
class TaylorTable(object):

    def __init__(self, f, x1, xn, n):
        f1 = make_d(f)
        f2 = make_d(f1)

        self.x1 = x1
        self.dx = (xn - x1) / n
        self.table = []
        x = x1
        for _ in range(n + 1):
            y, y1, y2 = f(x), f1(x), f2(x)
            self.table.append(y2 / 2)
            self.table.append(y1 - x * y2)
            self.table.append(y - x * y1 + y2 * x * x / 2)
            x += self.dx

    def __call__(self, x):
        i = math.floor((x - self.x1) / self.dx) * 3
        a2, a1, a0 = self.table[i], self.table[i+1], self.table[i+2]
        return a2 * x * x + a1 * x + a0


def main():
    my_sin = TaylorTable(math.sin, 0, math.pi/2, 90)

    for i in range(37):
        x = math.pi * i / 74 + 0.00001
        y1, y2 = math.sin(x), my_sin(x)
        print('%.6f%%' % (100 * abs(y2 - y1) / y1))



main()
