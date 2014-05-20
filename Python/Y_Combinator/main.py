# vim:fileencoding=utf-8

print map(
        lambda n:
        (lambda f:
            (lambda Y: Y(Y))
            (lambda Y_self: f(lambda *args: Y_self(Y_self)(*args))))
        (lambda self:
            lambda n:
            n if n == 1 else n * self(n - 1))(n),
        range(1, 10))

print map(
        lambda n:
        (lambda f: 
            (lambda Y: Y(Y))
            (lambda Y_self: f(lambda *args: Y_self(Y_self)(*args))))
        (lambda self:
            lambda n, a, b:
            a if 0 == n else self(n - 1, b, a + b))(n, 0, 1), 
        range(10))
