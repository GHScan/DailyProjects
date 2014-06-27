var func fib(n) {
    if n < 2 {
        n
    } else {
        fib(n - 1) + fib(n - 2)
    }
}

func factorial(n) {
    if n == 1 {
        ret n
    } else {
        ret n * factorial(n - 1)
    }
}

var func fib2(n) {
    var a, b = 0, 1
    for _ in 0...n {
        a, b = b, a + b
    }
    ret a
}

func counterExample() {
    var maker = func(init) {
        ret func(i) {
            init += i
        }
    }

    var c1 = maker(10)
    print(c1(1))
    print(c1(2))

    var c2 = maker(20)
    print(c2(2))
    print(c2(4))

    print(c1(0))
    print(c2(0))
}

func curryExample() {
    var add3 = func(a) {
        func(b) {
            func(c) {
                a + b + c
            }
        }
    }

    print(add3(1)(2)(3))
}

func lambdaExample() {
    var maker = func(f) {
        func(a) {
            func(b) {
                f(a, b)
            }
        }
    }

    var f = maker(func(a,b){a+b})
    print(f(1)(2))
}

func classExample() {
    var Vec = class {
        x, y = 0, 1
        func __init(_x, _y) {
            x, y = _x, _y
        }
        func sqrLength() {
            ret x * x + y * y
        }
    }

    var a1 = Vec(1, 2)
    var a2 = Vec(2, 3)
    print(a1.sqrLength())
    print(a2.sqrLength())
}

func classExample2() {
    var Vec = class {
        x, y = 0
        func __setitem(i, v) {
            if i == 0 {
                x = v
            } else {
                y = v
            }
        }
    }

    var a1 = Vec()@{x=1, y=2}
    var a2 = Vec()@{1, 2}
    var a3 = Vec()@{[0]=1, [1]=2}
    print(a1.sqrLength())
    print(a2.sqrLength())
    print(a3.sqrLength())
}
