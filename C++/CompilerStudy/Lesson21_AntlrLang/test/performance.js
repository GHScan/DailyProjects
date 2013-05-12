var function printPrime(n) {
    var start = clock()
    for (var i = 2; i < n; ++i) {
        var flag = true
        for (var j = 2; j < i / 2; ++j) {
            if (i % j == 0) {
                flag = false
            }
        }
        if (flag) {}
    }
    print('printPrime: ', clock() - start)
}
var function perform() {
    var start = clock()
    for (var i = 0; i < 1000000; ++i);
    print('perform: ', clock() - start)
}
var function perform_1_5() {
    var start = clock()
    var i = 0
    while (i < 1000000) i += 1
    print('perform 1.5: ', clock() - start)
}
var function perform2() {
    var start = clock()
    for (var i = 0; i < 100; ++i) {
        for (var j = 0; j < 1000; ++j) {
            for (var k = 0; k < 1000; ++k) {
            }
        }
    }
    print('perform2: ', clock() - start)
}
var function feb(n) {
    if (n <= 2) return 1
    return feb(n - 1) + feb(n - 2)
}
var function perform3() {
    var start = clock()
    for (var i = 0; i < 30; ++i) feb(i)
    print('perform3: ', clock() - start)
}
var function main()  {
    printPrime(10000)
    perform()
    perform_1_5()
    perform2()
    perform3()
}
main()

