function printPrime(n) {
    var start = clock()
    for (var i = 2; i < n; ++i) {
        var flag = true
        for (var j = 2; j <= i / 2; ++j) {
            if (i % j == 0) {
                flag = false
                break
            }
        }
        if (flag);
    }
    println('printPrime: ', clock() - start)
}
function perform() {
    var start = clock()
    for (var i = 0; i < 1000000; ++i);
    println('perform: ', clock() - start)
}
function perform_1_5() {
    var start = clock()
    var i = 0
    while (i < 1000000) i += 1
    println('perform 1.5: ', clock() - start)
}
function perform2() {
    var start = clock()
    for (var i = 0; i < 100; ++i) {
        for (var j = 0; j < 1000; ++j) {
            for (var k = 0; k < 1000; ++k) {
            }
        }
    }
    println('perform2: ', clock() - start)
}
function feb(n) {
    if (n <= 2) return 1
    return feb(n - 1) + feb(n - 2)
}
function perform3() {
    var start = clock()
    for (var i = 0; i < 30; ++i) feb(i)
    println('perform3: ', clock() - start)
}
function main()  {
    printPrime(10000)
    perform()
    perform_1_5()
    perform2()
    perform3()
}
main()

