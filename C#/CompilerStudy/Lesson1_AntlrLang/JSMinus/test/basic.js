function echo(str, b) {
    println('echo', str)
    return b
}
function testAnd() {
    println('jfkd')
    println('========== testAnd 1 ==========')
    var b = echo('1', true) && echo('1', true)
    println('========== testAnd 2 ==========')
    b = echo('0', false) && echo('1', true)
}
function testOr() {
    println('========== testOr 1 ==========')
    var b = echo('0', false) || echo('0', false)
    println('========== testOr 1 ==========')
    b = echo('1', true) || echo('0', false)
}
function printPrime(n) {
    println('========== printPrime ==========')
    for (var i = 2; i < n; ++i) {
        var flag = true
        for (var j = 2; j <= i / 2; ++j) {
            if (i % j == 0) {
                flag = false
                break
            }
        }
        if (flag) println(i) 
    }
    println()
}
function factorial(n) {
    if (n <= 1) return 1
    return n * factorial(n - 1)
}
function printFeb1() {
    println('========== printFeb1 ==========')
    var i = 1 
    var j = 1
    while (i < 100) {
        println(j)
        var t = i + j
        j = i
        i = t
    }
    println()
}
function feb2(n) {
    // it's hard to print function name, so...
    if (n <= 2) { return 1 }
    return feb2(n - 1) + feb2(n - 2)
}
function printFeb2() {
    println('========== printFeb2 ==========')
    var i = 1
    while (i <= 10) {
        println(feb2(i)) 
        i = i + 1
    }
    println()
}
function print9x9() {
    println('========== print9x9 ==========')
    for (var i = 1; i <= 9; ++i) {
        for (var j = 1; j <= i; ++j) {
            print(format('%dx%d=%-2d ', j, i, i * j))
        }
        println()
    }
}
function perform() {
    println('========== perform ==========')
    var start = clock()
    for (var i = 0; i < 1000000; ++i);
    println('loop 1000000 times: ', clock() - start)
}
function main()  {
    // test for comment
    testAnd() // test for comment too
    testOr()
    printPrime(30)
    println('factorial', 10, factorial(10))
    printFeb1()
    printFeb2()
    print9x9()
    perform()
    println('15-3*(2*2+(7-2)) = ', 15-3*(2*2+(7-2)))
}
main()
