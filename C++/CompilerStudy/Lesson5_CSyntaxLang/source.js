function echo(str, b)
{
    println('echo', str);
    return b;
}
function testAnd()
{
    println('========== testAnd 1 ==========');
    echo('1', 1) && echo('1', 1);
    println('========== testAnd 2 ==========');
    echo('0', 0) && echo('1', 1);
}
function testOr()
{
    println('========== testOr 1 ==========');
    echo('0', 0) || echo('0', 0);
    println('========== testOr 1 ==========');
    echo('1', 1) || echo('0', 0);
}
function printPrime(n)
{
    println('========== printPrime ==========');
    local i;
    local j;
    local flag;
    for (i = 2; i <= n; ++i) {
        flag = 1;
        for (j = 2; j <= i / 2; ++j) {
            if (i % j == 0) {
                flag = 0;
                break;
            }
        }
        if (flag) print(i);
    }
    println();
}
function factorial(n)
{
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}
function printFeb1()
{
    println('========== printFeb1 ==========');
    local i = 1;
    local j = 1;
    local t = 0;
    while (i < 100) {
        print(j);
        t = i;
        i = i + j;
        j = t;
    }
    println();
}
function feb2(n)
{
    // it's hard to print function name, so...
    if (n <= 2) return 1;
    return feb2(n - 1) + feb2(n - 2);
}
function printFeb2()
{
    println('========== printFeb2 ==========');
    local i = 1;
    for (; i <= 10; ++i) print(feb2(i));
    println();
}
function print9x9()
{
    println('========== print9x9 ==========');
    local i = 0;
    local j = 0;
    for (i = 1; i <= 9; ++i) {
        for (j = 1; j <= i; ++j) {
            print('' + j + 'x' + i + '=' + i * j);
        }
        println();
    }
}
function perform()
{
    println('========== perform ==========');
    local start = clock();
    local i;
    for (i = 0; i < 100000; ++i);
    println('loop 100000 times: ', clock() - start, 'mill seconds');
}
function main() 
{
    // test for comment
    testAnd(); // test for comment too
    testOr();
    printPrime(30); 
    println('factorial', 10, factorial(10));
    printFeb1();
    printFeb2();
    print9x9();
    perform();
}
