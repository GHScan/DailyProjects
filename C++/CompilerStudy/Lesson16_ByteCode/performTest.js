function printPrime(n)
{
    println('========== printPrime ==========');
    local start = clock();
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
        if (flag) ;
    }
    println('perform : ', clock() - start);
}
function perform()
{
    println('========== perform ==========');
    local start = clock();
    local i;
    for (i = 0; i < 1000000; ++i);
    println('perform : ', clock() - start);
}
function perform2()
{
    println('========== perform2 ==========');
    local start = clock();
    local i;
    local j; 
    local k;
    for (i = 0; i < 100; ++i)
    for (j = 0; j < 1000; ++j)
    for (k = 0; k < 1000; ++k);
    println('perform2', clock() - start);
}
function feb(n)
{
    // it's hard to print function name, so...
    if (n <= 2) return 1;
    return feb(n - 1) + feb(n - 2);
}
function perform3()
{
    println('========== perform3 ==========');
    local start = clock();
    local i;
    for (i = 0; i < 30; ++i) feb(i);
    println('perform3', clock() - start);
}
function main() 
{
    printPrime(10000);
    perform();
    perform2();
    perform3();
}
