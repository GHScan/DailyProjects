int echo(int b)
{
    println(b);
    return b;
}
void testAnd()
{
    echo(1) && echo(1);
    echo(0) && echo(1);
}
void testOr()
{
    echo(0) || echo(0);
    echo(1) || echo(0);
}
void printPrime(int n)
{
    for (int i = 2; i <= n; ++i) {
        int flag = 1;
        for (int j = 2; j <= i / 2; ++j) {
            if (i % j == 0) {
                flag = 0;
                break;
            }
        }
        if (flag) print(i);
    }
    println(0);
}
int factorial(int n)
{
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}
void printFeb1()
{
    int i = 1, j = 1;
    while (i < 100) {
        print(j);
        int t = i;
        i = i + j;
        j = t;
    }
    println(0);
}
int feb2(int n)
{
    // it's hard to print function name, so...
    if (n <= 2) return 1;
    return feb2(n - 1) + feb2(n - 2);
}
void printFeb2()
{
    for (int i = 1; i <= 10; ++i) print(feb2(i));
    println(0);
}
void print9x9()
{
    for (int i = 1; i <= 9; ++i) {
        for (int j = 1; j <= i; ++j) {
            print(i);
            print(j);
            print(i * j);
        }
        println(0);
    }
}
void perform()
{
    int start = clock();
    for (int i = 0; i < 100000; ++i);
    println(clock() - start);
}
int main() 
{
    // test for comment
    testAnd(); // test for comment too
    testOr();
    printPrime(30); 
    println(factorial(10));
    printFeb1();
    printFeb2();
    print9x9();
    perform();
    int a = 30 - 5;
    println(15 - 3*(2*2+(7 - 2)));
}

