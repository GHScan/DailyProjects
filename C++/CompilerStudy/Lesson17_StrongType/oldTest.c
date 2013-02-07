int echo(int b)
{
    printf("%d\n", b);
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
        if (flag) printf("%d\t", i);
    }
    printf("\n");
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
        printf("%d\t", j);
        int t = i;
        i = i + j;
        j = t;
    }
    printf("\n");
}
int feb2(int n)
{
    // it's hard to print function name, so...
    if (n <= 2) return 1;
    return feb2(n - 1) + feb2(n - 2);
}
void printFeb2()
{
    for (int i = 1; i <= 10; ++i) printf("%d\t", feb2(i));
    printf("\n");
}
void print9x9()
{
    for (int i = 1; i <= 9; ++i) {
        for (int j = 1; j <= i; ++j) {
            printf("%dx%d=%-2d ", j, i, i * j);
        }
        printf("\n");
    }
}
void perform()
{
    int start = clock();
    for (int i = 0; i < 100000; ++i);
    printf("perform: %d \n", clock() - start);
}
int main() 
{
    // test for comment
    testAnd(); // test for comment too
    testOr();
    printPrime(30); 
    printf("%d\n", factorial(10));
    printFeb1();
    printFeb2();
    print9x9();
    perform();
    int a = 30 - 5;
    printf("%d\n", 15 - 3*(2*2+(7 - 2)));
}

