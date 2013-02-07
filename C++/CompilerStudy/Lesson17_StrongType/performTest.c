void printPrime(int n)
{
    int start = clock();
    for (int i = 2; i <= n; ++i) {
        int flag = 1;
        for (int j = 2; j <= i / 2; ++j) {
            if (i % j == 0) {
                flag = 0;
                break;
            }
        }
        if (flag) ;
    }
    printf("printPrime: %d\n", clock() - start);
}
void perform()
{
    int start = clock();
    for (int i = 0; i < 1000000; ++i);
    printf("perform: %d\n", clock() - start);
}
void perform2()
{
    int start = clock();
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 1000; ++j)
    for (int k = 0; k < 1000; ++k);
    printf("perform2: %d\n", clock() - start);
}
int feb(int n)
{
    if (n <= 2) return 1;
    return feb(n - 1) + feb(n - 2);
}
void perform3()
{
    int start = clock();
    for (int i = 0; i < 30; ++i) feb(i);
    printf("perform3: %d\n", clock() - start);
}
int main() 
{
    printPrime(10000); 
    perform();
    perform2();
    perform3();
}

