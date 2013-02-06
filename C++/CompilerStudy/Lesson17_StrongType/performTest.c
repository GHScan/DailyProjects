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
    println(clock() - start);
}
void perform()
{
    int start = clock();
    for (int i = 0; i < 1000000; ++i);
    println(clock() - start);
}
void perform2()
{
    int start = clock();
    for (int i = 0; i < 100; ++i)
    for (int j = 0; j < 1000; ++j)
    for (int k = 0; k < 1000; ++k);
    println(clock() - start);
}
int main() 
{
    printPrime(10000); 
    perform();
    perform2();
}

