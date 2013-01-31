/*
 * This is test source.c
 *
 * */

void test1_AndOr()
{
    print("----------{ test1_AndOr : ");

    if (true && 1) print("ture && true");
    if (true && 0) ; else print("true && false");
    if (false && 1); else print("false && true");
    if (false && 0); else print("false && false");
    if (true || 1) print("ture || true");
    if (true || 0) print("true || false");
    if (false || 1) print("false || true");
    if (false || 0); else print("false || false");

    print("----------}");
}
void test2_While()
{
    print("----------{ test2_While : ");
    {
        int i = 0;
        n = 0;
        while (i < 1000) n += i;
        assert(n == 1000);
    }
    {
        int i = 0;
        do ; while(++i < 1000);
        assert(i == 1000);
    }
    print("----------}");
}
void test3_switch()
{
    int n = 0;
    for (int i = 0; i <= 10; ++i) {
        switch (n) {
            case 0: case 2: case 4: case 6: case 8:
                n -= i;
            case 1: case 3: case 5: case 7: case 9:
                n += i;
            default:
                n -= i;
        }
    }
    assert(n == -5);
}
struct Record
{
    char* name;
    int id;
};
void test4_struct()
{
    Record r;
    r.id = 5;
    r.name = "ABC";
    print("Record : ", r.id, r.name);

    Record *p = &r;
    p->name = (char*)malloc();
    strcpy(p->name, "yes");
    print("Record : ", p->id, p->name);
    free(p->name);
}

void perform1_prime()
{
    print("----------{ perform1_prime");
    int start = clock();

    int n = 0;
    for (int i = 2; i < 10000; ++i) {
        int isPrime = true;
        for (int j = 2; j <= i / 2; ++j) {
            if (i % j == 0) {
                isPrime = false;
                break;
            }
        }
        if (isPrime) ++n;
    }

    print("---------- %d, }", clock() - start);
}
void perform2_loop3()
{
    print("----------{ perform2_loop3");
    int start = clock();

    int i = 0, j, k = 0; 
    for (i = 0; i < 1000; ++i) {
        for (j = 0; j < 1000; ++j) { 
            for (k = 0; k < 1000; ++k);
        }
    }

    print("---------- %d, }", clock() - start);
}
int _feb(int n)
{
    if (n <= 2) return 1;
    return _feb(n - 1) + _feb(n - 2);
}
// perform3_feb
void perform3_feb()
{
    print("----------{ perform3_feb:");
    int start = clock();

    for (int i = 1; i < 30; ++i) _feb(i);

    print("----------} %d", clock() - start);
}
int main()
{
    test1_AndOr();
    test2_While();
    test3_switch();

    perform1_prime();
    perform2_loop3();
    perform3_feb();
}
