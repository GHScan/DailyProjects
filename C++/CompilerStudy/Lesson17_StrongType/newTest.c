/*
 * This is test source.c
 *
 * */

int g_i, g_i2 = 1;
void test1_andOr()
{
    printf("----------{ test1_AndOr : \n");

    if (true && 1) printf("ture && true\n");
    if (true && 0) ; else printf("true && false\n");
    if (false && 1); else printf("false && true\n");
    if (false && 0); else printf("false && false\n");
    if (true || 1) printf("ture || true\n");
    if (true || 0) printf("true || false\n");
    if (false || 1) printf("false || true\n");
    if (false || 0); else printf("false || false\n");

    printf("----------}\n");
}
void test2_while()
{
    printf("----------{ test2_While : \n");
    {
        int i = 1;
        int n = 0;
        while (n < 1000) n += i;
        assert(n == 1000);
    }
    {
        int i = 0;
        do ; while(++i < 1000);
        assert(i == 1000);
    }
    printf("----------}\n");
}
void test3_switch()
{
    int n = 0;
    for (int i = 0; i < 10; ++i) {
        switch (i) {
            case 0: case 2: case 4: case 6: case 8:
                n -= i;
            case 1: case 3: case 5: case 7: case 9:
                n += i;
            default:
                n -= i;
        }
    }
    assert(n == 5);
}
void test4_array()
{
    int[4][4] a;
    for (int i = 0; i < 4; ++i) {
        a[i][0] = i * 4;
        for (int j = 1; j < 4; ++j) {
            a[i][j] = a[i][j - 1] + 1;
        }
    }

    int sum = 0;
    int *p = a;
    for (int i = 0; i < 16; ++i) sum += p[i];

    assert(sum == 120);
}
struct Student
{
    char* name;
    int id;
};
void strcpy(char *d, char *s)
{
    *d = *s;
    while (*++d = *++s);
}
void test5_struct()
{
    Student r;
    r.id = 5;
    r.name = "ABC";
    printf("Student : %d, %s\n", r.id, r.name);

    Student *p = &r;
    p->name = (char*)malloc(20);
    strcpy(p->name, "yes");
    printf("Student : %d, %s\n", p->id, p->name);
    free(p->name);
}
void test6_sizeof()
{
    int i = 0;
    assert(sizeof(i) == sizeof(int));
    int[2][5] a;
    assert(sizeof(a) == 40);
}

void perform1_prime()
{
    printf("----------{ perform1_prime\n");
    int start = clock();

    int n = 0;
    for (int i = 2; i < 1000; ++i) {
        int isPrime = true;
        for (int j = 2; j <= i / 2; ++j) {
            if (i % j == 0) {
                isPrime = false;
                break;
            }
        }
        if (isPrime) ++n;
    }

    printf("---------- %d, }\n", clock() - start);
}
void perform2_loop3()
{
    printf("----------{ perform2_loop3\n");
    int start = clock();

    int i = 0, j, k = 0; 
    for (i = 0; i < 10; ++i) {
        for (j = 0; j < 1000; ++j) { 
            for (k = 0; k < 1000; ++k);
        }
    }

    printf("---------- %d, }\n", clock() - start);
}
int _feb(int n)
{
    if (n <= 2) return 1;
    return _feb(n - 1) + _feb(n - 2);
}
// perform3_feb
void perform3_feb()
{
    printf("----------{ perform3_feb:\n");
    int start = clock();

    for (int i = 1; i < 30; ++i) _feb(i);

    printf("----------} %d\n", clock() - start);
}
int main()
{
    test1_andOr();
    test2_while();
    test3_switch();
    test4_array();
    test5_struct();
    test6_sizeof();

    perform1_prime();
    perform2_loop3();
    perform3_feb();
}
