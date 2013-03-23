
#include "pch.h"

#include "KMP.h"
#include "Sunday.h"

#include <time.h>

class Timer
{
public:
    Timer(const char *name): m_name(name), m_start(clock())
    { }
    ~Timer()
    {
        printf("%s : %f\n", m_name.c_str(), float(clock() - m_start) / CLOCKS_PER_SEC);
    }
private:
    string m_name;
    clock_t m_start;
};

int for2Find(const char *pattern, const char *src)
{
    int len1 = (int)strlen(pattern), len2 = (int)strlen(src);
    int r = 0;
    for (int i = 0; i < len2 - len1; ++i) {
        int j = 0;
        for (; j < len1; ++j) if (pattern[j] != src[i + j]) break;
        if (j == len1) ++r;
    }
    return r;
}
int strcmpFind(const char *pattern, const char *src)
{
    int r = 0;
    int len = (int)strlen(pattern);
    for (; *src; ++src) {
        if (strncmp(src, pattern, len) == 0) ++r;
    }
    return r;
}
int strstrFind(const char *pattern, const char *src)
{
    int r = 0;
    while ((src = strstr(src, pattern)) != NULL) ++r, ++src;
    return r;
}

void test(const char *pat, const char *text)
{
    printf("TEST:\npattern:%30s\ntext:%30s\n\n", pat, text);

    const int N = 1 << 15;
    int r, r2;
    {
        Timer _t("strcmp");
        r = 0;
        for (int i = 0; i < N; ++i) r += strcmpFind(pat, text);
    }
    {
        Timer _t("strstr");
        r2 = 0;
        for (int i = 0; i < N; ++i) r2 += strstrFind(pat, text);
        if (r2 != r) puts("wrong !!!!");
    }
    {
        Timer _t("for2");
        r2 = 0;
        for (int i = 0; i < N; ++i) r2 += for2Find(pat, text);
        if (r2 != r) puts("wrong !!!!");
    }
    {
        Timer _t("KMP");
        KMPFinder finder(pat);
        r2 = 0;
        for (int i = 0; i < N; ++i) r2 += finder.find(text);
        if (r2 != r) puts("wrong !!!!");
    }
    {
        Timer _t("KMP2");
        KMPFinder finder(pat);
        r2 = 0;
        for (int i = 0; i < N; ++i) r2 += finder.find2(text);
        if (r2 != r) puts("wrong !!!!");
    }
    {
        Timer _t("Sunday");
        SundayFinder finder(pat);
        r2 = 0;
        for (int i = 0; i < N; ++i) r2 += finder.find(text);
        if (r2 != r) puts("wrong !!!!");
    }

    cout << "occurs:" << r << endl;
}

int main()
{
    test("aabaaababbbbbabaaaaaaaaaababababaabaaaaaaaaaaaaababababababbababaaaaaaaaaaaaaaaababbbbbbbbbaaaaaaaaaaaababbbbbbbbbbbbbbbbbbbbbbbabaaaaaaaaaaaaaaaaaaabababaabbbbbbbbbbbbbbbabaaaaaaaaaabbaba" 
            , "aaaaaabaaabaabaaababbbbbabaaaaaaaaaababababaabaaaaaaaaaaaaababababababbababaaaaaaaaaaaaaaaababbbbbbbbbaaaaaaaaaaaababbbbbbbbbbbbbbbbbbbbbbbabaaaaaaaaaaaaaaaaaaabababaabbbbbbbbbbbbbbbabaaaaabaaababbbbbabaaaaaaaaaababababaabaaaaaaaaaaaaababababababbababaaaaaaaaaaaaaaaababbbbbbbbbaaaaaaaaaaaababbbbbbbbbbbbbbbbbbbbbbbabaaaaaaaaaaaaaaaaaaabababaabbbbbbbbbbbbbbbabaaaaaaaaaabbabaaaaaaaabbabaaaababbbbbabaaaaaaaaaababababaababaaaaaaaaaaaaaaaababbbbbbbbbbabaabaaababbbbbabaaaaaaaaaababababaabaaaaaaaaaaaaababababababbababaaaaaaaaaaaaaaaababbbbbbbbbaaaaaaaaaaaababbbbbbbbbbbbbbbbbbbbbbbabaaaaaaaaaaaaaaaaaaabababaabbbbbbbbbbbbbbbabaaaaaaaaaabbabaaabaaababbbbbabaaaaaaaaaababababaababaaaaaaaaaaaaaaaababbbbbbbbbbabaaaabaaabaaababbbbbabaaaaaaaaaababaaabaaababbbbbabaaaaaaaaaababababaabaaaaaaaaaaaaababababababbababaaaaaaaaaaaaaaaababbbbbbbbbaaaaaaaaaaaababbbbbbbbbbbbbbbbbbbbbbbabaaaaaaaaaaaaaaaaaaabababaabbbbbbbbbbbbbbbabaaaaaaaaaabbabababaababaaaaaaaaaaaaaaaababbbbbbbbbbabaaabaaababbbbbabaaaaaaaaaababababaababaaaaaaaaaaaaaaaababbbbbbbbbbababbbbbabaaaaaaaaababbbbbbbbbbbbbaabaaababbbbbabbbbbbbbbbbbbbaaaaaaaaaaaaaaaabaaabaaababbbbbabaabaaababbbbbabaaaaaaaaaababababaabaaaaaaaaaaaaababababababbababaaaaaaaaaaaaaaaababbbbbbbbbaaaaaaaaaaaababbbbbbbbbbbbbbbbbbbbbbbabaaaaaaaaaaaaaaaaaaabababaabbbbbbbbbbbbbbbabaaaaaabaaababbbbbabaaaaaaaaaababababaabaaaaaaaaaaaaababababababbababaaaaaaaaaaaaaaaababbbbbbbbbaaaaaaaaaaaababbbbbbbbbbbbbbbbbbbbbbbabaaaaaaaaaaaaaaaaaaabababaabbbbbbbbbbbbbbbabaaaaaaaaaabbabaaaaaaabbababbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbabbbbbbbbbbbbbbbbbbbbbbbbbbbbaabaaababbbbbabbbbbbbbbbbbbbaabaaababbbbbabaaaaaaaaaababababaababaaaaaaaaaaaaaaaababbbbbbaabaaababbbbbabaaaaaaaaaababababaabaaaaaaaaaaaaababababababbababaaaaaaaaaaaaaaaababbbbbbbbbaaaaaaaaaaaababbbbbbbbbbbbbbbbbbbbbbbabaabaaababbbbbabaaaaaaaaaababababaabaaaaaaaaaaaaababababababbababaaaaaaaaaaaaaaaababbbbbbbbbaaaaaaaaaaaababbbbbbbbbbbbbbbbbbbbbbbabaaaaaaaaaaaaaaaaaaabababaabbbbbbbbbbbbbbbabaaaaaaaaaabbabaaaaaaaaaaaaaaaaaaaabababaabbbbbbbbbbbbbbbabaaaaaaaaaabbababbbbabababa");
    test("abuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdpqafdsjkl"
            , "afsdabuaiwpfsdjkgbacz.fdsabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdpqafdsabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdpqafdsjklpqafdsjkljkljskaguiwardsbjkafcnafsabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdpqafdsjkldpqafdsjkljklfsdjfklsbcxzm,.fjwdabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdpqafdsjkls;oafjpabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdpqafdsjklnafsdpqafdsjklaweoiufklads;jkl;bjaksldfjklds;bjksanmv,sdjfklsdjfakl;sdjfkldsjfabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdpqafdsjklklsdjfklsdjfkljweiouabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdpqafdsjklbiaabuaiwpfsdjkgbacz.fdsjskaguiwardsabuaiwpfsdjkgbacz.fdsjskaguiwardsbjkafcnafsdpqafdsjklbjkafcnafsdpqafdsjkljklfjdksl");
}
