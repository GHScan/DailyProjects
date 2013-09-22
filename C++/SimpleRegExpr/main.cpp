#include "pch.h" 

#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>

#include <algorithm>

//////////////////////////////
bool _match1(const char *pat, const char *str) {
    if (pat[0] == 0) return true;
    if (pat[0] == '$') return str[0] == 0;
    if (str[0] == 0) return false;
    if (pat[1] == '*') { 
        const char *begin = str;
        while ((pat[0] == '.' && *str) || pat[0] == *str) ++str;
        do {
            if (_match1(pat + 2, str)) return true;
        } while(begin <= --str);
        return false;
    }
    if ((pat[0] == '.' && *str) || pat[0] == *str) {
        return _match1(pat + 1, str + 1);
    }
    return false;
}
bool find1(const char *pat, const char *str) {
    if (pat[0] == '^') return _match1(pat + 1, str);
    for (; *str; ++str) {
        if (_match1(pat, str)) return true;
    }
    return false;
}
//////////////////////////////
// always consume pattern
bool _matchCharacter2(const char *&pat, const char *&str) {
    bool succ;
    if (pat[0] == '\\') { 
        pat += 2;
        switch (pat[-1]) {
            case 'd': succ = isdigit(*str); break;
            case 'a': succ = isalpha(*str); break;
            case 'w': succ = isalnum(*str); break;
            default: succ = *str == pat[-1]; break;
        }
    } else if (pat[0] == '[') {
        char buf[128] = {0}; 
        while (*++pat != ']') {
            if (*pat == '\\') buf[*++pat] = 1;
            else buf[*pat] = 1;
        }
        ++pat;
        succ = buf[str[0]];
    } else {
        ++pat;
        succ = (pat[-1] == '.' && *str) || pat[-1] == *str;
    }
    if (succ) ++str;
    return succ;
}
bool _testCharacter2(const char *pat, const char *str) {
    return _matchCharacter2(pat, str);
}
bool _match2(const char *pat, const char *str) {
    if (pat[0] == 0) return true;
    if (pat[0] == '$') return str[0] == 0;
    if (str[0] == 0) return false;

    const char *rawPat = pat;
    bool succ = _matchCharacter2(pat, str);
    if (pat[0] == '?') {
        if (pat[1] == '?') {
            if (!succ) return _match2(pat + 2, str);
            return _match2(pat + 2, str - 1) || _match2(pat + 2, str);
        } else {
            if (!succ) return _match2(pat + 1, str);
            return _match2(pat + 1, str) || _match2(pat + 1, str - 1);
        }
    } else if (pat[0] == '*') {
        if (pat[1] == '?') {
            if (_match2(pat + 2, str - 1)) return true;
            if (!succ) return false;

            do {
                if (_match2(pat + 2, str)) return true;
            } while(_testCharacter2(rawPat, str++));
            return false;
        } else {
            if (!succ) return _match2(pat + 1, str);
            const char *begin = str - 1;
            while (_testCharacter2(rawPat, str)) ++str;
            do {
                if (_match2(pat + 1, str)) return true;
            } while(begin != --str);
            return _match2(pat + 1, str);
        }
    } else if (pat[0] == '+') {
        if (!succ) return false;
        if (pat[1] == '?') {
            do {
                if (_match2(pat + 2, str)) return true;
            } while (_testCharacter2(rawPat, str++));
            return false;
        } else {
            const char *begin = str - 1;
            while(_testCharacter2(rawPat, str)) ++str;
            do {
                if (_match2(pat + 1, str)) return true;
            } while(begin != --str);
            return false;
        }
    } else {
        return succ && _match2(pat, str);
    }
}
bool find2(const char *pat, const char *str) {
    if (pat[0] == '^') return _match2(pat + 1, str);
    for (; *str; ++str) {
        if (_match2(pat, str)) return true;
    }
    return false;
}
//////////////////////////////

void test(const char *name, bool(*f)(const char*, const char*)) {
    const char *testCase[][3] = {
        "def^", "abcdef^", "",
        "cde$", "abcdef", NULL,
        "def$", "abcdef", "",
        "^abcd", "abcdef", "",
        "^bcd", "abcdef", NULL,
        "a..f", "abcdef", NULL,
        "a.*f", "abbbbf", "",
        "^a*bb.*.9$", "aabb0099", "",
        "^a*b.*.9$", "aabb0099", "",
        "^e*abc$", "abc", "",
    };
    for (int i = 0; i < sizeof(testCase) / sizeof(testCase[0]); ++i) {
        if (f(testCase[i][0], testCase[i][1]) != (testCase[i][2] != NULL)) {
            printf("failed: %s- %d: %s , %s , %s", name, i, testCase[i][0], testCase[i][1], testCase[i][2] != NULL ? "true" : "false");
            assert(0);
        }
    }
}

void test2(const char *name, bool(*f)(const char*, const char*)) {
    const char *testCase[][3] = {
        "w+\\.baidu\\.com$", "www.baidu.com", "",
        "w+\\.baidu\\.com$", "www.baidu.com,", NULL,
        "\\w\\a+\\d*abc", "ab1234abc", "",
        "\\w\\a+\\d*abc", "ababc", "",
        "\\w\\a+\\d*abc", "aabc", NULL,
        "^a??bbb", "bbb", "",
        "^a??bbb", "abbb", "",
        "^a??bbb", "cbbb", NULL,
        "[ab\\]d]*", "bbad]]a", "",
        "[ab\\]d]*", "bbad]z]a", "",
        "^[ab\\]d]*$", "bbad]z]a", NULL,
    };
    for (int i = 0; i < sizeof(testCase) / sizeof(testCase[0]); ++i) {
        if (f(testCase[i][0], testCase[i][1]) != (testCase[i][2] != NULL)) {
            printf("failed: %s- %d: %s , %s , %s", name, i, testCase[i][0], testCase[i][1], testCase[i][2] != NULL ? "true" : "false");
            assert(0);
        }
    }
}

int main() {
    test("find1", &find1);
    test("find2", &find2);
    test2("find2", &find2);

    {
        clock_t start = clock();
#ifdef NDEBUG
        const int LOOP = 1000000;
#else
        const int LOOP = 10000;
#endif
        for (int i = 0; i < LOOP; ++i) {
            test("find2", &find2);
            test2("find2", &find2);
        }
        printf("%f sec\n", float(clock() - start) / CLOCKS_PER_SEC);
    }
}
