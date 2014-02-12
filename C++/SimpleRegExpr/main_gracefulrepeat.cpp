#include "pch.h" 

static bool wildcardMatch(const char *pat, const char *str) {
    if (pat[0] == 0) return str[0] == 0;
    if (pat[0] == '*') {
        while (str[0] != 0) {
            if (wildcardMatch(pat + 1, str)) return true;
            ++str;
        }
        return pat[1] == 0;
    } else {
        return pat[0] == str[0] && wildcardMatch(pat + 1, str + 1);
    }
}

static void test_wildcardMatch() {
    assert(!wildcardMatch("", "cba"));
    assert(wildcardMatch("*", ""));
    assert(wildcardMatch("*", "abc"));
    assert(!wildcardMatch("*a", "abc"));
    assert(wildcardMatch("*a", "cba"));
    assert(wildcardMatch("*a*", "cba"));
    assert(wildcardMatch("*a*", "cbade"));
    assert(wildcardMatch("*.cpp", "main.cpp"));
    assert(!wildcardMatch("*.cpp", "main.cpp.h"));
    assert(wildcardMatch("main*.cpp", "main.cpp"));
    assert(wildcardMatch("main*.cpp", "main.c.cpp"));
    assert(wildcardMatch("main*.cpp*", "main.c.cpp"));
    assert(wildcardMatch("main*.cpp*", "main.c.cpp.def"));
    assert(wildcardMatch("main*.cpp*.h", "main.c.cpp.def.h"));
    assert(wildcardMatch("main*.cpp*.h", "main.c.cpp.h"));
    assert(!wildcardMatch("main*.cpp*.h", "main.c.cpp.H"));
    assert(!wildcardMatch("main*.cpp*.h", ""));
}

static const char* _match(const char *pat, const char *str) {
    if (pat[0] == 0) return str;
    if (pat[0] == '$') return str[0] == 0 ? str : NULL;
    if (str[0] == 0) return NULL;

    char patChar = pat[0];
    int minCount = 1, maxCount = 1;
    bool isGready = true;
    switch (pat[1]) {
        case '+': minCount = 1, maxCount = 1 << 20; pat += 2; break;
        case '*': minCount = 0, maxCount = 1 << 20; pat += 2; break;
        case '?': minCount = 0, maxCount = 1; pat += 2; break;
        default: ++pat;
    }
    if ((pat[-1] == '+' || pat[-1] == '*' || pat[-1] == '?') && pat[0] == '?') {
        isGready = false;
        ++pat;
    }

    while (minCount > 0) {
        if (!(patChar == '.' || patChar == str[0])) return NULL;
        ++str;
        --minCount;
        --maxCount;
    }

    const char *end = _match(pat, str);
    if (isGready) {
        while (str[0] && maxCount > 0 && (patChar == '.' || patChar == str[0])) {
            ++str;
            --maxCount;
            if (const char *_end = _match(pat, str)) end = _end;
        }
    } else {
        while (end == NULL && str[0] && maxCount > 0 && (patChar == '.' || patChar == str[0])) {
            ++str;
            --maxCount;
            end = _match(pat, str);
        }
    }
    return end;
}
static const char* regSearch(const char *pat, const char *str, const char *&end) {
    if (pat[0] == '^') {
        if (end = _match(pat + 1, str)) return str;
        else return NULL;
    }
    do {
        if (end = _match(pat, str)) return str;
    } while(*str++ != 0);
    return NULL;
}
static string regSearch(const char *pat, const char *str) {
    const char *end;
    if (const char *begin = regSearch(pat, str, end)) return string(begin, end);
    else return string();
}

static void test_regSearch() {
    assert(regSearch("", "") == "");
    assert(regSearch("abc", "ab") == "");
    assert(regSearch("abc", "bc") == "");
    assert(regSearch("abc", "defabc") == "abc");
    assert(regSearch("abe", "defabc") == "");
    assert(regSearch("^abc.", "abc1abc2") == "abc1");
    assert(regSearch("abc.$", "abc1abc2") == "abc2");
    assert(regSearch("^abc$", "abc1abc2") == "");
    assert(regSearch("a.c", "abc1abc2") == "abc");
    assert(regSearch("a.+c", "abc1abc2") == "abc1abc");
    assert(regSearch("a.+?c", "abc1abc2") == "abc");
    assert(regSearch("a.*c", "abc1abc2") == "abc1abc");
    assert(regSearch("a.*?c", "abc1abc2") == "abc");
    assert(regSearch("ab?", "abc1abc2") == "ab");
    assert(regSearch("ab??", "abc1abc2") == "a");
    assert(regSearch("<tag>.+</tag>", "<tag>tag1</tag><tag>tag2</tag>") == "<tag>tag1</tag><tag>tag2</tag>");
    assert(regSearch("<tag>.+?</tag>", "<tag>tag1</tag><tag>tag2</tag>") == "<tag>tag1</tag>");
    assert(regSearch("<tag>.+?</tag>", "fjadkfjdslk <tag>tag1</tag><tag>tag2</tag> fsdajkfl") == "<tag>tag1</tag>");
    assert(regSearch("<charset>.*</charset>", "<title>abc<charset>GBK</charset></title><body><charset>utf-8</charset><tr/></body>") != "<charset>GBK</charset>");
    assert(regSearch("<charset>u.*?</charset>", "<title>abc<charset>GBK</charset></title><body><charset>utf-8</charset><tr/></body>") == "<charset>utf-8</charset>");
}

int main() {
    test_wildcardMatch();
    test_regSearch();
}
