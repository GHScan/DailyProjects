#include "pch.h"

#include <functional>

#include "Utils.h"

struct IMatcher {
    virtual ~IMatcher() {}
    virtual void build(const char *pat) = 0;
    virtual const char* find(const char *str) = 0;
};

class SundayMatcher: public IMatcher {
public:
    virtual void build(const char *pat) {
        mPat = pat;
        for (int i = 0; i < 256; ++i) mAdvanceTable[i] = (int)mPat.size() + 1;
        for (const char *p = pat; *p; ++p) mAdvanceTable[(unsigned char)*p] = (int)mPat.size() - (p - pat);
    }
    virtual const char* find(const char *str) {
        int patLen = (int)mPat.size();
        int len = strlen(str);
        if (patLen == 0) return str;
        if (len < patLen) return nullptr;
        const char *pat = mPat.c_str();

        for (int i = 0; i <= len - patLen; ) {
            if (memcmp(str + i, pat, patLen) == 0) return str + i;
            i += mAdvanceTable[(unsigned char)str[i + patLen]];
        }

        return nullptr;
    }
private:
    int mAdvanceTable[256];
    string mPat;
};

class KMPMatcher: public IMatcher {
public:
    virtual void build(const char *pat) {
        mPat = pat;
        mNextTable.resize(mPat.size());
        if (mPat.empty()) return;

        mNextTable[0] = -1;
        for (int i = 1; i < (int)mPat.size(); ++i) {
            mNextTable[i] = -1;
            for (int next = mNextTable[i - 1]; ; next = mNextTable[next]) {
                if (mPat[next + 1] == mPat[i]) {
                    mNextTable[i] = next + 1;
                    break;
                }
                if (next == -1) break;
            }
        }
    }
    virtual const char* find(const char *str) {
        int patLen = (int)mPat.size();
        int len = strlen(str);
        if (patLen == 0) return str;
        if (len < patLen) return nullptr;
        int *nextTable = &mNextTable[0];
        const char *pat = mPat.c_str();

        for (int patOff = 0, i = 0; i < len; ++i) {
            while (patOff > 0 && pat[patOff] != str[i]) {
                patOff = nextTable[patOff - 1] + 1;
            }
            if (str[i] == pat[patOff]) {
                if (++patOff == patLen) return str + i + 1 - patLen;
            }
        }

        return nullptr;
    }
    void dump() {
        printf("kmp, pat=%s\n", mPat.c_str());
        for (int next : mNextTable) printf("%d,", next);
        puts("");
    }
private:
    vector<int> mNextTable;
    string mPat;
};

static const char* memcmpSearch(const char *str, const char *pat) {
    int patLen = strlen(pat);
    int len = strlen(str);
    if (patLen == 0) return str;
    if (len < patLen) return nullptr;

    for (int i = 0; i <= len - patLen; ++i) {
        if (memcmp(str + i, pat, patLen) == 0) return str + i;
    }

    return nullptr;
}
static const char* simpleSearch(const char *str, const char *pat) {
    int patLen = strlen(pat);
    int len = strlen(str);
    if (patLen == 0) return str;
    if (len < patLen) return nullptr;

    for (int i = 0; i <= len - patLen; ++i) {
        int j = 0;
        while (j < patLen && str[i + j] == pat[j]) ++j;
        if (j == patLen) return str + i;
    }

    return nullptr;
}
class FuncMatcher: public IMatcher {
public:
    FuncMatcher(const char* (*f)(const char *, const char *)): mF(f){}
    virtual void build(const char *pat) {
        mPat = pat;
    }
    virtual const char* find(const char *str) {
        return mF(str, mPat.c_str());
    }
private:
    const char* (*mF)(const char *, const char *);
    string mPat;
};

struct FuncItem {
    const char *name;
    function<IMatcher*()> factory;
};

static void correctnessTest(vector<FuncItem> &funcs) {
    for (auto &func : funcs) {
        const char *pats[] = {
            "",
            "a",
            "aaaa",
            "abb",
            "aa bb",
            "aa baabb abcb",
        };
        const char *strs[] = {
            "",
            "b",
            "ba",
            "bdab",
            "baaabbbaabbaaaab",
            "abbaaaba",
            "abbaaab  aabba bba",
            "abbaaab  aabba aa baabb abcbabba",
        };
        for (auto pat : pats) {
            for (auto str : strs) {
                auto matcher = func.factory();
                matcher->build(pat);
                if (matcher->find(str) != strstr(str, pat)) {
                    printf("failed! func=%s, str=%s, pat=%s\n", func.name, str, pat);
                    assert(0);
                }
                delete matcher;
            }
        }
    }
}

static void benchmark(vector<FuncItem> &funcs) {
    string longStr = checkoutCmd("man bash");
    vector<string> keywords = splitToKeywords(longStr);
    keywords.erase(
            remove_if(keywords.begin(), keywords.end(), [](const string &str){ 
                for (char c : str) if (!isalpha(c)) return true;
                if (str.size() < 3) return true;
                return false;
            }), keywords.end());
    random_shuffle(keywords.begin(), keywords.end());
    keywords.resize(min((int)keywords.size(), 256));

    puts("benchmark:");
    for (auto &func : funcs) {
        auto matcher = func.factory();

        vector<double> times(8);
        for (double &time : times) {
            time = getTime();
            for (auto &keyword : keywords) {
                matcher->build(keyword.c_str());
                for (const char *p = longStr.c_str(); ; ) {
                    p = matcher->find(p);
                    if (p == nullptr) break;
                    else ++p;
                }

            }
            time = getTime() - time;
        }
        sort(times.begin(), times.end());
        printf("\t%s : %f\n", func.name, accumulate(times.begin(), times.begin() + 3, 0.0) / 3);

        delete matcher;
    }
}

static void benchmarkLongStr(vector<FuncItem> &funcs) {
    string longStr = "abcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcabcaaaaaaaaab aaaaaabbbbbbbba bbbbbbbbbbaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbabab abaaaaaaaaaaaaabbabcccccccccabacaaaaaaaaaaaaaaaaabcccccccccccccaba        babacaaaaaaaaaaaaabaabbbbbbbbbbbbbbbbbbbbccbcbcbbbcbcbcabbbbbbbbbbbbcbacbab aaaaaaaaab aaaaaabbbbbbbba bbbbbbbbbbaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbabab abaaaaaaaaaaaaabbabcccccccccabacaaaaaaaaaaaaaaaaabcccccccccccccaba        babacaaaaaaaaaaaaabaabbbbbbbbbbbbbbbbbbbbccbcbcbbbcbcbcabbbbbbbbbbbbcbacbab";
    const char * keywords[] = {
        "bbbbbbccbcbcbbbcbcbcabbbbbbbbbbbbcbacbab aaaaaaaaab aaaaaabbbbbbbba bbbbbbbbbbaaaaaaaaa",
        "aaaaaaaaab aaaaaabbbbbbbba bbbbbbbbbbaaaaaaaaaaaaaaaaaaaaaabbbbbbbbbbbbbbaba",
        "aaaaaaaaaaabbabcccccccccabacaaaaaaaaaaaaaaaaabcccccccccccccaba        babacaaaaaaaaaaaaabaabbbbbbbbbbbbbbbbbbbbccb",
        "aaaaaaaaaaaaabaabbbbbbbbbbbbbbbbbbbbccbd",
    };

    puts("benchmark longStr:");
    for (auto &func : funcs) {
        auto matcher = func.factory();

        vector<double> times(8);
        for (double &time : times) {
            time = getTime();
            for (auto &keyword : keywords) {
                matcher->build(keyword);
                for (int n = 0; n < 2024; ++n)
                for (const char *p = longStr.c_str(); ; ) {
                    p = matcher->find(p);
                    if (p == nullptr) break;
                    else ++p;
                }

            }
            time = getTime() - time;
        }
        sort(times.begin(), times.end());
        printf("\t%s : %f\n", func.name, accumulate(times.begin(), times.begin() + 3, 0.0) / 3);

        delete matcher;
    }
}

int main() {
    srand(time(nullptr));
    setCpuAffinity(1);

#define ITEM(cmd) {#cmd, []()->IMatcher* { cmd; },}

    {
        vector<FuncItem> funcs = {
            ITEM(return new FuncMatcher(simpleSearch); ),
            ITEM(return new FuncMatcher(memcmpSearch); ),
            ITEM(return new FuncMatcher(strstr); ),
            ITEM(return new SundayMatcher();),
            ITEM(return new KMPMatcher();),
        };
        correctnessTest(funcs);
        benchmark(funcs);
        benchmarkLongStr(funcs);
    }
#undef ITEM
}
