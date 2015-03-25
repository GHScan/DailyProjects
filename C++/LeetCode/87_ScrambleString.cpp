#include "stdafx.h"

#include <assert.h>

//-------------------------------------------------------------------------
#include <unordered_map>

inline size_t hashMerge(size_t seed, size_t h) {
    return seed ^ (h + 0x9e3779b9 + (seed << 6) + (seed >> 2));
}

struct StringRef {
    StringRef(const char *ptr, int len): mPtr(ptr), mLen(len), mHashCode(0) {
    }
    bool operator == (StringRef const &o) const {
        return mLen == o.mLen && hashCode() == o.hashCode() && memcmp(mPtr, o.mPtr, mLen) == 0;
    }
    size_t hashCode() const {
        if (mHashCode == 0) {
            mHashCode = hashMerge(0, 0);
            for (int i = 0; i < mLen; ++i) mHashCode = hashMerge(mHashCode, mPtr[i]);
        }
        return mHashCode;
    }
    char operator [] (int i) const {
        return mPtr[i];
    }
    size_t size() const {
        return mLen;
    }
    StringRef prefix(int len) const {
        return StringRef(mPtr, len);
    }
    StringRef suffix(int len) const {
        return StringRef(mPtr + mLen - len, len);
    }
    const char *begin() const { return mPtr; }
    const char *end() const { return mPtr + mLen; }

private:
    const char *mPtr;
    int mLen;
    mutable size_t mHashCode;
};

struct StringRefPairHash {
    size_t operator () (pair<StringRef, StringRef> const &p) const {
        return hashMerge(p.first.hashCode(), p.second.hashCode());
    }
};
struct StringRefPairEqual {
    bool operator () (pair<StringRef, StringRef> const &p1, pair<StringRef, StringRef> const &p2) const {
        return p1.first == p2.first && p1.second == p2.second;
    }
};

class Solution {
public:
    Solution() {
        memset(mZeroCharCount, 0, sizeof(mZeroCharCount));
    }

    bool isScramble(string const &s1, string const &s2) {
        if (s1.size() != s2.size()) return false;
        if (s1.empty()) return true;

        mMem.clear();
        return isScramble_(StringRef(s1.c_str(), (int)s1.size()), StringRef(s2.c_str(), (int)s2.size()));
    }
    
private:
    bool isScramble_(StringRef const &s1, StringRef const &s2) {
        if (s1.size() == 1) return s1[0] == s2[0];
        if (s1.size() == 2) {
            return (s1[0] == s2[0] && s1[1] == s2[1]) || (s1[0] == s2[1] && s1[1] == s2[0]);
        }

        pair<StringRef, StringRef> key(s1, s2);
        auto it = mMem.find(key);
        if (it != mMem.end()) return it->second;

        int charCount[256] = { 0 };
        for (auto c : s1) ++charCount[(unsigned char)c];
        for (auto c : s2) --charCount[(unsigned char)c];
        if (memcmp(charCount, mZeroCharCount, sizeof(charCount)) != 0) return mMem[key] = false;

        for (int i = 1; i <= (int)s1.size() - 1; ++i) {
            if (isScramble_(s1.prefix(i), s2.prefix(i)) && isScramble_(s1.suffix(s1.size() - i), s2.suffix(s2.size() - i))) {
                return mMem[key] = true;
            }
            if (isScramble_(s1.prefix(i), s2.suffix(i)) && isScramble_(s1.suffix(s1.size() - i), s2.prefix(s2.size() - i))) {
                return mMem[key] = true;
            }
        }
        return mMem[key] = false;
    }
private:
    unordered_map<pair<StringRef, StringRef>, bool, StringRefPairHash, StringRefPairEqual> mMem;
    int mZeroCharCount[256];
};
//-------------------------------------------------------------------------

int main() {
    Solution so;
    cout << so.isScramble("great", "rgtae") << endl;
    cout << so.isScramble("ccabcbabcbabbbbcbb", "bbbbabccccbbbabcba") << endl;
}
