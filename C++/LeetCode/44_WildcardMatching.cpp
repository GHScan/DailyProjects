#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    bool isMatch(const char *s, const char *p) {
        if (!*s && !*p) return true;
        else if (!*p) return false;
        else switch (*p) {
        case '?':
            return *s && isMatch(s + 1, p + 1);
        case '*':
            do {
                if (isMatch(s, p + 1)) return true;
            } while(*s++);
            return false;
        default:
            return *s == *p && isMatch(s + 1, p + 1);
        }
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

int main() {
    Solution s;
    cout << s.isMatch("", "") << endl;
    cout << s.isMatch("", "a") << endl;
    cout << s.isMatch("a", "") << endl;
    cout << s.isMatch("?", "") << endl;
    cout << s.isMatch("", "?") << endl;
    cout << s.isMatch("*", "") << endl;
    cout << s.isMatch("", "*") << endl;
    cout << s.isMatch("aa", "a") << endl;
    cout << s.isMatch("aa", "aa") << endl;
    cout << s.isMatch("aaa", "aa") << endl;
    cout << s.isMatch("aa", "*") << endl;
    cout << s.isMatch("aa", "a*") << endl;
    cout << s.isMatch("ab", "?*") << endl;
    cout << s.isMatch("aab", "c*a*b") << endl;
}
