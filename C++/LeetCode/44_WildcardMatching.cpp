#include "stdafx.h"

//------------------------------
class Solution {
public:
    bool isMatch(const char *s, const char *p) {
        const char *lasts = nullptr, *lastp = nullptr;
        while (*s) {
            if (*p == '*') {
                lastp = ++p;
                lasts = s + 1;
            } else if (*p == '?' || *p == *s) {
                ++s;
                ++p;
            } else {
                if (lastp == nullptr) return false;
                p = lastp;
                s = lasts++;
            }
        }
        for (; *p == '*'; ++p);
        return *p == 0;
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
    cout << s.isMatch("a", "??") << endl;
    cout << s.isMatch("aa", "??") << endl;
    cout << s.isMatch("aaa", "??") << endl;
    cout << s.isMatch("aaa", "??*") << endl;
    cout << s.isMatch("aaa", "?*?*") << endl;
    cout << s.isMatch("aa", "*a*") << endl;
    cout << s.isMatch("bb", "*a*") << endl;
    cout << s.isMatch("abbabaaabbabbaababbabbbbbabbbabbbabaaaaababababbbabababaabbababaabbbbbbaaaabababbbaabbbbaabbbbababababbaabbaababaabbbababababbbbaaabbbbbabaaaabbababbbbaababaabbababbbbbababbbabaaaaaaaabbbbbaabaaababaaaabb", "**aa*****ba*a*bb**aa*ab****a*aaaaaa***a*aaaa**bbabb*b*b**aaaaaaaaa*a********ba*bbb***a*ba*bb*bb**a*b*bb") << endl;
    cout << s.isMatch("aaabbbaabaaaaababaabaaabbabbbbbbbbaabababbabbbaaaaba", "a*******b") << endl;
    cout << s.isMatch("baaaaaaaaabaaabaabaabbaabbbabbabaabaabbaaabaaaabbabaabababaababbaabaaababaabababbbabbabbabbbbaabbbbbaabbabaaabaaaabbaababababbbabbbbbaaabaaabaaaabbbaabbbbaaabababaabaaabaaabaabaaaaaabbbbbbbbbbabbbababaabb", "b***a*b**a***bbb****a*ba*b*bb***abbaa****bbb***bbabb**b**a*a*****a**********bb**ba*a*a*bab***b**bbbaa") << endl;
    {
        string str(32316, 'a');
        string pat = '*' + str + "a*";
        cout << s.isMatch(str.c_str(), pat.c_str()) << endl;
        pat = '*' + str + "*";
        cout << s.isMatch(str.c_str(), pat.c_str()) << endl;
    }
}
