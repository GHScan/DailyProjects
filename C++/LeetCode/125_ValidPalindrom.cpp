#include "stdafx.h"

//-------------------------------------------------------------------------
#include <cctype>

class Solution {
public:
    bool isPalindrome(string const &s) {
        if (s.empty()) return true;

        const char *first = s.c_str();
        const char *last = first + s.size() - 1;
        do {
            if (!forward(first, last) || !backward(last, first)) return true;
        } while (toupper(*first++) == toupper(*last--));
        return false;
    }
private:
    bool forward(const char *&p, const char *end) {
        for (; p < end && !isalnum(*p); ++p);
        return p < end;
    }
    bool backward(const char *&p, const char *end) {
        for (; p > end && !isalnum(*p); --p);
        return p > end;
    }
};
//-------------------------------------------------------------------------

int main() {
    Solution so;

    cout << so.isPalindrome("") << endl;
    cout << so.isPalindrome("a") << endl;
    cout << so.isPalindrome("ab") << endl;
    cout << so.isPalindrome("abc") << endl;
    cout << so.isPalindrome("1a2") << endl;
    cout << so.isPalindrome("b abc") << endl;
    cout << so.isPalindrome("a b abc") << endl;
    cout << so.isPalindrome("c b abc") << endl;
    cout << so.isPalindrome("race a car") << endl;
    cout << so.isPalindrome("A man, a plan, a canal: Panama") << endl;
}
