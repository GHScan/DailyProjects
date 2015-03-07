#include "stdafx.h"

//------------------------------
class Solution {
public:
    int lengthOfLastWord(const char *s) {
        if (s == nullptr) return 0;
        const char *p = s + strlen(s) - 1;
        for (; p >= s && *p == ' '; --p);
        int length = 0;
        for (; p >= s && *p != ' '; --p, ++length);
        return length;
    }
};

//------------------------------

int main() {
    Solution s;
    cout << s.lengthOfLastWord("") << endl;
    cout << s.lengthOfLastWord(" ABC") << endl;
    cout << s.lengthOfLastWord(" ABC  ") << endl;
    cout << s.lengthOfLastWord(" ABC D ") << endl;
    cout << s.lengthOfLastWord(" ABC DEFG ") << endl;
    cout << s.lengthOfLastWord("Hello World") << endl;
    cout << s.lengthOfLastWord("  Hello World  ") << endl;
}
