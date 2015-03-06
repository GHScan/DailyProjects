#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    string const *mStr;
    int longestValidParentheses(string const &s) {
        mStr = &s;

        vector<pair<int, int>> parentheses;
        for (int i = 0; i < (int)s.size() - 1; ) {
            if (s[i] == '(' && s[i + 1] == ')') {
                addParenthese(parentheses, i, 2);
                i = parentheses.back().first + parentheses.back().second;
            } else {
                ++i;
            }
        }

        int maxLen = 0;
        for (auto p : parentheses) maxLen = max(maxLen, p.second);
        return maxLen;
    }
    void addParenthese(vector<pair<int, int>> &parentheses, int off, int len) {
        parentheses.push_back(make_pair(off, len));
        do {
            parentheses.back() = extend(parentheses.back());
        } while(combineSuffix(parentheses));
    }
    pair<int, int> extend(pair<int, int> parenthese) {
        int off = parenthese.first, len = parenthese.second;
        while (off - 1 >= 0 && off + len < (int)mStr->size() && (*mStr)[off - 1] == '(' && (*mStr)[off + len] == ')') {
            --off;
            len += 2;
        }
        return make_pair(off, len);
    }
    bool combineSuffix(vector<pair<int, int>> &parentheses) {
        if (parentheses.empty()) return false;

        bool combined = false;

        int off = parentheses.back().first, len = parentheses.back().second;
        parentheses.pop_back();
        while (!parentheses.empty()) {
            if (parentheses.back().first + parentheses.back().second != off) break;
            len += parentheses.back().second;
            off = parentheses.back().first;
            parentheses.pop_back();
            combined = true;
        }
        parentheses.push_back(make_pair(off, len));

        return combined;
    }
};

//------------------------------

int main() {
    Solution s;
    cout << s.longestValidParentheses("") << endl;
    cout << s.longestValidParentheses(")(") << endl;
    cout << s.longestValidParentheses("()(") << endl;
    cout << s.longestValidParentheses("())(") << endl;
    cout << s.longestValidParentheses("()") << endl;
    cout << s.longestValidParentheses(")()())") << endl;
    cout << s.longestValidParentheses("()((((()()()((()()") << endl;
    cout << s.longestValidParentheses(")()()(()()()))()") << endl;
    cout << s.longestValidParentheses("(()))((((())()()((()()()))()())") << endl;
}
