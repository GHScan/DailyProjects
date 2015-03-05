#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    int longestValidParentheses(string const &s) {
        int maxLen = 0;

        vector<pair<int, int>> workList;

        vector<int> markVec(s.size(), 0);
        for (int i = 0; i < (int)s.size() - 1; ++i) {
            if (s[i] == '(' && s[i + 1] == ')') {
                markVec[i] = markVec[i + 1] = 2;
                maxLen = 2;
                workList.push_back(make_pair(i, 2));
            }
        }

        while (!workList.empty()) {
            auto seg = workList.back();
            workList.pop_back();
            int off = seg.first, len = seg.second;
            if (markVec[off] != len || markVec[off + len - 1] != len) continue;
            markVec[off] = markVec[off + len - 1] = 0;

            while (off - 1 >= 0 && off + len < (int)s.size() && s[off - 1] == '(' && s[off + len] == ')') {
                off -= 1;
                len += 2;
            }

            bool combine = false;
            while (off - 1 >= 0 && markVec[off - 1] != 0) {
                int leftLen = markVec[off - 1];
                markVec[off - 1] = 0;
                off -= leftLen;
                markVec[off] = 0;
                len += leftLen;
                combine = true;
            }
            while (off + len < (int)s.size() && markVec[off + len] != 0) {
                int rightLen = markVec[off + len];
                markVec[off + len] = 0;
                len += rightLen;
                markVec[off + len - 1] = 0;
                combine = true;
            }

            markVec[off] = len;
            markVec[off + len - 1] = len;

            maxLen = max(len, maxLen);

            if (combine) {
                workList.push_back(make_pair(off, len));
            }
        }

        return maxLen;
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
