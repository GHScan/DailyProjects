#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    int minDistance(string const &word1, string const &word2) {
        if (word1.size() < word2.size()) return minDistance(word2, word1);

        vector<int> mems[2];
        mems[1].resize(word2.size() + 1, 0);
        for (int i = 0; i < (int)word2.size() + 1; ++i) {
            mems[0].push_back((int)word2.size() - i);
        }

        int curr = 0;
        for (int i = 0; i < (int)word1.size(); ++i) {
            int next = 1 - curr;

            char c1 = word1[word1.size() - 1 - i];
            mems[next].back() = i + 1;
            for (int j = (int)word2.size() - 1; j >= 0; --j) {
                char c2 = word2[j];
                mems[next][j] = c1 == c2 ? mems[curr][j + 1] : min(min(mems[curr][j], mems[next][j + 1]), mems[curr][j + 1]) + 1;
            }

            curr = next;
        }
        return mems[curr][0];
    }
};
//------------------------------

int main() {
    Solution s;
    cout << s.minDistance("", "") << endl;
    cout << s.minDistance("", "a") << endl;
    cout << s.minDistance("a", "") << endl;
    cout << s.minDistance("a", "a") << endl;
    cout << s.minDistance("ab", "") << endl;
    cout << s.minDistance("abc", "a") << endl;
    cout << s.minDistance("abcd", "ab1cd") << endl;
    cout << s.minDistance("abcd", "aecd") << endl;
    cout << s.minDistance("abcd", "defg") << endl;
    cout << s.minDistance("1abcd2", "1defg2") << endl;
    cout << s.minDistance("0abcd234", "1defg234") << endl;
}
