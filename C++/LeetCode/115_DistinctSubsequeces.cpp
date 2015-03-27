#include "stdafx.h" 

//-----------------------------------------------------------------
class Solution {
public:
    int numDistinct(string const &S, string const &T) {
        if (T.empty() || S.empty()) return 0;

        vector<int> mem[2];
        mem[0].resize(S.size(), 0);
        mem[1].resize(S.size(), 0);

        mem[0].back() = S.back() == T.back();
        for (int i = S.size() - 2; i >= 0; --i) {
            mem[0][i] = (S[i] == T.back() ? 1 : 0) + mem[0][i + 1];
        }

        int curr = 0;
        for (int i = T.size() - 2; i >= 0; --i) {
            int next = 1 - curr;

            for (int j = S.size() - (T.size() - i) + 1; j < (int)S.size(); ++j) {
                mem[next][j] = 0;
            }
            for (int j = S.size() - (T.size() - i); j >= 0; --j) {
                mem[next][j] = (T[i] == S[j] ? mem[curr][j + 1] : 0) + mem[next][j + 1];
            }

            curr = next;
        }

        return mem[curr][0];
    }
};
//-----------------------------------------------------------------


int main() {
    Solution so;
    cout << so.numDistinct("", "") << endl;
    cout << so.numDistinct("abc", "") << endl;
    cout << so.numDistinct("", "abc") << endl;
    cout << so.numDistinct("aaa", "a") << endl;
    cout << so.numDistinct("aaa", "aa") << endl;
    cout << so.numDistinct("rabbbit", "rabbit") << endl;
    cout << so.numDistinct("rabbbbit", "rabbit") << endl;
}
