#include "stdafx.h" 

//-----------------------------------------------------------------
class Solution {
public:
    bool isInterleave(string const &s1, string const &s2, string const &s3) {
        if (s1.size() + s2.size() != s3.size()) return false;

        vector<bool> mem[2];
        mem[0].resize(s2.size() + 1, false);
        mem[1].resize(s2.size() + 1, false);

        {
            vector<bool> &row = mem[0];
            row.back() = true;
            for (int i = (int)s2.size() - 1; i >= 0; --i) {
                row[i] = row[i + 1] && (s2[i] == s3[s3.size() - (s2.size() - i)]);
            }
        }

        int curr = 0;
        for (int i = (int)s1.size() - 1; i >= 0; --i) {
            int next = 1 - curr;

            mem[next][s2.size()] = mem[curr][s2.size()] && (s1[i] == s3[s3.size() - (s1.size() - i)]);
            for (int j = (int)s2.size() - 1; j >= 0; --j) {
                int c3 = s3[s3.size() - (s1.size() - i) - (s2.size() - j)];
                mem[next][j] = (s1[i] == c3 && mem[curr][j]) || (s2[j] == c3 && mem[next][j + 1]);
            }

            curr = next;
        }

        return mem[curr].front();
    }
};
//-----------------------------------------------------------------

int main() {
    Solution so;
    cout << so.isInterleave("", "", "") << endl;
    cout << so.isInterleave("", "", "a") << endl;
    cout << so.isInterleave("", "a", "a") << endl;
    cout << so.isInterleave("", "ab", "ab") << endl;
    cout << so.isInterleave("b", "a", "ab") << endl;
    cout << so.isInterleave("b", "a", "ac") << endl;
    cout << so.isInterleave("b", "c", "ac") << endl;
    cout << so.isInterleave("c", "a", "ac") << endl;
    cout << so.isInterleave("aabcc", "dbbca", "aadbbcbcac") << endl;
    cout << so.isInterleave("aabcc", "dbbca", "aadbbbaccc") << endl;
}
