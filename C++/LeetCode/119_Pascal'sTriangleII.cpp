#include "stdafx.h" 

//-----------------------------------------------------------------
class Solution {
public:
    vector<int> getRow(int rowIndex) {
        vector<int> mem[2];
        mem[0].resize(rowIndex + 1);
        mem[1].resize(rowIndex + 1);

        mem[0][0] = 1;
        for (int i = 0; i < rowIndex; ++i) {
            vector<int> &curr = mem[i % 2], &next = mem[1 - i % 2];
            next[0] = next[i + 1] = 1;
            for (int j = 1; j <= i; ++j) {
                next[j] = curr[j - 1] + curr[j];
            }
        }
        return mem[rowIndex % 2];
    }
};
//-----------------------------------------------------------------
static void printResult(vector<int> const &result) {
    for (auto i : result) cout << i << ',';
    cout << endl;
}

int main() {
    Solution so;
    for (int i = 0; i < 6; ++i) printResult(so.getRow(i));
}
