#include "stdafx.h"
//------------------------------
class Solution {
public:
    int uniquePaths(int m, int n) {
        vector<int> mem(n * m, 0);
        for (int i = 0; i < n; ++i) mem[i] = 1;
        for (int i = 0; i < m; ++i) mem[i * n] = 1;
        for (int i = 1; i < m; ++i) {
            for (int j = 1; j < n; ++j) {
                mem[i * n + j] = mem[(i - 1) * n + j] + mem[i * n + j - 1];
            }
        }
        return mem.back();
    }
};
//------------------------------

int main() {
    Solution s;
    cout << s.uniquePaths(1, 1) << endl;
    cout << s.uniquePaths(2, 2) << endl;
    cout << s.uniquePaths(3, 3) << endl;
    cout << s.uniquePaths(3, 7) << endl;
    cout << s.uniquePaths(20, 20) << endl;
    cout << s.uniquePaths(100, 100) << endl;
}
