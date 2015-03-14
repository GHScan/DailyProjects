#include "stdafx.h"
//------------------------------
#include <limits>
#include <algorithm>

class Solution {
public:
    int minPathSum(vector<vector<int> > &grid) {
        if (grid.empty()) return numeric_limits<int>::max();
        int m = grid.size();
        int n = grid.front().size();

        vector<vector<int>> mem(m, vector<int>(n, 0));
        mem.front().front() = grid.front().front();
        for (int i = 1; i < n; ++i) mem[0][i] = mem[0][i - 1] + grid[0][i];
        for (int i = 1; i < m; ++i) mem[i][0] = mem[i - 1][0] + grid[i][0];

        for (int i = 1; i < m; ++i) {
            for (int j = 1; j < n; ++j) {
                mem[i][j] = min(mem[i - 1][j], mem[i][j - 1]) + grid[i][j];
            }
        }

        return mem.back().back();
    }
};
//------------------------------

int main() {
    Solution s;
    cout << s.minPathSum(vector<vector<int>>{
    }) << endl;
    cout << s.minPathSum(vector<vector<int>>{
        {0},
    }) << endl;
    cout << s.minPathSum(vector<vector<int>>{
        { 1 },
    }) << endl;
    cout << s.minPathSum(vector<vector<int>>{
        {1, 2},
        {0, 3},
    }) << endl;
    cout << s.minPathSum(vector<vector<int>>{
        { 1, 3 },
        { 2, 0 },
    }) << endl;
    cout << s.minPathSum(vector<vector<int>>{
        { 1, 1 },
        { 2, 2 },
    }) << endl;
    cout << s.minPathSum(vector<vector<int>>{
        { 1, 0, 0 },
        { 2, 3, 4 },
        { 0, 0, 5 },
    }) << endl;
    cout << s.minPathSum(vector<vector<int>>{
        { 1, 2, 3 },
        { 6, 5, 4 },
        { 7, 8, 9 },
    }) << endl;
    cout << s.minPathSum(vector<vector<int>>{
        { 1, 4, 7 },
        { 2, 5, 8 },
        { 3, 6, 9 },
    }) << endl;
}
