#include "stdafx.h"
//------------------------------
class Solution {
public:
    int uniquePathsWithObstacles(vector<vector<int> > &obstacleGrid) {
        if (obstacleGrid.empty()) return 0;
        int m = (int)obstacleGrid.size();
        int n = (int)obstacleGrid.front().size();

        vector<vector<int>> vec(m, vector<int>(n, 0));
        vec.front().front() = obstacleGrid.front().front() == 1 ? 0 : 1;
        for (int i = 1; i < n; ++i) vec[0][i] = obstacleGrid[0][i] == 0 && vec[0][i - 1] == 1 ? 1 : 0;
        for (int i = 1; i < m; ++i) vec[i][0] = obstacleGrid[i][0] == 0 && vec[i - 1][0] == 1 ? 1 : 0;
        for (int i = 1; i < m; ++i) {
            for (int j = 1; j < n; ++j) {
                vec[i][j] = obstacleGrid[i][j] == 1 ? 0 : vec[i - 1][j] + vec[i][j - 1];
            }
        }
        return vec.back().back();
    }
};
//------------------------------

int main() {
    Solution s;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
    }) << endl;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
        {0},
    }) << endl;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
        { 1 },
    }) << endl;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
        {0, 0},
        {0, 0},
    }) << endl;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
        { 0, 1 },
        { 1, 0 },
    }) << endl;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
        { 0, 1 },
        { 0, 0 },
    }) << endl;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
        { 0, 0, 0 },
        { 0, 1, 0 },
        { 0, 0, 0 },
    }) << endl;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
        { 0, 0, 0 },
        { 1, 0, 1 },
        { 0, 0, 0 },
    }) << endl;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
        { 0, 1, 0 },
        { 0, 0, 0 },
        { 0, 1, 0 },
    }) << endl;
    cout << s.uniquePathsWithObstacles(vector<vector<int>>{
        { 0, 0, 0, 1 },
        { 0, 1, 0, 0 },
        { 0, 0, 0, 0 },
        { 0, 0, 1, 0 },
    }) << endl;
}
