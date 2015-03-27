#include "stdafx.h" 

//-----------------------------------------------------------------
#include <algorithm>
#include <limits>

class Solution {
public:
    int minimumTotal(vector<vector<int> > &triangle) {
        if (triangle.empty()) return numeric_limits<int>::max();

        vector<int> mem[2];
        mem[0].resize(triangle.back().size(), 0);
        mem[1].resize(triangle.back().size(), 0);

        mem[0][0] = triangle[0][0];
        int curr = 0;
        for (int i = 1; i < (int)triangle.size(); ++i) {
            int next = 1 - curr;
            
            mem[next][0] = mem[curr][0] + triangle[i][0];
            mem[next][i] = mem[curr][i - 1] + triangle[i][i];
            for (int j = 1; j < i; ++j) {
                mem[next][j] = min(mem[curr][j - 1], mem[curr][j]) + triangle[i][j];
            }

            curr = next;
        }
        return *min_element(mem[curr].begin(), mem[curr].end());
    }
};
//-----------------------------------------------------------------

int main() {
    Solution so;
    cout << so.minimumTotal(vector<vector<int>>{
    }) << endl;
    cout << so.minimumTotal(vector<vector<int>>{
        {1},
    }) << endl;
    cout << so.minimumTotal(vector<vector<int>>{
        { 1 },
        { 2, 3 },
    }) << endl;
    cout << so.minimumTotal(vector<vector<int>>{
        { 1 },
        { 2, 3 },
        { 1, 2, 4 },
    }) << endl;
    cout << so.minimumTotal(vector<vector<int>>{
        { 2 },
        { 3, 4 },
        { 6, 5, 7 },
        { 4, 1, 8, 3 },
    }) << endl;
}
