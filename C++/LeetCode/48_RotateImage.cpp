#include "stdafx.h"

//------------------------------
#include <algorithm>

class Solution {
public:
    void rotate(vector<vector<int>> &matrix) {
        flipVertically(matrix);
        transpose(matrix);
    }
private:
    void transpose(vector<vector<int>> &matrix) {
        for (int y = 0; y < (int)matrix.size(); ++y) {
            for (int x = y; x < (int)matrix.size(); ++x) {
                swap(matrix[y][x], matrix[x][y]);
            }
        }
    }
    void flipVertically(vector<vector<int>> &matrix) {
        for (int y = 0; y < (int)matrix.size() / 2; ++y) {
            swap(matrix[y], matrix[matrix.size() - 1 - y]);
        }
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

static void printResult(vector<vector<int>> &result) {
    cout << "#######" << endl;
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution s;
    {
        vector<vector<int>> matrix{
            { 1 },
    };
        s.rotate(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix{
            { 1, 2 },
            { 3, 4 },
    };
        s.rotate(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix{
            { 1, 2, 3, },
            { 4, 5, 6, },
            { 7, 8, 9, },
    };
        s.rotate(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix{
            { 1, 2, 3, 4, },
            { 5, 6, 7, 8, },
            { 9, 10, 11, 12, },
            { 13, 14, 15, 16, },
    };
        s.rotate(matrix);
        printResult(matrix);
    }
}
