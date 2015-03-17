#include "stdafx.h"

//------------------------------
class Solution {
public:
    void setZeroes(vector<vector<int> > &matrix) {
        if (matrix.empty() || matrix.front().empty()) return;

        bool firstRow = false, firstColumn = false;
        for (auto v : matrix[0]) if (firstRow = (v == 0)) break;
        for (int i = 0; i < (int)matrix.size(); ++i) if (firstColumn = (matrix[i][0] == 0)) break;

        for (int i = 1; i < (int)matrix.size(); ++i) {
            auto &row = matrix[i];
            for (int j = 1; j < (int)row.size(); ++j) {
                if (row[j] == 0) {
                    matrix[0][j] = 0;
                    matrix[i][0] = 0;
                }
            }
        }

        for (int i = 1; i < (int)matrix.size(); ++i) {
            if (matrix[i][0] == 0) {
                for (auto &v : matrix[i]) v = 0;
            }
        }
        for (int j = 1; j < (int)matrix[0].size(); ++j) {
            if (matrix[0][j] == 0) {
                for (int i = 1; i < (int)matrix.size(); ++i) matrix[i][j] = 0;
            }
        }
        if (firstRow) {
            for (auto &v : matrix[0]) v = 0;
        }
        if (firstColumn) {
            for (int i = 0; i < (int)matrix.size(); ++i) matrix[i][0] = 0;
        }
    }
};
//------------------------------
static void printResult(vector<vector<int>> const &result) {
    cout << "#############" << endl;
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
}

int main() {
    Solution s;
    {
        vector<vector<int>> matrix({
        });
        s.setZeroes(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix({
            {3},
        });
        s.setZeroes(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix({
            { 0 },
        });
        s.setZeroes(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix({
            {1, 1, 1},
            { 1, 1, 1 },
            { 1, 1, 1 },
        });
        s.setZeroes(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix({
            { 1, 1, 1 },
            { 0, 1, 1 },
            { 1, 1, 1 },
        });
        s.setZeroes(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix({
            { 1, 1, 0 },
            { 0, 1, 1 },
            { 1, 1, 1 },
        });
        s.setZeroes(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix({
            { 1, 1, 1 },
            { 1, 0, 1 },
            { 1, 1, 1 },
        });
        s.setZeroes(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix({
            { 1, 1, 1 },
            { 1, 0, 0 },
            { 1, 0, 1 },
        });
        s.setZeroes(matrix);
        printResult(matrix);
    }
    {
        vector<vector<int>> matrix({
            { 1, 1, 1 },
            { 0, 0, 0 },
            { 1, 1, 1 },
        });
        s.setZeroes(matrix);
        printResult(matrix);
    }
}
