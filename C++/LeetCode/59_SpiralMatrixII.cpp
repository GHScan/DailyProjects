#include "stdafx.h"

//------------------------------
class Solution {
public:
    vector<vector<int> > generateMatrix(int n) {
        vector<vector<int>> matrix(n, vector<int>(n, 0));
        traverseRect(matrix, 0, n, 1);
        return matrix;
    }
private:
    void traverseRect(vector<vector<int> > &matrix, int depth, int width, int num) {
        if (width <= 0) return;
        for (int i = 0; i < width; ++i) matrix[depth][depth + i] = num++;
        for (int i = 1; i < width; ++i) matrix[depth + i][depth + width - 1] = num++;
        for (int i = width - 2; i >= 0; --i) matrix[depth + width - 1][depth + i] = num++;
        for (int i = width - 2; i > 0; --i) matrix[depth + i][depth] = num++;
        traverseRect(matrix, depth + 1, width - 2, num);
    }
};
//------------------------------
static void printResult(vector<vector<int>> &result) {
    for (auto &v : result) {
        for (auto i : v) cout << i << ',';
        cout << endl;
    }
    cout << endl;
}

int main() {
    Solution s;
    printResult(s.generateMatrix(0));
    printResult(s.generateMatrix(1));
    printResult(s.generateMatrix(2));
    printResult(s.generateMatrix(3));
    printResult(s.generateMatrix(4));
    printResult(s.generateMatrix(5));
}
