#include "stdafx.h"

#include <assert.h>

//-------------------------------------------------------------------------
#include <algorithm>

class Solution {
public:
    int maximalRectangle(vector<vector<char> > const &matrix) {
        if (matrix.empty()) return 0;

        auto histograms = buildHistograms(matrix);
        int result = 0;
        for (auto &histogram : histograms) {
            result = max(result, largestRectangleArea(histogram));
        }
        return result;
    }
private:
    vector<vector<int>> buildHistograms(vector<vector<char> > const &matrix) {
        vector<vector<int>> histograms(matrix.size(), vector<int>(matrix[0].size(), 0));
        for (int j = 0; j < (int)matrix[0].size(); ++j) histograms[0][j] = matrix[0][j] == '1' ? 1 : 0;
        for (int i = 1; i < (int)matrix.size(); ++i) {
            for (int j = 0; j < (int)matrix[i].size(); ++j) {
                if (matrix[i][j] == '1') histograms[i][j] = histograms[i - 1][j] + 1;
                else histograms[i][j] = 0;
            }
        }
        return histograms;
    }
    int largestRectangleArea(vector<int> &height) {
        height.push_back(0);

        int result = 0;
        vector<int> stack;
        for (int i = 0; i < (int)height.size(); ++i) {
            while (!stack.empty() && height[stack.back()] >= height[i]) {
                int h = height[stack.back()];
                stack.pop_back();
                result = max(result, h * (stack.empty() ? i : (i - stack.back() - 1)));
            }
            stack.push_back(i);
        }

        height.pop_back();
        return result;
    }
};
//-------------------------------------------------------------------------

int main() {
    Solution so;

    cout << so.maximalRectangle(vector<vector<char>>{
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        {'1'},
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '0' },
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '0', '0' },
        { '0', '1' },
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '1', '0' },
        { '0', '1' },
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '1', '0' },
        { '1', '1' },
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '1', '1' },
        { '1', '1' },
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '1', '1', '0' },
        { '1', '1', '0' },
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '1', '1', '0' },
        { '1', '1', '0' },
        { '1', '1', '1' },
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '1', '1', '1' },
        { '1', '0', '0' },
        { '1', '1', '1' },
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '1', '1', '1' },
        { '1', '0', '1' },
        { '1', '1', '1' },
    }) << endl;

    cout << so.maximalRectangle(vector<vector<char>>{
        { '1', '1', '1' },
        { '1', '1', '1' },
        { '1', '1', '1' },
    }) << endl;
}
