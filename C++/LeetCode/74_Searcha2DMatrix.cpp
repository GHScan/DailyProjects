#include "stdafx.h"

//------------------------------
class Solution {
public:
    bool searchMatrix(vector<vector<int> > &matrix, int target) {
        if (matrix.empty() || matrix[0].empty()) return false;
        if (matrix[0][0] > target) return false;
        int row;
        if (matrix.back()[0] <= target) {
            if (matrix.back().back() <= target) return matrix.back().back() == target;
            row = (int)matrix.size() - 1;
        } else {
            row = 0;
            int last = (int)matrix.size() - 1;
            while(row + 1 < last) {
                int mid = row + (last - row) / 2; 
                if (matrix[mid][0] <= target) row = mid;
                else last = mid;
            }
            if (matrix[row].back() <= target) return matrix[row].back() == target;
        }

        int first = 0, last = (int)matrix[row].size() - 1;
        while (first + 1 < last) {
            int mid = first + (last - first) / 2;
            if (matrix[row][mid] <= target) first = mid;
            else last = mid;
        }
        return matrix[row][first] == target;
    }
};
//------------------------------

int main() {
    Solution s;
    {
        vector<vector<int>> matrix({
        });
        cout << s.searchMatrix(matrix, 1) << endl;
    }
    {
        vector<vector<int>> matrix({
            {1},
        });
        cout << "####################" << endl;
        for (int i = 0; i < 3; ++i) cout << s.searchMatrix(matrix, i) << ',';
        cout << endl;
    }
    {
        vector<vector<int>> matrix({
            { 1, 1 },
            { 1, 1 },
        });
        cout << "####################" << endl;
        for (int i = 0; i < 3; ++i) cout << s.searchMatrix(matrix, i) << ',';
        cout << endl;
    }
    {
        vector<vector<int>> matrix({
            { 1, 1 },
            { 1, 2 },
        });
        cout << "####################" << endl;
        for (int i = 0; i < 4; ++i) cout << s.searchMatrix(matrix, i) << ',';
        cout << endl;
    }
    {
        vector<vector<int>> matrix({
            { 1, 1 },
            { 2, 2 },
        });
        cout << "####################" << endl;
        for (int i = 0; i < 4; ++i) cout << s.searchMatrix(matrix, i) << ',';
        cout << endl;
    }
    {
        vector<vector<int>> matrix({
            { 1, 2 },
            { 2, 3 },
        });
        cout << "####################" << endl;
        for (int i = 0; i < 5; ++i) cout << s.searchMatrix(matrix, i) << ',';
        cout << endl;
    }
    {
        vector<vector<int>> matrix({
            { 1, 2 },
            { 3, 3 },
        });
        cout << "####################" << endl;
        for (int i = 0; i < 5; ++i) cout << s.searchMatrix(matrix, i) << ',';
        cout << endl;
    }
    {
        vector<vector<int>> matrix({
            { 1, 3 },
            { 3, 3 },
        });
        cout << "####################" << endl;
        for (int i = 0; i < 5; ++i) cout << s.searchMatrix(matrix, i) << ',';
        cout << endl;
    }
}
