#include "stdafx.h"

//------------------------------
class Solution {
public:
    int totalNQueens(int n) {
        vector<int> path;
        return tryPlace(n, path);
    }
private:
    int tryPlace(int n, vector<int> &path) {
        if (n == (int)path.size()) return 1;
        int count = 0;
        for (int x = 0; x < n; ++x) {
            if (!isValid(path, x)) continue;
            path.push_back(x);
            count += tryPlace(n, path);
            path.pop_back();
        }
        return count;
    }
    bool isValid(vector<int> const &path, int x) {
        int y = (int)path.size();
        for (int y2 = 0; y2 < y; ++y2) {
            if (path[y2] == x || y - y2 == abs(path[y2] - x)) return false;
        }
        return true;
    }
};

//------------------------------
int main() {
    Solution s;
    for (int i = 1; i <= 10; ++i) {
        cout << i << '=' << s.totalNQueens(i) << endl;
    }
}
