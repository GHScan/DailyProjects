#include "stdafx.h"

//------------------------------
class Solution {
public:
    vector<vector<string> > solveNQueens(int n) {
        vector<vector<string>> result;
        vector<int> path;
        tryPlace(n, result, path);
        return result;
    }
private:
    void tryPlace(int n, vector<vector<string>> &result, vector<int> &path) {
        if (n == (int)path.size()) {
            result.push_back(path2Board(n, path));
            return;
        }
        for (int x = 0; x < n; ++x) {
            if (!isValid(path, x)) continue;
            path.push_back(x);
            tryPlace(n, result, path);
            path.pop_back();
        }
    }
    bool isValid(vector<int> const &path, int x) {
        int y = (int)path.size();
        for (int y2 = 0; y2 < y; ++y2) {
            if (path[y2] == x || y - y2 == abs(path[y2] - x)) return false;
        }
        return true;
    }
    vector<string> path2Board(int n, vector<int> const &path) {
        vector<string> board;
        for (auto x : path) {
            string s;
            for (int i = 0; i < n; ++i) s += i == x ? 'Q' : '.';
            board.push_back(s);
        }
        return board;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))

static void printResult(vector<vector<string>> const &result) {
    cout << "###########" << endl;
    for (auto &board : result) {
        for (auto &s : board) cout << s << endl;
        cout << endl;
    }
}

int main() {
    Solution s;
    printResult(s.solveNQueens(2));
    printResult(s.solveNQueens(4));
    cout << s.solveNQueens(8).size() << endl;
}
