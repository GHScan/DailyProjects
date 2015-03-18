#include "stdafx.h"

//------------------------------
class Solution {
public:
    bool exist(vector<vector<char> > &board, string const &word) {
        if (word.empty()) return true;

        vector<vector<bool>> trace;
        for (auto &row : board) trace.push_back(vector<bool>(row.size(), false));
        for (int y = 0; y < (int)board.size(); ++y) {
            for (int x = 0; x < (int)board[y].size(); ++x) {
                if (word[0] == board[y][x]) {
                    trace[y][x] = true;
                    if (go(board, y, x, trace, word.c_str() + 1)) return true;
                    trace[y][x] = false;
                }
            }
        }
        return false;
    }
private:
    bool go(vector<vector<char>> const &board, int y, int x, vector<vector<bool>> &trace, const char* str) {
        if (*str == 0) return true;
        int adjacents[][2] = {{x - 1, y}, {x + 1, y}, {x, y + 1}, {x, y - 1}};
        for (auto &xy : adjacents) {
            int ax = xy[0], ay = xy[1];
            if (ay < 0 || ay >= (int)board.size() || ax < 0 || ax >= (int)board[ay].size()) continue;
            if (*str != board[ay][ax]) continue;
            if (trace[ay][ax]) continue;
            trace[ay][ax] = true;
            if (go(board, ay, ax, trace, str + 1)) return true;
            trace[ay][ax] = false;
        }
        return false;
    }
};
//------------------------------
#define countOf(a) (sizeof(a) / sizeof(a[0]))

int main() {
    Solution s;
    cout << s.exist(vector<vector<char>>{
    }, "") << endl;
    cout << s.exist(vector<vector<char>>{
        {'a'},
    }, "") << endl;
    cout << s.exist(vector<vector<char>>{
        { 'a' },
    }, "a") << endl;
    cout << s.exist(vector<vector<char>>{
        { 'a', 'a' },
    }, "aaa") << endl;
    cout << s.exist(vector<vector<char>>{
        { 'a', 'b' },
        { 'c', 'd' },
    }, "abcd") << endl;
    cout << s.exist(vector<vector<char>>{
        { 'a', 'b' },
        { 'd', 'c' },
    }, "abcd") << endl;
    {
        vector<vector<char>> board({
            {'A', 'B', 'C', 'E'},
            { 'S', 'F', 'C', 'S' },
            { 'A', 'D', 'E', 'E' },
        });
        const char *strs[] = {
            "ABCCED", "SEE", "ABCB", "FSAD", "FSADF",
        };
        for (auto str : strs) cout << s.exist(board, str) << endl;
    }
}
