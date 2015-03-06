#include "stdafx.h"

//------------------------------
#include <limits>

class Solution {
public:
    void solveSudoku(vector<vector<char> > &board) {
        int emptyCount = 0;

        vector<vector<int>> killMaskBoard(9, vector<int>(9, 0));
        for (int y = 0; y < 9; ++y) {
            for (int x = 0; x < 9; ++x) {
                char c = board[y][x];
                if (c != '.') {
                    killMaskBoard[y][x] = 0x1ff;
                    fillKillMaskBoard(killMaskBoard, x, y, c);
                } else {
                    ++emptyCount;
                }
            }
        }

        tryFill(board, killMaskBoard, emptyCount);
    }
private:
    bool tryFill(vector<vector<char> > &board, vector<vector<int>> killMaskBoard, int emptyCount) {
        if (emptyCount == 0) return true;

        auto xy = findLeastChoicePos(killMaskBoard);
        if (xy.first == -1) return false;
        int x = xy.first, y = xy.second;

        int availables[9];
        int count = getAvailables(availables, killMaskBoard[y][x]);
        for (int i = 0; i < count; ++i) {
            char c = '1' + availables[i];

            assert(board[y][x] == '.');
            board[y][x] = c;

            vector<vector<int>> newKillMaskBoard(killMaskBoard);
            fillKillMaskBoard(newKillMaskBoard, x, y, c);
            newKillMaskBoard[y][x] = 0x1ff;

            if (tryFill(board, newKillMaskBoard, emptyCount - 1)) return true;

            board[y][x] = '.';
        }

        return false;
    }
    void fillKillMaskBoard(vector<vector<int>> &killMaskBoard, int placeX, int placeY, char c) {
        int mask = 1 << (c - '1');
        for (int y = 0; y < 9; ++y) {
            for (int x = 0; x < 9; ++x) {
                if (x == placeX || y == placeY || (x / 3 == placeX / 3 && y / 3 == placeY / 3)) {
                    killMaskBoard[y][x] |= mask;
                }
            }
        }
    }
    int availableCount(int killMask) {
        int liveMask = killMask ^ 0x1ff;
        int count = 0;
        for (; liveMask > 0; ++count) liveMask &= liveMask - 1;
        return count;
    }
    pair<int, int> findLeastChoicePos(vector<vector<int>> const &killMaskBoard) {
        int posX = -1, posY = -1;
        int minCount = numeric_limits<int>::max();
        for (int y = 0; y < 9; ++y) {
            for (int x = 0; x < 9; ++x) {
                int count = availableCount(killMaskBoard[y][x]);
                if (count > 0 && count < minCount) {
                    minCount = count;
                    posX = x, posY = y;
                }
            }
        }
        return make_pair(posX, posY);
    }
    int getAvailables(int availabes[9], int killMask) {
        int liveMask = killMask ^ 0x1ff;
        int count = 0;
        for (int i = 0; i < 9; ++i) {
            if (liveMask & (1 << i)) {
                availabes[count++] = i;
            }
        }
        return count;
    }
};

//------------------------------

static vector<vector<char>> buildBoard(vector<string> const &board) {
    vector<vector<char>> result;
    for (auto &s : board) result.push_back(vector<char>(s.begin(), s.end()));
    return result;
}
static void printBoard(vector<vector<char>> const &board) {
    for (auto &row : board) {
        for (auto c : row) cout << c;
        cout << endl;
    }
    cout << endl;
}

int main() {
    Solution s;
    {
        auto board = 
            buildBoard(vector<string>{
            "53..7....",
                "6..195...",
                ".98....6.",

                "8...6...3",
                "4..8.3..1",
                "7...2...6",

                ".6....28.",
                "...419..5",
                "....8..79",
        });
        s.solveSudoku(board);
        printBoard(board);
    }
    {
        auto board =
            buildBoard(vector<string>{
                "..9748...", 
                    "7........", 
                    ".2.1.9...", 
                    "..7...24.", 
                    ".64.1.59.", 
                    ".98...3..", 
                    "...8.3.2.", 
                    "........6", 
                    "...2759..",
        });
        s.solveSudoku(board);
        printBoard(board);
    }
}
