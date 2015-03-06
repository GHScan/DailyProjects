class Solution {
public:
    bool isValidSudoku(vector<vector<char> > &board) {
        for (int y = 0; y < 9; ++y) {
            if (!followRule(board, 0, y, 9)) return false;
        }
        for (int x = 0; x < 9; ++x) {
            if (!followRule(board, x, 0, 0)) return false;
        }
        for (int x = 0; x < 9; x += 3) {
            for (int y = 0; y < 9; y += 3) {
                if (!followRule(board, x, y, 3)) return false;
            }
        }
        return true;
    }
private:
    bool followRule(vector<vector<char> > &board, int initX, int initY, int width) {
        char occurs[10] = { 0 };
        int x = initX, y = initY;
        for (int i = 0; i < 9; ++i) {
            char c = board[y][x];

            if (c != '.') {
                if (occurs[c - '0'] != 0) {
                    return false;
                }
                occurs[c - '0'] = 1;
            }

            ++x;
            if (x - initX >= width) {
                x = initX;
                ++y;
            }
        }
        return true;
    }
};
