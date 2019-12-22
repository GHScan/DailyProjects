#include "stdafx.h"

//----------------------------------------------

#include <string>
#include <vector>
#include <queue>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    void solve(vector<vector<char>>& board) {
        if (board.empty())
            return;

        int h = (int)board.size(), w = (int)board[0].size();

        // build disjointSet
        vector<int> disjointSet(h * w);
        for (int i = 0; i < h; ++i)
        {
            for (int j = 0; j < w; ++j)
            {
                int off = i * w + j;
                int set = off;
                if (i > 0 && board[i - 1][j] == board[i][j])
                {
                    set = disjointSet[off - w];
                }
                if (j > 0 && board[i][j - 1] == board[i][j])
                {
                    if (set == off)
                    {
                        set = disjointSet[off - 1];
                    }
                    else
                    {
                        join(disjointSet, off - w, off - 1);
                    }
                }
                disjointSet[off] = set;
            }
        }

        // mark lead
        for (int i = 0; i < h; ++i)
        {
            bool iBorder = i == 0 || i == h - 1;
            for (int j = 0; j < w; ++j)
            {
                int off = i * w + j;
                disjointSet[off] = lead(disjointSet, off);

                bool border = iBorder || j == 0 || j == w - 1;
                if (board[i][j] == 'X' || border)
                {
                    disjointSet[disjointSet[off]] = -1;
                }
            }
        }

        // flip
        for (int i = 0; i < h; ++i)
        {
            for (int j = 0; j < w; ++j)
            {
                int off = i * w + j;
                if (disjointSet[lead(disjointSet, off)] != -1)
                    board[i][j] = 'X';
            }
        }
    }

    int lead(vector<int> const &disjointSet, int i)
    {
        while (disjointSet[i] != -1 && i != disjointSet[i])
            i = disjointSet[i];
        return i;
    }

    void join(vector<int> &disjointSet, int i, int j)
    {
        int leadI = lead(disjointSet, i);
        int leadJ = lead(disjointSet, j);
        int coLead = min(leadI, leadJ);
        disjointSet[leadI] = coLead;
        disjointSet[leadJ] = coLead;
    }
};

int main(int argc, char *argv[])
{
    {
        vector<vector<char>> board
        {
            {'X', 'X', 'X', 'X',},
            {'X', 'O', 'O', 'X',},
            {'X', 'X', 'O', 'X',},
            {'X', 'O', 'X', 'X',},
        };
        Solution().solve(board);
        cout << "--------------\n";
        for (auto &row : board)
        {
            for (auto c : row)
                cout << c << ' ';
            cout << '\n';
        }
    }
    {
        vector<vector<char>> board
        {
            {'X', 'X', 'O'},
            {'X', 'O', 'O'},
            {'X', 'X', 'O'},
        };
        Solution().solve(board);
        cout << "--------------\n";
        for (auto &row : board)
        {
            for (auto c : row)
                cout << c << ' ';
            cout << '\n';
        }
    }
    {
        vector<vector<char>> board
        {
            {'O', 'X', 'O', 'O', 'O', 'O', 'O', 'O', 'O'},{'O', 'O', 'O', 'X', 'O', 'O', 'O', 'O', 'X'},{'O', 'X', 'O', 'X', 'O', 'O', 'O', 'O', 'X'},{'O', 'O', 'O', 'O', 'X', 'O', 'O', 'O', 'O'},{'X', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'X'},{'X', 'X', 'O', 'O', 'X', 'O', 'X', 'O', 'X'},{'O', 'O', 'O', 'X', 'O', 'O', 'O', 'O', 'O'},{'O', 'O', 'O', 'X', 'O', 'O', 'O', 'O', 'O'},{'O', 'O', 'O', 'O', 'O', 'X', 'X', 'O', 'O'}
        };
        Solution().solve(board);
        cout << "--------------\n";
        for (auto &row : board)
        {
            for (auto c : row)
                cout << c << ' ';
            cout << '\n';
        }
    }
}
