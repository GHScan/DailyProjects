
#include <cstdio>
#include <cassert>

#include <list>
#include <stack>
#include <queue>
#include <string>
#include <vector>
#include <numeric>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <unordered_set>
#include <unordered_map>

using namespace std;

class Solution {
public:
    int numIslands(vector<vector<char>> const &grid) {
        int m = (int)grid.size();
        if (m == 0) return 0;
        int n = (int)grid.front().size();
        if (n == 0) return 0;

        vector<int> ids(m * n, -1);
        for (int i = 0; i < m; ++i)
        {
            for (int j = 0; j < n; ++j)
            {
                if (grid[i][j] == '0') continue;

                int idx = i * n + j;
                int id = idx;
                if (i > 0 && grid[i][j] == grid[i - 1][j])
                    id = ids[(i - 1) * n + j];
                if (j > 0 && grid[i][j] == grid[i][j - 1])
                {
                    int idLeft = ids[i * n + j - 1];
                    if (id == idx)
                        id = idLeft;
                    else
                    {
                        if (id != idLeft)
                            id = join(ids, id, idLeft);
                    }
                }
                ids[idx] = id;
            }
        }

        int count = 0;
        for (int i = 0; i < m * n; ++i)
            count += ids[i] == i;
        return count;
    }

    int join(vector<int> &ids, int id0, int id1)
    {
        for (; id0 != ids[id0]; id0 = ids[id0]);
        for (; id1 != ids[id1]; id1 = ids[id1]);
        int minId = min(id0, id1);
        return ids[id0] = ids[id1] = minId;
    }
};

int main()
{
    for (auto &test : vector<vector<vector<char>>>
        {
            {
                {'1','1','1','1','0',},
                {'1','1','0','1','0',},
                {'1','1','0','0','0',},
                {'0','0','0','0','0',},
            },
            {
                {'1','1','0','0','0',},
                {'1','1','0','0','0',},
                {'0','0','1','0','0',},
                {'0','0','0','1','1',},
            },
        })
    {
        cout << Solution().numIslands(test) << "\n";
    }
}