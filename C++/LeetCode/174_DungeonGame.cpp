
#include <cstdio>

#include <string>
#include <vector>
#include <stack>
#include <list>
#include <unordered_map>
#include <iostream>
#include <iterator>
#include <algorithm>

using namespace std;

class Solution {
public:
    int calculateMinimumHP(vector<vector<int>> &dungeon) {
        if (dungeon.empty()) return 0;

        int m = (int)dungeon.size(), n = (int)dungeon[0].size();

        vector<int> accum0(n), accum1(n);

        accum0.back() = max(1 - dungeon.back().back(), 1);
        for (int j = n - 2; j >= 0; --j)
            accum0[j] = max(accum0[j + 1] - dungeon.back()[j], 1);

        for (int i = m - 2; i >= 0; --i)
        {
            accum1.back() = max(accum0.back() - dungeon[i].back(), 1);
            for (int j = n - 2; j >= 0; --j)
                accum1[j] = max(min(accum1[j + 1], accum0[j]) - dungeon[i][j], 1);

            swap(accum0, accum1);
        }

        return accum0.front();
    }
};


int main()
{
    for (auto &test : vector<vector<vector<int>>>
        {
            {{-3}},
            { {2} ,{1} },
            {{-2,1},{-3,-3}},
            {{-2, -3, 3}, {-5, -10 , 1}, {10, 30, -5}}
        })
    {
        cout << Solution().calculateMinimumHP(test) << "\n";
    }
}