
#include <cstdio>

#include <string>
#include <vector>
#include <stack>
#include <list>
#include <unordered_map>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    int maxPoints(vector<vector<int>> const &points_) {
        int n = (int)points_.size();
        vector<pair<int, int>> points(n);
        for (int i = 0; i < n; ++i)
            points[i] = make_pair(points_[i][0], points_[i][1]);

        int maxCount = n > 0 ? 1 : 0;
        for (int i = 0; i < n; ++i)
        {
            for (int j = 0; j < i; ++j)
            {
                int64_t x1 = points[i].first, y1 = points[i].second;
                int64_t x2 = points[j].first, y2 = points[j].second;

                int count = 2;
                for (int k = 0; k < n; ++k)
                {
                    if (k == i || k == j) continue;
                    int64_t x3 = points[k].first, y3 = points[k].second;

                    if (x2 == x1)
                    {
                        count += x3 == x1;
                    }
                    else
                    {
                        if ((x3 - x1) * (y2 - y1) == (x2 - x1) * (y3 - y1))
                            ++count;
                    }
                }
                maxCount = max(count, maxCount);
            }
        }

        return maxCount;
    }
};

int main()
{
    for (auto &test : vector<vector<vector<int>>>
        {
            {},
            {{1,1},},
            { {1,1},{2,2}},
            { {0,0},{2,2}},
            { {1,1} ,{2,2},{3,3}},
            {{1,1},{3,2},{5,3},{4,1},{2,3},{1,4}},
            {{84,250},{0,0},{1,0},{0,-70},{0,-70},{1,-1},{21,10},{42,90},{-42,-230}},
        })
    {
        cout << Solution().maxPoints(test) << "\n";
    }
}