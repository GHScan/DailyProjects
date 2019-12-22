
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
    int maxProduct(vector<int> const &nums) {
        int n = (int)nums.size();

        int maxPos = max(nums[0], 0);
        int maxNeg = min(nums[0], 0);
        int maxProd = maxPos;
        for (int i = 1; i < n; ++i)
        {
            int num = nums[i];
            if (num > 0)
            {
                maxPos = max(num, maxPos * num);
                maxNeg = maxNeg * num;
            }
            else if (num < 0)
            {
                int newPos = maxNeg * num;
                int newNeg = min(maxPos * num, num);
                maxPos = newPos;
                maxNeg = newNeg;
            }
            else
            {
                maxPos = maxNeg = 0;
            }
            maxProd = max(maxProd, maxPos);
        }
        if (maxProd == 0)
        {
            maxProd = *max_element(nums.begin(), nums.end());
        }
        return maxProd;
    }
};

int main()
{
    for (auto &test : vector<vector<int>>
        {
            {-2},
            {2,3,-2,4},
            {2,-1,1,1 },
            {-2, -1},
            {-2, 0, -1},
            {0,12,-2,3,6,9,-1,20,10},
            {-3, -5, -7, 0, -10, -13},
            {-3, -5, -7, -10, -13},
        })
    {
        cout << Solution().maxProduct(test) << "\n";
    }
}