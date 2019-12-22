
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
    vector<int> twoSum(vector<int> &nums, int target) {
        int n = (int)nums.size();

        for (int i = 0; i < n; ++i)
        {
            int num2 = target - nums[i];
            auto it = std::lower_bound(nums.begin() + i + 1, nums.end(), num2);
            if (it != nums.end() && *it == num2)
                return {i + 1, int(it - nums.begin()) + 1, };
        }

        return {};
    }
};

int main()
{
    for (auto &test : vector<pair<vector<int>, int>>
        {
            {{2,}, 3},
            {{2,}, 2},
            {{1, 2}, 3},
            {{1, 2, 5}, 3},
            {{-5, 2, 3}, -2},
            {{2,7,11,15}, 9},
            {{2,7,11,15}, 18},
        })
    {
        auto result = Solution().twoSum(test.first, test.second);
        copy(result.begin(), result.end(), ostream_iterator<int>(cout, ","));
        cout << "\n";
    }
}