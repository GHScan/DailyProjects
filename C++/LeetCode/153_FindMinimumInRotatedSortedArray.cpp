
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
    int findMin(vector<int> const &nums) {
        int first = 0, last = (int)nums.size() - 1;
        if (nums[first] <= nums[last])
            return nums[first];

        while (last - first > 1)
        {
            int mid = first + (last - first) / 2;
            if (nums[mid] >= nums[first])
                first = mid;
            else
                last = mid;
        }
        return nums[last];
    }
};

int main()
{
    for (auto &test : vector<vector<int>>
        {
            {1},
            {1, 2},
            {2, 1},
            {1, 2, 3},
            {3, 1, 2,},
            {3,4,5,1,2},
            {4,5,6,7,0,1,2},
        })
    {
        cout << Solution().findMin(test) << "\n";
    }
}