
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
        if (nums[first] < nums[last])
            return nums[first];
        else if (nums[first] == nums[last])
        {
            while (first + 1 < last && nums[first + 1] == nums[last])
                ++first;
            if (++first > last)
                return nums[last];
            if (nums[first] <= nums[last])
                return nums[first];
        }

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
            {1, 1},
            {1, 1, 1},
            {1, 2},
            {2, 1},
            {2, 1, 1, 2},
            {2, 1, 1, 1, 2},
            {1, 2, 3},
            {3, 1, 2,},
            { 10,1,10,10,10 },
            {2,2,2,0,1},
            {3,4,5,1,2},
            {4,5,6,7,0,1,2},
        })
    {
        cout << Solution().findMin(test) << "\n";
    }
}