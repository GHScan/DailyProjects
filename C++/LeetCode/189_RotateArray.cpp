
#include <cstdio>
#include <cassert>

#include <list>
#include <stack>
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
    void rotate(vector<int> &nums, int k) {
        int start = 0, len = (int)nums.size();
        if (len == 0) return;
        k %= len;
        while (k < len && k > 0)
        {
            if (k * 2 > len)
            {
                int n = len - k;
                swap(
                    nums.data() + start,
                    nums.data() + start + n, n);
                start += n;
                k -= n;
                len -= n;
            }
            else
            {
                swap(
                    nums.data() + start, 
                    nums.data() + start + len - k, k);
                start += k;
                len -= k;
            }
        }
    }

    void swap(int *nums0, int *nums1, int len)
    {
        for (int i = 0; i < len; ++i)
            std::swap(nums0[i], nums1[i]);
    }
};

int main()
{
    for (auto &test : vector<pair<int, int>>
        {
            {0, 0},
            {1, 1},
            {2, 1},
            {2, 3},
            {3, 1},
            {3, 2},
            {4, 1},
            {4, 2},
            {4, 3},
            {5, 1},
            {5, 2},
            {5, 3},
            {5, 4},
            {5, 5},
        })
    {
        vector<int> nums(test.first);
        iota(nums.begin(), nums.end(), 0);
        Solution().rotate(nums, test.second);
        copy(nums.begin(), nums.end(), ostream_iterator<int>(cout, ","));
        cout << "\n";
    }
}