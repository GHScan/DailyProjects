
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
    int maximumGap(vector<int> &nums) {
        int n = (int)nums.size();
        if (n <= 1) return 0;

        radixSort(nums.data(), n);

        int maxGap = numeric_limits<int>::min();
        for (int i = 0; i < n - 1; ++i)
        {
            maxGap = max(maxGap, nums[i + 1] - nums[i]);
        }

        return maxGap;
    }

    static void radixSort(int a[], int n)
    {
        vector<int> tmp(n);

        int *src = a, *dst = tmp.data();

        for (int turn = 0; turn < sizeof(int); ++turn)
        {
            int counts[256] = {0};

            int shift = turn * 8;
            for (int i = 0; i < n; ++i)
            {
                int idx = (src[i] >> shift) & 0xff;
                ++counts[idx];
            }

            int offs[256];
            offs[0] = 0;
            for (int i = 0; i < 256 - 1; ++i)
                offs[i + 1] = offs[i] + counts[i];

            for (int i = 0; i < n; ++i)
            {
                int idx = (src[i] >> shift) & 0xff;
                dst[offs[idx]++] = src[i];
            }

            swap(dst, src);
        }

        if (dst != tmp.data())
            memcpy(tmp.data(), dst, n * sizeof(int));
    }
};

int main()
{
    for (auto &test : vector<vector<int>>
        {
            {},
            {3},
            {3, 5},
            {9, 3, 5,},
            {3, 1, 7, 5},
            { 3, 6, 9, 1 },
        })
    {
        cout << Solution().maximumGap(test) << "\n";
    }
}