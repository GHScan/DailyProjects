
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
    int majorityElement(vector<int> &nums) 
    {
        int n = (int)nums.size();

        if (n < 3)
            return nums[0];

        if (n % 2 == 0)
            --n;

        nth_element(nums.data(), n, n / 2);

        return nums[n / 2];
    }

    void nth_element(int a[], int n, int idx)
    {
        for (;;)
        {
            if (idx == 0)
                break;

            int v = a[0];
            int p = 0;
            for (int i = 1; i < n; ++i)
            {
                if (a[i] <= v)
                    swap(a[i], a[++p]);
            }
            swap(a[p], a[0]);

            while (p > idx && a[p] == a[p - 1])
                --p;
            while (p < idx && a[p] == a[p + 1])
                ++p;

            if (p < idx)
            {
                a += p + 1;
                n -= p + 1;
                idx -= p + 1;
            }
            else if (p > idx)
            {
                n = p;
            }
            else
            {
                break;
            }
        }
    }
};

int main()
{
    for (auto &test : vector<vector<int>>
        {
            {1, },
            {1, 1},
            {1, 1, 2},
            {1, 2, 1},
            {2, 1, 1},
            {2, 1, 2, 2},
            {2, 1, 1, 1, 2},
        })
    {
        cout << Solution().majorityElement(test) << "\n";
    }
}