
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
    int rob(vector<int> const &nums) 
    {
        int with = 0, without = 0;
        for (int num : nums)
        {
            int newWith = without + num;
            int newWithout = max(with, without);
            with = newWith;
            without = newWithout;
        }
        return max(with, without);
    }
};

int main()
{
    for (auto &test : vector<vector<int>>
        {
            {1,2,3,1},
            {2,7,9,3,1},
            {1, 5, 2, 3, 5},
            {1, 5, 2, 3, 1, 10},
        })
    {
        cout << Solution().rob(test) << "\n";
    }
}