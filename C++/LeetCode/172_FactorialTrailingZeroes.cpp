
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
    int trailingZeroes(int n) {
        int count5 = 0;
        for (int64_t i = 5; i <= n; i *= 5)
            count5 += n / i;
        return count5;
    }
};

int main()
{
    for (auto &test : vector<int>
        {
            3,
            5,
            10,
            20,
            30,
            40,
            1808548329,
        })
    {
        cout << Solution().trailingZeroes(test) << "\n";
    }
}