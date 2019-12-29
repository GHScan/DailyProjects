
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
    uint32_t reverseBits(uint32_t n) {
        uint32_t ret = 0;
        for (int32_t i = 0; i < 32; ++i)
        {
            ret = (ret << 1) | (n >> i) & 1;
        }
        return ret;
    }
};

int main()
{
    for (auto &test : vector<int32_t>
        {
            43261596,
            int32_t(4294967293),
            -3,
        })
    {
        cout << (int32_t)Solution().reverseBits(test) << "\n";
    }
}