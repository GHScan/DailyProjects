
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
    int hammingWeight(uint32_t n) {
        int count = 0;
        while (n > 0)
        {
            n &= n - 1;
            ++count;
        }
        return count;
    }
};

int main()
{
}