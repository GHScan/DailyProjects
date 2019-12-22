#include <string>
#include <vector>
#include <queue>
#include <numeric>
#include <unordered_map>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    int singleNumber(vector<int> &nums) {
        int result = 0;
        for (int num : nums)
            result ^= num;
        return result;
    }
};

int main()
{
}