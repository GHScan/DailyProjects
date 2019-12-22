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
    int singleNumber(vector<int> const &nums) {
        int result = 0;

        for (int i = 0; i < sizeof(int) * 8; ++i)
        {
            int sum = 0;
            for (int num : nums)
                sum += (num >> i) & 1;
            result |= (sum % 3) << i;
        }

        return result;
    }
};

int main()
{
    cout << Solution().singleNumber({ 2,2,3,2 }) << "\n";
    cout << Solution().singleNumber({ 0,1,0,1,0,1,99 }) << "\n";
}