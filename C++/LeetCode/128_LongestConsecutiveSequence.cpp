#include <vector>
#include <unordered_set>
#include <algorithm>
#include <iostream>

using namespace std;

class Solution {
public:
    int longestConsecutive(vector<int> const& nums_) {

        unordered_set<int> nums(nums_.begin(), nums_.end());

        int maxLen = 0;
        while (!nums.empty())
        {
            int first = *nums.begin();
            nums.erase(nums.begin());
            int last = first;
            int len = 1;

            for (auto it = nums.find(first - 1); it != nums.end(); it = nums.find(first - 1))
            {
                nums.erase(it);
                --first;
                ++len;
            }
            for (auto it = nums.find(last + 1); it != nums.end(); it = nums.find(last + 1))
            {
                nums.erase(it);
                ++last;
                ++len;
            }

            maxLen = max(maxLen, len);
        }

        return maxLen;
    }
};

int main(int argc, char *argv[])
{
    cout << Solution().longestConsecutive({ -1 }) << "\n";
    cout << Solution().longestConsecutive({ 0, -3, -2 }) << "\n";
    cout << Solution().longestConsecutive({ 100, 4, 200, 1, 3, 2 }) << "\n";
    cout << Solution().longestConsecutive({ 100, 4, -2, -5, 200, -1, -3, 1, -4, 3, 2 }) << "\n";
}
