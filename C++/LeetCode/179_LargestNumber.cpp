#include <cstdio>

#include <list>
#include <stack>
#include <string>
#include <vector>
#include <numeric>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <unordered_map>

using namespace std;

class Solution {
public:
    string largestNumber(vector<int> const &nums_) {

        vector<string> nums(nums_.size());
        transform(
            nums_.begin(), nums_.end(), nums.begin(), 
            [](int i) { return to_string(i); });

        sort(nums.begin(), nums.end(),
            [](string const &a, string const &b) { return a + b > b + a; });

        string result = accumulate(
            nums.begin(), nums.end(), string(), 
            [](string const &a, string const &b) { return a + b; });
        if (!result.empty() && result.front() == '0')
            return "0";
        return result;
    }
};


int main()
{
    cout << Solution().largestNumber({  }) << "\n";
    cout << Solution().largestNumber({ 0 }) << "\n";
    cout << Solution().largestNumber({ 0, 0 }) << "\n";
    cout << Solution().largestNumber({ 0, 3, 0 }) << "\n";
    cout << Solution().largestNumber({ 3, 3, 331 }) << "\n";
    cout << Solution().largestNumber({ 10,2 }) << "\n";
    cout << Solution().largestNumber({ 3,30,34,5,9 }) << "\n";
}
