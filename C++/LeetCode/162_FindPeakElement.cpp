
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
    int findPeakElement(vector<int> const &nums) {
        int n = (int)nums.size();
        if (n <= 1) return 0;
        int first = 0, last = n - 1;
        if (nums[first] > nums[first + 1]) return first;
        if (nums[last - 1] < nums[last]) return last;

        while (last - first > 2)
        {
            int mid = first + (last - first) / 2;
            bool inc1 = nums[mid - 1] < nums[mid];
            bool inc2 = nums[mid] < nums[mid + 1];
            if (inc1 && inc2)
                first = mid;
            else if (!inc1 && !inc2)
                last = mid;
            else if (inc1 && !inc2)
                return mid;
            else 
                last = mid - 1;
        }

        return first + 1;
    }
};

int main()
{
    cout << Solution().findPeakElement({ }) << "\n";
    cout << Solution().findPeakElement({ 3 }) << "\n";
    cout << Solution().findPeakElement({ 3, 5 }) << "\n";
    cout << Solution().findPeakElement({ 5, 3 }) << "\n";
    cout << Solution().findPeakElement({ 1,2,3,1 }) << "\n";
    cout << Solution().findPeakElement({ 1,2,1,3,5,6,4 }) << "\n";
}