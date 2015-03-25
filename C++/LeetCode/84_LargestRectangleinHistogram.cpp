#include "stdafx.h"

#include <assert.h>

//-------------------------------------------------------------------------
#include <algorithm>

class Solution {
public:
    int largestRectangleArea(vector<int> &height) {
        height.push_back(0);

        int result = 0;
        vector<int> stack;
        for (int i = 0; i < (int)height.size(); ++i) {
            while (!stack.empty() && height[stack.back()] >= height[i]) {
                int h = height[stack.back()];
                stack.pop_back();
                result = max(result, h * (stack.empty() ? i : (i - stack.back() - 1)));
            }
            stack.push_back(i);
        }
        return result;
    }
};
//-------------------------------------------------------------------------

int main() {
    Solution so;

    cout << so.largestRectangleArea(vector<int>{}) << endl;
    cout << so.largestRectangleArea(vector<int>{1}) << endl;
    cout << so.largestRectangleArea(vector<int>{1, 2}) << endl;
    cout << so.largestRectangleArea(vector<int>{2, 1}) << endl;
    cout << so.largestRectangleArea(vector<int>{2, 10, 2, 1}) << endl;
    cout << so.largestRectangleArea(vector<int>{2, 2, 10, 1}) << endl;
    cout << so.largestRectangleArea(vector<int>{3, 2, 10, 3}) << endl;
    cout << so.largestRectangleArea(vector<int>{3, 2, 10, 3, 2, 2}) << endl;
    cout << so.largestRectangleArea(vector<int>{2, 1, 5, 6, 2, 3, }) << endl;
    cout << so.largestRectangleArea(vector<int>{2, 1, 6, 5, 2, 3, }) << endl;
    cout << so.largestRectangleArea(vector<int>{11, 11, 6, 11, 11, }) << endl;
}
