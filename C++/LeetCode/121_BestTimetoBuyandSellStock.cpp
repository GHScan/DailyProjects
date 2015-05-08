#include "stdafx.h"

//-------------------------------------------------------------------------
#include <limits>
#include <algorithm>

class Solution {
public:
    int maxProfit(vector<int> const& prices) {
        int result = 0;
        int minPrice = std::numeric_limits<int>::max();

        for (auto price : prices) {
            if (price > minPrice) {
                result = max(result, price - minPrice);
            } else {
                minPrice = price;
            }
        }

        return result;
    }
};
//-------------------------------------------------------------------------

int main() {
    Solution so;

    cout << so.maxProfit(vector<int>{}) << endl;
    cout << so.maxProfit(vector<int>{ 1 }) << endl;
    cout << so.maxProfit(vector<int>{ 1, 2 }) << endl;
    cout << so.maxProfit(vector<int>{ 2, 1 }) << endl;
    cout << so.maxProfit(vector<int>{ 1, 2, 3, 4 }) << endl;
    cout << so.maxProfit(vector<int>{ 1, 4, 3, 2 }) << endl;
    cout << so.maxProfit(vector<int>{ 4, 3, 2, 1 }) << endl;
    cout << so.maxProfit(vector<int>{ 4, 1, 2, 3 }) << endl;
}
