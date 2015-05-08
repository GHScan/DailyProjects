#include "stdafx.h"

//-------------------------------------------------------------------------
#include <algorithm>

class Solution {
public:
    int maxProfit(vector<int> const & prices) {
        int profit = 0;
        for (int i = 1; i < (int)prices.size(); ++i) {
            profit += max(prices[i] - prices[i - 1], 0);
        }
        return profit;
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
    cout << so.maxProfit(vector<int>{ 1, 2, 1, 2, 1, 2 }) << endl;
    cout << so.maxProfit(vector<int>{ 0, 1, 2, 3, 0, 5 }) << endl;

    {
        vector<int> prices;
        for (int i = 10000; i >= 0; --i) prices.push_back(i);
        for (int i = 10000; i >= 0; --i) prices.push_back(0);
        cout << so.maxProfit(prices) << endl;
    }
}
