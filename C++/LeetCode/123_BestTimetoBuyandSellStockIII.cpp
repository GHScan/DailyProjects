#include "stdafx.h"

//-------------------------------------------------------------------------
#include <algorithm>

class Solution {
public:
    int maxProfit(vector<int> const& prices) {
        if (prices.empty()) return 0;

        vector<int> leftProfits(prices.size(), 0);
        {
            int minPrice = prices[0];
            for (int i = 1; i < (int)prices.size(); ++i) {
                if (prices[i] > minPrice) {
                    leftProfits[i] = max(leftProfits[i - 1], prices[i] - minPrice);
                } else {
                    minPrice = prices[i];
                    leftProfits[i] = leftProfits[i - 1];
                }
            }
        }

        int result = 0;

        int maxPrice = prices.back();
        int rightProfit = 0;
        for (int i = prices.size() - 2; i >= 0; --i) {
            if (prices[i] < maxPrice) {
                rightProfit = max(rightProfit, maxPrice - prices[i]);
            } else {
                maxPrice = prices[i];
            }

            result = max(result, (i - 1 >= 0 ? leftProfits[i - 1] : 0) + rightProfit);
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
    cout << so.maxProfit(vector<int>{ 1, 2, 1, 2, 1, 2 }) << endl;
    cout << so.maxProfit(vector<int>{ 0, 1, 2, 3, 0, 5 }) << endl;
    cout << so.maxProfit(vector<int>{ 1, 3, 1, 2, 1, 3 }) << endl;
    cout << so.maxProfit(vector<int>{ 1, 2, 1, 3, 1, 3 }) << endl;

    {
        vector<int> prices;
        for (int i = 10000; i >= 0; --i) prices.push_back(i);
        for (int i = 10000; i >= 0; --i) prices.push_back(0);
        cout << so.maxProfit(prices) << endl;
    }
}
