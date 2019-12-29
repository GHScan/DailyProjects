
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
    int maxProfit(int k, vector<int> &prices) {
        int len = (int)prices.size();
        if (len <= 1) return 0;

        vector<pair<int, int>> growLines;
        for (int buy = 0;;)
        {
            while (buy < len && !isBuyPoint(buy, prices.data(), len))
                ++buy;
            if (buy == len) break;

            int sell = buy + 1;
            while (sell < len && !isSellPoint(sell, prices.data(), len))
                ++sell;
            if (sell == len) break;

            growLines.push_back(make_pair(buy, sell));
            buy = sell + 1;
        }

        int n = (int)growLines.size();
        if (n == 0) return 0;
        if (k >= n)
        {
            return accumulate(
                growLines.begin(), growLines.end(), 0,
                [&](int sum, pair<int, int> const &p)
                { return sum + prices[p.second] - prices[p.first]; });
        }
        if (k <= 0) return 0;

        vector<int> maxProfits(n * k);

        for (int n0 = 0; n0 < n; ++n0)
        {
            for (int k0 = 0; k0 < k; ++k0)
            {
                int maxProfit = n0 > 0 ? maxProfits[(n0 - 1) * k + k0] : 0;

                int sell = prices[growLines[n0].second];
                int lastBuy = sell;
                for (int i = n0;;)
                {
                    for (; i >= 0 && prices[growLines[i].first] >= lastBuy;)
                        --i;
                    if (i < 0) break;
                    lastBuy = prices[growLines[i].first];

                    int profit = sell - lastBuy;
                    if (i > 0 && k0 > 0)
                        profit += maxProfits[(i - 1) * k + k0 - 1];
                    maxProfit = max(maxProfit, profit);

                    --i;
                }

                maxProfits[n0 * k + k0] = maxProfit;
            }
        }

        return maxProfits.back();
    }

    static bool isBuyPoint(int i, int *prices, int len)
    {
        if (i == 0) return prices[i + 1] > prices[i];
        if (i == len - 1) return false;
        int left = prices[i - 1] - prices[i];
        int right = prices[i + 1] - prices[i];
        return left >= 0 && right >= 0 && !(left == 0 && right == 0);
    }

    static bool isSellPoint(int i, int *prices, int len)
    {
        if (i == 0) return false;
        if (i == len - 1) return prices[i] > prices[i - 1];
        int left = prices[i - 1] - prices[i];
        int right = prices[i + 1] - prices[i];
        return left <= 0 && right <= 0 && !(left == 0 && right == 0);
    }
};

int main()
{
    for (auto &test : vector<pair<int, vector<int>>>
        {
            {1, {}},
            {1, {1}},
            {0, {3}},
            {1, {3}},
            {1, {5, 3}},
            {1, {3, 5}},
            {1, {3, 1, 5}},
            {2, {2, 4, 1}},
            {2, {3,2,6,5,0,3}},
            {1, {1, 3, 2, 1, 3, 2, 1, 3, 2}},
            {2, {1, 3, 2, 1, 3, 2, 1, 3, 2}},
            {3, {1, 3, 2, 1, 3, 2, 1, 3, 2}},
        })
    {
        cout << Solution().maxProfit(test.first, test.second) << "\n";
    }
}