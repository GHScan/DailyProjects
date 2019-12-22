
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
    int candy(vector<int> const &ratings) {
        int totalCandy = 0;

        int i = 0;
        for (; i < (int)ratings.size();)
        {
            int ei = i + 1;
            while (ei < (int)ratings.size() && ratings[ei] != ratings[ei - 1])
                ++ei;

            totalCandy += assignCandy(ratings, i, ei - i);

            i = ei;
        }

        return totalCandy;
    }

    int assignCandy(vector<int> const &ratings, int off, int n)
    {
        if (n == 1)
            return 1;

        vector<bool> ascent(n - 1, false);
        for (int i = 0; i < n - 1; ++i)
        {
            if (ratings[off + i] < ratings[off + i + 1])
                ascent[i] = true;
        }

        vector<int> candies(n);
        queue<int> pendings;

        if (ascent.front())
        {
            candies.front() = 1;
            pendings.push(0);
        }
        if (!ascent.back())
        {
            candies.back() = 1;
            pendings.push(n - 1);
        }
        for (int i = 1; i < n - 1; ++i)
        {
            if (!ascent[i - 1] && ascent[i])
            {
                candies[i] = 1;
                pendings.push(i);
            }
        }

        while (!pendings.empty())
        {
            int i = pendings.front();
            pendings.pop();

            if (i - 1 >= 0 && !ascent[i - 1])
            {
                candies[i - 1] = max(candies[i - 1], candies[i] + 1);
                pendings.push(i - 1);
            }
            if (i + 1 < n && ascent[i])
            {
                candies[i + 1] = max(candies[i + 1], candies[i] + 1);
                pendings.push(i + 1);
            }
        }

        return accumulate(candies.begin(), candies.end(), 0);
    }
};

int main()
{
    cout << Solution().candy({ }) << "\n";
    cout << Solution().candy({ 1, 1 }) << "\n";
    cout << Solution().candy({ 1,0,2 }) << "\n";
    cout << Solution().candy({ 1,2,2 }) << "\n";
    cout << Solution().candy({ 3, 2, 1 }) << "\n";
    cout << Solution().candy({ 3, 2, 1, 1, -2, -1 }) << "\n";
    cout << Solution().candy({ 1, 3, 4, 5, 2 }) << "\n";
}