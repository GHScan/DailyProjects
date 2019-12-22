
#include <string>
#include <vector>
#include <queue>
#include <unordered_map>
#include <iterator>
#include <iostream>
#include <algorithm>

using namespace std;

class Solution {
public:
    int canCompleteCircuit(vector<int> const &gas, vector<int> const &cost) {
        int n = (int)gas.size();

        for (int start = 0; start < n; ++start)
        {
            int accumGas = 0;
            int accumCost = 0;

            int i = 0;
            for (; i < n; ++i)
            {
                accumGas += gas[(start + i) % n];
                accumCost += cost[(start + i) % n];
                if (accumGas < accumCost)
                    break;
            }

            if (i == n)
                return start;
        }

        return -1;
    }
};

int main()
{
    cout << Solution().canCompleteCircuit({ 1,2,3,4,5 }, { 3,4,5,1,2 }) << "\n";
    cout << Solution().canCompleteCircuit({ 2,3,4 }, { 3,4,3 }) << "\n";
}