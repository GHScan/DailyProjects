
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
    string fractionToDecimal(int64_t numerator, int64_t denominator)
    {
        bool neg = (numerator < 0) ^ (denominator < 0);
        numerator = abs(numerator);
        denominator = abs(denominator);

        string result;
        if (numerator != 0 && neg)
            result.push_back('-');
        result.append(to_string(numerator / denominator));
        numerator = numerator % denominator;

        if (numerator == 0)
            return result;

        result.push_back('.');

        unordered_map<int64_t, size_t> map;

        do {
            map[numerator] = result.size();
            numerator *= 10;
            result.push_back(char(numerator / denominator + '0'));
            numerator = numerator % denominator;
        } while(numerator > 0 && map.count(numerator) == 0);

        if (numerator == 0)
            return result;

        result.insert(result.begin() + map[numerator], '(');
        result.push_back(')');
        return result;
    }
};

int main()
{
    for (auto &test : vector<pair<int64_t, int64_t>>
        {
            {0, -5},
            {0, 2},
            {1, 2},
            {2, 1},
            {2, 3},
            {3, 2},
            {1, 3},
            {1, 15},
            {1, 17},
            {1, 23},
            {1, 27},
            {2, 27},
            {2, -27},
            {-50, 8},
            {50, -8},
            {-50, -8},
            {-1,-2147483648}
        })
    {
        cout << Solution().fractionToDecimal(test.first, test.second) << "\n";
    }
}