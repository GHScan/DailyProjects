
#include <cstdio>

#include <string>
#include <vector>
#include <stack>
#include <list>
#include <unordered_map>
#include <iostream>
#include <iterator>
#include <algorithm>

using namespace std;

class Solution {
public:
    int titleToNumber(string s) {
        int n = 0;
        for (auto c : s)
            n = n * 26 + (c - 'A' + 1);
        return n;
    }
};

int main()
{
    for (auto &test : vector<string>
        {
            "A",
            "B",
            "Z",
            "AA",
            "AB",
            "ZY",
        })
    {
        cout << Solution().titleToNumber(test) << "\n";
    }
}